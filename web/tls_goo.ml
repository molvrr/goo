(* A TLS client session over a non-blocking socket, driven by [Tls.Engine].

   The read task and the write task of an httpun connection share one
   session. Every step that advances the TLS state and produces bytes for
   the wire runs under [lock] together with writing those bytes, so records
   go out in the order they were sealed. Reads from the socket happen outside
   the lock: only the read task (or the handshake, before it starts) reads. *)

module R = Goo

type t = {
  fd : Unix.file_descr;
  lock : R.Mutex.t;
  mutable state : Tls.Engine.state;
  mutable pending : string; (* decrypted, not yet handed out *)
  mutable pending_off : int;
  mutable eof : bool; (* close_notify, socket EOF or a fatal error *)
  mutable send_closed : bool; (* close_notify sent *)
  mutable send_done : bool; (* the protocol finished writing *)
  mutable receive_done : bool; (* the protocol finished reading *)
  raw : bytes;
}

let fail fmt = Printf.ksprintf failwith ("tls: " ^^ fmt)

let shutdown fd cmd =
  try Unix.shutdown fd cmd with Unix.Unix_error ((ENOTCONN | EBADF), _, _) -> ()

(* Certificate errors print the whole certificate; keep the gist. *)
let describe_failure (f : Tls.Engine.failure) =
  let name dn = Fmt.to_to_string X509.Distinguished_name.pp dn in
  let subject c = name (X509.Certificate.subject c) in
  match f with
  | `Error (`AuthenticationFailure e) -> (
      "certificate rejected: "
      ^
      match e with
      | `LeafCertificateExpired (c, _) -> "expired (" ^ subject c ^ ")"
      | `LeafInvalidName (c, _) -> "not valid for this host name (" ^ subject c ^ ")"
      | `LeafInvalidIP (c, _) -> "not valid for this IP address (" ^ subject c ^ ")"
      | `LeafInvalidVersion c | `LeafInvalidExtensions c -> "invalid leaf certificate (" ^ subject c ^ ")"
      | `InvalidFingerprint (c, _, _) -> "fingerprint mismatch (" ^ subject c ^ ")"
      | `InvalidChain -> "no chain to a trusted root"
      | `EmptyCertificateChain -> "no certificate sent"
      | #X509.Validation.signature_error as e -> Fmt.to_to_string X509.Validation.pp_signature_error e)
  | f -> Tls.Engine.string_of_failure f

(* Feed bytes from the socket to the engine; queue any application data. *)
let feed t raw =
  R.Mutex.use t.lock (fun () ->
      match Tls.Engine.handle_tls t.state raw with
      | Ok (state, eof, `Response resp, `Data data) ->
          t.state <- state;
          Option.iter (R.write_string t.fd) resp;
          if Option.is_some eof then t.eof <- true;
          Option.iter
            (fun d ->
              let rest = String.sub t.pending t.pending_off (String.length t.pending - t.pending_off) in
              t.pending <- rest ^ d;
              t.pending_off <- 0)
            data
      | Error (failure, `Response resp) ->
          t.eof <- true;
          (try R.write_string t.fd resp with Unix.Unix_error _ -> ());
          fail "%s" (describe_failure failure))

(* One read from the socket; false at EOF. *)
let pull t =
  match R.read t.fd t.raw 0 (Bytes.length t.raw) with
  | 0 -> false
  | n ->
      feed t (Bytes.sub_string t.raw 0 n);
      true
  | exception Unix.Unix_error ((ECONNRESET | ENOTCONN), _, _) -> false

let available t = String.length t.pending - t.pending_off

let client config fd =
  let state, hello = Tls.Engine.client config in
  let t =
    {
      fd;
      lock = R.Mutex.create ();
      state;
      pending = "";
      pending_off = 0;
      eof = false;
      send_closed = false;
      send_done = false;
      receive_done = false;
      raw = Bytes.create 0x4000;
    }
  in
  R.write_string fd hello;
  while Tls.Engine.handshake_in_progress t.state do
    if t.eof || not (pull t) then fail "connection closed during the handshake"
  done;
  t

let rec read t buf off len =
  if available t > 0 then begin
    let n = min len (available t) in
    Bigstringaf.blit_from_string t.pending ~src_off:t.pending_off buf ~dst_off:off ~len:n;
    t.pending_off <- t.pending_off + n;
    n
  end
  else if t.eof then 0
  else if pull t then read t buf off len
  else begin
    (* Socket EOF without close_notify: treat as EOF. HTTP framing
       (content-length / chunked) catches a truncated body. *)
    t.eof <- true;
    0
  end

let write t buf off len =
  R.Mutex.use t.lock (fun () ->
      if t.send_closed then raise End_of_file;
      match Tls.Engine.send_application_data t.state [ Bigstringaf.substring buf ~off ~len ] with
      | Some (state, out) ->
          t.state <- state;
          R.write_string t.fd out;
          len
      | None -> raise End_of_file)

let rec watch_eof t =
  if available t > 0 then `Data
  else if t.eof then `Eof
  else begin
    R.await_readable t.fd;
    match Unix.recv t.fd (Bytes.create 1) 0 1 [ Unix.MSG_PEEK ] with
    | 0 -> `Eof
    | _ -> `Data
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) -> watch_eof t
    | exception Unix.Unix_error ((ECONNRESET | ENOTCONN), _, _) -> `Eof
  end

(* close_notify ends the session for both sides, and servers tend to hang
   up on receiving it, so don't half-close: an HTTP/1.1 client that sent
   "connection: close" finishes writing before the response arrives. Send it
   once the protocol is done in both directions. *)
let close_notify t =
  R.Mutex.use t.lock (fun () ->
      if not t.send_closed then begin
        t.send_closed <- true;
        let state, out = Tls.Engine.send_close_notify t.state in
        t.state <- state;
        (try R.write_string t.fd out with Unix.Unix_error _ -> ());
        shutdown t.fd Unix.SHUTDOWN_SEND
      end)

let shutdown_send t =
  t.send_done <- true;
  if t.receive_done then close_notify t

let shutdown_receive t =
  t.receive_done <- true;
  shutdown t.fd Unix.SHUTDOWN_RECEIVE;
  if t.send_done then close_notify t

let transport t : HttpunGoo.transport =
  {
    read = read t;
    write = write t;
    watch_eof = (fun () -> watch_eof t);
    shutdown_receive = (fun () -> shutdown_receive t);
    shutdown_send = (fun () -> shutdown_send t);
  }

(* mirage-crypto needs a seeded RNG; [use_default] reads the OS (getrandom /
   getentropy) on demand, so it is safe from any domain. Setting it twice is
   harmless. *)
let rng_ready = Atomic.make false

let ensure_rng () =
  if not (Atomic.get rng_ready) then begin
    Mirage_crypto_rng_unix.use_default ();
    Atomic.set rng_ready true
  end

(* Parsing the NSS bundle takes a moment; do it once. Racing domains may
   both build it, which is harmless. *)
let default = Atomic.make None

let default_authenticator () =
  match Atomic.get default with
  | Some a -> a
  | None -> (
      match Ca_certs_nss.authenticator () with
      | Ok a ->
          Atomic.set default (Some a);
          a
      | Error (`Msg m) -> fail "CA bundle: %s" m)

let config ?authenticator host =
  ensure_rng ();
  let authenticator = match authenticator with Some a -> a | None -> default_authenticator () in
  let result =
    match Ipaddr.of_string host with
    | Ok ip -> Tls.Config.client ~authenticator ~ip ()
    | Error _ -> (
        match Result.bind (Domain_name.of_string host) Domain_name.host with
        | Ok peer_name -> Tls.Config.client ~authenticator ~peer_name ()
        | Error (`Msg m) -> fail "bad host name %S: %s" host m)
  in
  match result with Ok c -> c | Error (`Msg m) -> fail "%s" m
