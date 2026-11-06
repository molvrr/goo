module Ws = Httpun_ws

type message = [ `Text of string | `Binary of string ]

type t = {
  wsd : Ws.Wsd.t;
  inbox : message option Goo.Chan.t;  (* [None] marks the end *)
  mutable ended : bool;  (* [None] was pushed *)
  mutable finished : bool;  (* [None] was received *)
}

let push t m = Goo.protect (fun () -> Goo.Chan.send t.inbox m)

let finish t =
  if not t.ended then begin
    t.ended <- true;
    push t None
  end

(* [`Other n] serializes [n] as is, so any code can be sent (unpatched
   httpun-ws 0.2 mis-maps [Close_code.of_int]; see vendor/README.md). *)
let close ?(code = 1000) t =
  if not (Ws.Wsd.is_closed t.wsd) then Ws.Wsd.close ~code:(`Other code) t.wsd

let is_closed t = t.ended || Ws.Wsd.is_closed t.wsd

let recv t =
  if t.finished then None
  else
    match Goo.Chan.recv t.inbox with
    | Some _ as m -> m
    | None ->
        t.finished <- true;
        None

let send t (m : message) =
  if is_closed t then raise Goo.Closed;
  let kind, s = match m with `Text s -> (`Text, s) | `Binary s -> (`Binary, s) in
  Ws.Wsd.send_bytes t.wsd ~kind (Bytes.of_string s) ~off:0 ~len:(String.length s)

let ping t = if not (is_closed t) then Ws.Wsd.send_ping t.wsd

(* Read a frame's payload fully (it must always be drained, or the reader
   stalls), giving up past [limit] bytes. *)
let read_payload payload ~limit k =
  let b = Buffer.create 256 in
  let too_big = ref false in
  let rec next () =
    Ws.Payload.schedule_read payload
      ~on_eof:(fun () -> k (if !too_big then None else Some (Buffer.contents b)))
      ~on_read:(fun bs ~off ~len ->
        if Buffer.length b + len > limit then too_big := true
        else Buffer.add_string b (Bigstringaf.substring bs ~off ~len);
        next ())
  in
  next ()

(* Runs in the connection's read task: frames arrive one at a time, the next
   only once this one's payload has been read. *)
let input_handlers ~max_message t =
  let fragments = Buffer.create 256 and kind = ref `Text in
  let frame ~opcode ~is_fin ~len:_ payload =
    match opcode with
    | (`Text | `Binary | `Continuation) as op ->
        if op <> `Continuation then begin
          Buffer.clear fragments;
          kind := (if op = `Text then `Text else `Binary)
        end;
        read_payload payload ~limit:(max_message - Buffer.length fragments) (function
          | None ->
              close ~code:1009 t;
              finish t
          | Some data ->
              Buffer.add_string fragments data;
              if is_fin then begin
                let s = Buffer.contents fragments in
                Buffer.clear fragments;
                push t (Some (if !kind = `Text then `Text s else `Binary s))
              end)
    | `Ping ->
        read_payload payload ~limit:125 (fun data ->
            let data = Option.value ~default:"" data in
            let buffer = Bigstringaf.of_string ~off:0 ~len:(String.length data) data in
            if not (Ws.Wsd.is_closed t.wsd) then
              Ws.Wsd.send_pong ~application_data:{ Httpun.IOVec.buffer; off = 0; len = String.length data } t.wsd)
    | `Pong -> read_payload payload ~limit:125 ignore
    | `Connection_close ->
        read_payload payload ~limit:125 (fun data ->
            (* Echo the peer's code, as the closing handshake requires. *)
            let code =
              match data with
              | Some s when String.length s >= 2 -> String.get_uint16_be s 0
              | _ -> 1000
            in
            close ~code:(if code >= 1000 && code < 5000 then code else 1002) t;
            finish t)
    | `Other _ ->
        read_payload payload ~limit:0 ignore;
        close ~code:1002 t;
        finish t
  in
  (* Peer gone: also close our side, so the write loop ends and the session
     is cancelled if it's blocked somewhere other than [recv]. *)
  let eof ?error:_ () =
    if not (Ws.Wsd.is_closed t.wsd) then Ws.Wsd.close t.wsd;
    finish t
  in
  { Ws.Websocket_connection.frame; eof }

let sha1 s = Digestif.SHA1.(to_raw_string (digest_string s))

let make wsd = { wsd; inbox = Goo.Chan.create (); ended = false; finished = false }

let connect ?(headers = []) ?(max_message = 1 lsl 20) url session =
  let ep = Codec.endpoint ~schemes:[ "ws"; "http" ] url in
  let fd = Codec.connect ep.addr in
  Fun.protect
    ~finally:(fun () -> Goo.protect (fun () -> Goo.close fd))
    (fun () ->
      let ready = Goo.Chan.create () in
      let signal v = Goo.protect (fun () -> Goo.Chan.send ready v) in
      (* Raw bytes: httpun-ws base64-encodes the nonce itself. *)
      let nonce = String.init 16 (fun _ -> Char.chr (Random.int 256)) in
      let conn =
        Ws.Client_connection.connect ~nonce
          ~headers:(Httpun.Headers.of_list (("host", ep.host) :: headers))
          ~sha1
          ~error_handler:(function
            | `Handshake_failure (resp, _) ->
                signal (Error (Printf.sprintf "handshake failed: %d" (Httpun.Status.to_code resp.Httpun.Response.status)))
            | `Malformed_response m -> signal (Error ("malformed response: " ^ m))
            | `Invalid_response_body_length _ -> signal (Error "invalid response")
            | `Exn e -> signal (Error (Printexc.to_string e)))
          ~websocket_handler:(fun wsd ->
            let t = make wsd in
            signal (Ok t);
            input_handlers ~max_message t)
          ep.target
      in
      let io =
        Goo.scope
          [
            (fun () ->
              Fun.protect
                ~finally:(fun () -> signal (Error "connection closed during handshake"))
                (fun () ->
                  HttpunGoo.run_protocol (module Ws.Client_connection) ~read_buffer_size:0x1000 conn fd));
          ]
      in
      Fun.protect
        ~finally:(fun () ->
          (* The server should close after the closing handshake; don't wait
             for it forever. *)
          Goo.protect (fun () ->
              let grace = Goo.scope [ (fun () -> Goo.sleep 2.; Goo.cancel io) ] in
              (try Goo.join io with _ -> ());
              Goo.cancel grace;
              try Goo.join grace with _ -> ()))
        (fun () ->
          match Goo.Chan.recv ready with
          | Error msg -> failwith ("Websocket.connect: " ^ msg)
          | Ok t -> (
              match session t with
              | v ->
                  close t;
                  v
              | exception e ->
                  close ~code:(if e = Goo.Cancelled then 1001 else 1011) t;
                  raise e)))

let accept ?(headers = []) ~max_message ~upgrade reqd session =
  let ready = Goo.Chan.create () in
  match
    Ws.Handshake.respond_with_upgrade ~headers:(Httpun.Headers.of_list headers) ~sha1 reqd (fun () ->
        upgrade
          (Gluten.make
             (module Ws.Server_connection)
             (Ws.Server_connection.create_websocket (fun wsd ->
                  let t = make wsd in
                  Goo.protect (fun () -> Goo.Chan.send ready t);
                  input_handlers ~max_message t))))
  with
  | Error _ as e -> e
  | Ok () ->
      (* The upgrade happens once the 101 response is written. *)
      let t = Goo.Chan.recv ready in
      (match session t with
      | () -> close t
      | exception (Goo.Cancelled as e) ->
          close ~code:1001 t;
          raise e
      | exception e ->
          close ~code:1011 t;
          raise e);
      Ok ()
