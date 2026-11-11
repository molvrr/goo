(* httpun on [Goo]: the same shape as httpun-eio, driving gluten's read
   and write state machines with two tasks per connection. *)

module R = Goo
module Buffer = Gluten.Buffer

let shutdown fd cmd =
  try Unix.shutdown fd cmd with Unix.Unix_error ((ENOTCONN | EBADF), _, _) -> ()

(* Run [a] and [b] concurrently; return the first result and cancel the
   other. *)
let first a b =
  let result = R.Chan.create () in
  let run f tag () =
    R.Chan.send result (match f () with v -> Ok (tag v) | exception e -> Error e)
  in
  let s = R.scope [ run a (fun v -> `A v); run b (fun v -> `B v) ] in
  let r = Fun.protect ~finally:(fun () -> R.cancel s) (fun () -> R.Chan.recv result) in
  (try R.join s with _ -> ());
  match r with Ok v -> v | Error e -> raise e

(* Wait for a wake-up registered through one of gluten's [yield_*]. The
   callback runs in whichever task made progress (often a handler's task),
   so it must not be a cancellation point: a cancelled task closing a body
   would otherwise lose the wake-up and raise inside httpun. *)
let yield register =
  let woken = R.Chan.create () in
  register (fun () -> R.protect (fun () -> R.Chan.send woken ()));
  woken

(* Readable with no data means the peer closed its side. *)
let rec watch_eof fd =
  R.await_readable fd;
  match Unix.recv fd (Bytes.create 1) 0 1 [ Unix.MSG_PEEK ] with
  | 0 -> `Eof
  | _ -> `Data
  | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) -> watch_eof fd
  | exception Unix.Unix_error ((ECONNRESET | ENOTCONN), _, _) -> `Eof

type transport = {
  read : Bigstringaf.t -> int -> int -> int;
  write : Bigstringaf.t -> int -> int -> int;
  watch_eof : unit -> [ `Eof | `Data ];
  shutdown_receive : unit -> unit;
  shutdown_send : unit -> unit;
}

let transport_of_fd fd =
  {
    read = (fun buf off len -> R.read_bigarray fd buf off len);
    write = (fun buf off len -> R.write_bigarray fd buf off len);
    watch_eof = (fun () -> watch_eof fd);
    shutdown_receive = (fun () -> shutdown fd Unix.SHUTDOWN_RECEIVE);
    shutdown_send = (fun () -> shutdown fd Unix.SHUTDOWN_SEND);
  }

(* Read into gluten's buffer; [0] means EOF (including a reset peer). *)
let read_into (tr : transport) buffer =
  let n = ref 0 in
  Buffer.put buffer
    ~f:(fun buf ~off ~len k ->
      k (try tr.read buf off len with Unix.Unix_error ((ECONNRESET | ENOTCONN), _, _) -> 0))
    (fun r -> n := r);
  !n

(* Write iovecs until one is partial. If the peer is gone, report what was
   written so far, or [`Closed] if nothing was. *)
let writev (tr : transport) iovecs =
  let rec go total = function
    | [] -> `Ok total
    | { Faraday.buffer; off; len } :: rest -> (
        match tr.write buffer off len with
        | n when n < len -> `Ok (total + n)
        | n -> go (total + n) rest
        | exception (Unix.Unix_error ((EPIPE | ECONNRESET | ENOTCONN), _, _) | End_of_file) ->
            if total > 0 then `Ok total else `Closed)
  in
  go 0 iovecs

let io_loop (type t) (module P : Gluten.RUNTIME with type t = t) ~read_buffer_size (t : t) (tr : transport) =
  let buffer = Buffer.create read_buffer_size in
  let feed f = Buffer.get buffer ~f:(fun buf ~off ~len -> f t buf ~off ~len) in
  let rec read_loop () =
    match P.next_read_operation t with
    | `Read ->
        (* Offer what's already buffered first: after a protocol switch
           (HTTP -> WebSocket) the bytes following the handshake are still
           in the buffer, and the peer may not send anything more until we
           react to them. Only read the socket once nothing is consumed. *)
        if feed P.read = 0 then
          if read_into tr buffer = 0 then ignore (feed P.read_eof : int) else ignore (feed P.read : int);
        read_loop ()
    | `Yield -> (
        let woken = yield (P.yield_reader t) in
        (* While a request is being handled, watch for the peer going away
           so streaming responses (e.g. SSE) notice the disconnect. *)
        match first (fun () -> R.Chan.recv woken) tr.watch_eof with
        | `A () -> read_loop ()
        | `B `Data ->
            R.Chan.recv woken;
            read_loop ()
        | `B `Eof ->
            tr.shutdown_receive ();
            P.report_write_result t `Closed;
            P.report_exn t End_of_file)
    | `Close -> tr.shutdown_receive ()
  in
  let rec write_loop () =
    match P.next_write_operation t with
    | `Write iovecs ->
        P.report_write_result t (writev tr iovecs);
        write_loop ()
    | `Yield ->
        R.Chan.recv (yield (P.yield_writer t));
        write_loop ()
    | `Close _ -> tr.shutdown_send ()
  in
  (* Tasks spawned while serving (request handlers, websocket sessions)
     join this scope. Once both loops are done the connection is over, so
     anything still running there is cancelled rather than left waiting for
     I/O that will never come. *)
  let loops_done = R.Chan.create () in
  let guard loop () =
    Fun.protect
      ~finally:(fun () -> R.protect (fun () -> R.Chan.send loops_done ()))
      (fun () -> try loop () with R.Cancelled -> raise R.Cancelled | exn -> P.report_exn t exn)
  in
  let s = R.scope [ guard read_loop; guard write_loop ] in
  Fun.protect
    ~finally:(fun () -> R.cancel s)
    (fun () ->
      R.Chan.recv loops_done;
      R.Chan.recv loops_done);
  R.protect (fun () -> R.join s)

let run_protocol p ~read_buffer_size t fd = io_loop p ~read_buffer_size t (transport_of_fd fd)

module Client = struct
  type t = { connection : Httpun.Client_connection.t; gluten : Gluten.Client.t; io : R.scope }

  let create_connection_on ?(config = Httpun.Config.default) tr =
    let connection = Httpun.Client_connection.create ~config () in
    let gluten = Gluten.Client.create ~protocol:(module Httpun.Client_connection) connection in
    let io =
      R.scope
        [ (fun () -> io_loop (module Gluten.Client) ~read_buffer_size:config.read_buffer_size gluten tr) ]
    in
    { connection; gluten; io }

  let create_connection ?config fd = create_connection_on ?config (transport_of_fd fd)

  let request t = Httpun.Client_connection.request t.connection
  let is_closed t = Gluten.Client.is_closed t.gluten
  let upgrade t impl = Gluten.Client.upgrade_protocol t.gluten impl

  let shutdown t =
    Gluten.Client.shutdown t.gluten;
    R.cancel t.io;
    R.protect (fun () -> try R.join t.io with _ -> ())
end

module Server = struct
  type request_handler = Unix.sockaddr -> Httpun.Reqd.t Gluten.reqd -> unit
  type error_handler = Unix.sockaddr -> Httpun.Server_connection.error_handler

  let create_connection_handler ?(config = Httpun.Config.default) ?error_handler
      ~(request_handler : request_handler) fd peer =
    let connection =
      Gluten.Server.create_upgradable
        ~protocol:(module Httpun.Server_connection)
        ~create:
          (Httpun.Server_connection.create ~config
             ?error_handler:(Option.map (fun h -> h peer) error_handler))
        (request_handler peer)
    in
    io_loop (module Gluten.Server) ~read_buffer_size:config.read_buffer_size connection (transport_of_fd fd)

  let start ?config ?domains ?acceptors ?backlog ?strategy ?error_handler addr request_handler =
    Goo_server.start ?domains ?acceptors ?backlog ?strategy addr
      (create_connection_handler ?config ?error_handler ~request_handler)
end
