(* Caqti on Goo. Fibers are plain direct-style code (as in caqti-eio):
   blocking points are Goo's, so a query waiting on Postgres suspends the
   task, not the domain. *)

open Caqti_platform

let log_src = Logs.Src.create "caqti.goo" ~doc:"Caqti on Goo"

module Fiber = struct
  type 'a t = 'a

  module Infix = struct
    let ( >>= ) x f = f x
    let ( >|= ) x f = f x
  end

  let return x = x
  let catch f g = try f () with exn -> g exn

  (* Cleanup runs protected: it returns pooled connections and closes
     things, which mustn't be cut short when the task is being cancelled
     (taking the pool's mutex is a cancellation point). *)
  let finally f g =
    match f () with
    | y ->
        Goo.protect g;
        y
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        Goo.protect g;
        Printexc.raise_with_backtrace exn bt

  let cleanup f g =
    try f ()
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      Goo.protect g;
      Printexc.raise_with_backtrace exn bt
end

module Stream = Caqti_platform.Stream.Make (Fiber)

(* Run [f], logging its failure (other than cancellation) instead of
   propagating it: for background work whose failure shouldn't take down
   everything else in its scope. *)
let background what f () =
  try f () with
  | Goo.Cancelled -> ()
  | exn -> Logs.err ~src:log_src (fun m -> m "%s failed: %s" what (Printexc.to_string exn))

(* Run the tasks until the first one returns, then cancel the others. *)
let first fns =
  let result = ref None and self = ref None in
  let finish r =
    if Option.is_none !result then result := Some r;
    Option.iter Goo.cancel !self
  in
  let s = Goo.scope (List.map (fun f () -> finish (f ())) fns) in
  self := Some s;
  (match Goo.join s with
  | () -> ()
  | exception Goo.Cancelled ->
      Goo.cancel s;
      Goo.protect (fun () -> try Goo.join s with _ -> ());
      raise Goo.Cancelled);
  match !result with Some r -> r | None -> raise Goo.Cancelled

module Switch = struct
  module Hooks = Caqti_platform.Switch.Make (Fiber)

  (* [scope] holds the pool's idle-timeout alarms. A keeper task keeps it
     running until the switch is released. *)
  type t = { hooks : Hooks.t; scope : Goo.scope }
  type hook = Hooks.hook

  exception Off = Hooks.Off

  let run f =
    let scope = Goo.scope [ (fun () -> Goo.Chan.recv (Goo.Chan.create ())) ] in
    let t = { hooks = Hooks.create (); scope } in
    let release () =
      Goo.protect (fun () ->
          (try Hooks.release t.hooks
           with exn ->
             Logs.err ~src:log_src (fun m ->
                 m "releasing switch: %s" (Printexc.to_string exn)));
          Goo.cancel scope;
          try Goo.join scope with _ -> ())
    in
    match f t with
    | v ->
        release ();
        v
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        release ();
        Printexc.raise_with_backtrace exn bt

  let check t = Hooks.check t.hooks
  let on_release_cancellable t f = Hooks.on_release_cancellable t.hooks f
  let remove_hook = Hooks.remove_hook
end

module System_core = struct
  module Fiber = Fiber
  module Stream = Stream
  module Switch = Switch

  type stdenv = unit

  (* Inline, as in caqti-blocking: the pool only uses this to put a returned
     connection back, a few lines under its mutex. A separate task could be
     cancelled before running and lose the connection (then [drain] waits
     for it forever). *)
  let async ~sw:_ f = f ()

  (* Not cancellable. Caqti's pool isn't written for cancellation at these
     points: a task cancelled in [Condition.wait] (waiting for a free
     connection) gets the pool's mutex back and raises, and [acquire] never
     unlocks it, so every later use of the pool hangs; its entry also stays
     in the wait queue and swallows the next wake-up. The sections under the
     pool's mutex are short, and the cancellation is delivered at the task's
     next blocking point (e.g. its first query), after which the connection
     goes back to the pool the normal way. *)
  module Mutex = struct
    type t = Goo.Mutex.t

    let create = Goo.Mutex.create
    let lock m = Goo.protect (fun () -> Goo.Mutex.lock m)
    let unlock = Goo.Mutex.unlock
  end

  module Condition = struct
    type t = Goo.Condition.t

    let create = Goo.Condition.create
    let wait c m = Goo.protect (fun () -> Goo.Condition.wait c m)
    let signal = Goo.Condition.signal
  end

  module Log = struct
    type 'a log = 'a Logs.log

    let err ?(src = Logging.default_log_src) = Logs.err ~src
    let warn ?(src = Logging.default_log_src) = Logs.warn ~src
    let info ?(src = Logging.default_log_src) = Logs.info ~src
    let debug ?(src = Logging.default_log_src) = Logs.debug ~src
  end

  module Sequencer = struct
    type 'a t = 'a * Goo.Mutex.t

    let create x = (x, Goo.Mutex.create ())
    let enqueue (x, m) f = Goo.Mutex.use m (fun () -> f x)
  end
end

module Alarm = struct
  (* Unscheduling only marks the alarm: its task sleeps until the deadline
     (or until the switch is released) and then does nothing. *)
  type t = { mutable cancelled : bool }

  let schedule ~sw ~stdenv:() time f =
    let a = { cancelled = false } in
    let now = Mtime_clock.now () in
    let delay =
      if Mtime.is_later time ~than:now then Mtime.Span.to_float_ns (Mtime.span now time) /. 1e9
      else 0.
    in
    (* [sleep] blocks even for 0, which provides the required yield. *)
    Goo.spawn_in sw.Switch.scope
      (background "caqti alarm" (fun () ->
           Goo.sleep delay;
           if not a.cancelled then f ()));
    a

  let unschedule a = a.cancelled <- true
end

module System = struct
  include System_core

  (* Only the pure-OCaml pgx driver talks to sockets through [Net]; the
     libpq-based postgresql driver doesn't, and is the one supported here. *)
  module Net = struct
    module Sockaddr = struct
      type t = Unix.sockaddr

      let unix s = Unix.ADDR_UNIX s

      let tcp (addr, port) =
        Unix.ADDR_INET (Unix.inet_addr_of_string (Ipaddr.to_string addr), port)
    end

    let getaddrinfo ~stdenv:() host port =
      try
        Unix.getaddrinfo (Domain_name.to_string host) (string_of_int port)
          [ Unix.AI_SOCKTYPE SOCK_STREAM ]
        |> List.map (fun ai -> ai.Unix.ai_addr)
        |> Result.ok
      with
      | Not_found -> Ok []
      | Unix.Unix_error (code, _, _) ->
          Error (`Msg ("Cannot resolve host name: " ^ Unix.error_message code))

    let convert_io_exception _ = None

    module Socket = struct
      type t = |

      let output_char (s : t) _ = match s with _ -> .
      let output_string (s : t) _ = match s with _ -> .
      let flush (s : t) = match s with _ -> .
      let input_char (s : t) = match s with _ -> .
      let really_input (s : t) _ _ _ = match s with _ -> .
      let close (s : t) = match s with _ -> .
    end

    type tcp_flow = Socket.t
    type tls_flow = Socket.t

    let connect_tcp ~sw:_ ~stdenv:() _ =
      Error
        (Caqti_error.Msg
           "caqti-goo doesn't support drivers using Caqti's sockets (pgx); use the \
            postgresql driver")

    let tcp_flow_of_socket _ = None
    let socket_of_tls_flow ~sw:_ = Fun.id

    module type TLS_PROVIDER =
      System_sig.TLS_PROVIDER
        with type 'a fiber := 'a
         and type tcp_flow := Socket.t
         and type tls_flow := Socket.t

    let register_tls_provider (_ : (module TLS_PROVIDER)) = ()
    let tls_providers _ = []
  end
end

module System_unix = struct
  module Unix = struct
    type file_descr = Unix.file_descr

    let wrap_fd f fd = f fd

    let poll ~stdenv:() ?(read = false) ?(write = false) ?timeout fd =
      let ready =
        match (read, write) with
        | false, false -> []
        | true, false -> [ (fun () -> Goo.await_readable fd; (true, false, false)) ]
        | false, true -> [ (fun () -> Goo.await_writable fd; (false, true, false)) ]
        | true, true ->
            [
              (fun () -> Goo.await_readable fd; (true, false, false));
              (fun () -> Goo.await_writable fd; (false, true, false));
            ]
      in
      let timeout =
        match timeout with
        | None -> []
        | Some t -> [ (fun () -> Goo.sleep t; (false, false, true)) ]
      in
      match ready @ timeout with
      | [] -> (false, false, false)
      | [ f ] -> f ()
      | fns -> first fns
  end

  (* Only the sqlite3 driver detaches work; here it simply runs (and blocks
     the domain). *)
  module Preemptive = struct
    let detach f x = f x
    let run_in_main f = f ()
  end
end

(* Connections are opened and closed without cancellation: a task cancelled
   while its connection was being opened (e.g. it had been waiting for a
   slot) would leave the new connection open on the server, uncounted by
   the pool; enough of them run the server out of connections. *)
module Pool = struct
  module P = Caqti_platform.Pool.Make (System) (Alarm)
  include P

  let create ?config ?check ?validate ?log_src ~sw ~stdenv connect disconnect =
    P.create ?config ?check ?validate ?log_src ~sw ~stdenv
      (fun () -> Goo.protect connect)
      (fun c -> Goo.protect (fun () -> disconnect c))
end
module Loader = Caqti_platform_unix.Driver_loader.Make (System) (System_unix)
module Connector = Connector.Make (System) (Pool) (Loader)

module type CONNECTION =
  Caqti_connection_sig.S with type 'a fiber := 'a and type ('a, 'e) stream := ('a, 'e) Stream.t

type connection = (module CONNECTION)

let connect ?subst ?env ?config ?tweaks_version ~sw uri =
  Connector.connect ?subst ?env ?config ?tweaks_version ~sw ~stdenv:() uri

let with_connection ?subst ?env ?config ?tweaks_version uri f =
  Connector.with_connection ?subst ?env ?config ?tweaks_version ~stdenv:() uri f

let connect_pool ?pool_config ?post_connect ?subst ?env ?config ?tweaks_version ~sw uri =
  Connector.connect_pool ?pool_config ?post_connect ?subst ?env ?config ?tweaks_version ~sw
    ~stdenv:() uri

let or_fail = function Ok x -> x | Error (#Caqti_error.t as err) -> raise (Caqti_error.Exn err)

module Domain_pool = struct
  type pool = (connection, Caqti_error.t) Pool.t

  (* One per domain that has used the pool. The pool lives in a task posted
     to that domain, inside a switch, until [close]. *)
  type slot = {
    lock : Goo.Mutex.t;
    mutable pool : pool option;
    stop : unit Goo.Chan.t;
    stopped : unit Goo.Chan.t;
  }

  type t = {
    open_pool : sw:Switch.t -> (pool, Caqti_error.t) result;
    lock : Stdlib.Mutex.t;
    slots : (int, slot) Hashtbl.t;
    mutable closed : bool;
  }

  let create ?pool_config ?post_connect ?subst ?env ?config uri =
    let open_pool ~sw =
      (connect_pool ?pool_config ?post_connect ?subst ?env ?config ~sw uri
        :> (pool, Caqti_error.t) result)
    in
    { open_pool; lock = Stdlib.Mutex.create (); slots = Hashtbl.create 8; closed = false }

  let slot t i =
    Stdlib.Mutex.protect t.lock (fun () ->
        if t.closed then invalid_arg "Caqti_goo.Domain_pool: closed";
        match Hashtbl.find_opt t.slots i with
        | Some s -> s
        | None ->
            let s =
              {
                lock = Goo.Mutex.create ();
                pool = None;
                stop = Goo.Chan.create ();
                stopped = Goo.Chan.create ();
              }
            in
            Hashtbl.replace t.slots i s;
            s)

  let start t s =
    let reply = Goo.Chan.create () in
    Goo.post (Goo.self ()) (fun () ->
        Fun.protect ~finally:(fun () -> Goo.Chan.send s.stopped ()) (fun () ->
            Switch.run (fun sw ->
                match t.open_pool ~sw with
                | Error _ as e -> Goo.Chan.send reply e
                | Ok pool ->
                    Goo.Chan.send reply (Ok pool);
                    Goo.Chan.recv s.stop)));
    Goo.Chan.recv reply

  let get t =
    let s = slot t (Goo.index (Goo.self ())) in
    match s.pool with
    | Some p -> p
    | None ->
        Goo.Mutex.use s.lock (fun () ->
            match s.pool with
            | Some p -> p
            | None -> (
                match start t s with
                | Ok p ->
                    s.pool <- Some p;
                    p
                | Error err -> raise (Caqti_error.Exn err)))

  (* A task cancelled while it waited for a connection (it gets one: the
     wait isn't cancellable, see System_core.Mutex) gives it back before
     starting a query. Cancelled mid-query, the driver would drop the
     connection and reconnect (a blocking connect), and many at once leave
     old backends lingering on the server past max_connections. *)
  let use ?priority t f = Pool.use ?priority (fun c -> Goo.check (); f c) (get t)

  let close t =
    let slots =
      Stdlib.Mutex.protect t.lock (fun () ->
          t.closed <- true;
          List.of_seq (Hashtbl.to_seq_values t.slots))
    in
    List.iter
      (fun s ->
        if Option.is_some s.pool then begin
          Goo.Chan.send s.stop ();
          Goo.Chan.recv s.stopped
        end)
      slots
end
