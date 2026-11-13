(* One LISTEN connection per process (libpq, not the pool: a pooled
   connection can't be kept listening), feeding Changes. Reconnects by
   itself; changes made while it was disconnected are lost, so every
   (re)connection wakes everyone. *)

module Log = (val Logs.src_log (Logs.Src.create "live.listener"))

let channel = "live"

(* Blocks the domain while connecting (libpq), like the pool does. *)
let connect url =
  let c = new Postgresql.connection ~conninfo:url () in
  match c#exec ~expect:[ Postgresql.Command_ok ] ("LISTEN " ^ channel) with
  | _ -> c
  | exception e ->
    c#finish;
    raise e
;;

let rec drain changes (c : Postgresql.connection) =
  match c#notifies with
  | Some { name; extra; _ } when name = channel ->
    let table, keys = Changes.parse extra in
    Changes.changed changes ?keys table;
    drain changes c
  | Some _ -> drain changes c
  | None -> ()
;;

(* Listen until cancelled, reconnecting (1s, 2s... up to 30s apart). *)
let listen changes ~connected ~url =
  let rec session ~retry_in =
    match connect url with
    | exception Postgresql.Error e ->
      Log.warn (fun m -> m "can't listen (retrying in %.0fs): %s" retry_in (Postgresql.string_of_error e));
      Goo.sleep retry_in;
      session ~retry_in:(Float.min 30. (retry_in *. 2.))
    | c ->
      Log.info (fun m -> m "listening for changes");
      Atomic.set connected true;
      Changes.lost changes;
      (match
         Fun.protect
           ~finally:(fun () ->
             Atomic.set connected false;
             c#finish)
           (fun () ->
             let fd = c#socket_descr in
             let rec loop () =
               Goo.await_readable fd;
               c#consume_input;
               drain changes c;
               loop ()
             in
             loop ())
       with
       | () -> ()
       | exception Postgresql.Error e -> Log.warn (fun m -> m "lost the connection: %s" (Postgresql.string_of_error e)));
      session ~retry_in:1.
  in
  session ~retry_in:1.
;;
