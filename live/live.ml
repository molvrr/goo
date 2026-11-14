(* See live.mli. *)

module Log = (val Logs.src_log (Logs.Src.create "live"))
module Changes = Changes
module Reads = Reads
module DP = Caqti_goo.Domain_pool

type mode = Table.mode =
  | Notify
  | Poll

type table = Table.t

let table = Table.make

type entry =
  { table: Table.t
  ; mutable mode: mode
  ; mutable since: float (* when it got [mode] *)
  ; mutable last: int option (* its write counter at the last poll *)
  ; rates: Rates.t
  }

type t =
  { db: DP.t
  ; url: string
  ; changes: Changes.t
  ; lock: Mutex.t (* guards the entries' mutable fields *)
  ; entries: entry list
  ; max_writes_per_s: float
  ; poll_every: float
  ; burst_window: float
  ; calm_window: float
  ; on_unwatched: [ `Fail | `Warn | `Ignore ]
  ; check_tables: bool (* pages with ~tables are still tracked, to check them *)
  ; warned: (string, unit) Hashtbl.t (* path|table, warned once *)
  ; connected: bool Atomic.t
  ; mutable scope: Goo.scope option
  }

let create ?(max_writes_per_s = 20.) ?(poll_every = 1.) ?(burst_window = 60.) ?(calm_window = 600.)
    ?(on_unwatched = `Warn) ?(check_tables = false) ~db ~url tables =
  let now = Unix.gettimeofday () in
  let entries =
    List.map
      (fun (tb : Table.t) ->
        { table = tb; mode = Table.initial_mode tb; since = now; last = None;
          rates = Rates.create ~keep:(Float.max burst_window calm_window +. (2. *. poll_every)) })
      tables
  in
  { db; url = Uri.to_string url; changes = Changes.create (); lock = Mutex.create (); entries; max_writes_per_s; poll_every;
    burst_window; calm_window; on_unwatched; check_tables; warned = Hashtbl.create 16; connected = Atomic.make false; scope = None }
;;

let locked t f = Mutex.protect t.lock f
let changes t = t.changes
let connected t = Atomic.get t.connected
let find t name = List.find_opt (fun e -> e.table.name = name) t.entries
let watched t name = Option.is_some (find t name)
let modes t = locked t (fun () -> List.map (fun e -> (e.table.name, e.mode)) t.entries)

(* {1 Triggers} *)

let desired t =
  locked t (fun () -> List.filter_map (fun e -> if e.mode = Notify then Some (e.table.name, e.table.keys) else None) t.entries)
;;

let show_error = function
  | `Msg m -> m
  | #Caqti_error.t as e -> Caqti_error.show e
;;

let reconcile t =
  (* The pool only carries Caqti's errors: ours come back inside. *)
  match Result.join (DP.use t.db (fun c -> Ok (Install.reconcile c (desired t))) :> (_, [ Caqti_error.t | `Msg of string ]) result) with
  | Ok changes ->
    List.iter
      (function
        | `Installed table -> Log.info (fun m -> m "triggers on %s" table)
        | `Removed table -> Log.info (fun m -> m "no more triggers on %s" table))
      changes;
    Ok ()
  | Error e -> Error (show_error e)
;;

(* {1 Polling} *)

let counts_q =
  Caqti_request.Infix.(
    Caqti_type.(unit ->* t2 string int)
      {|SELECT n.nspname || '.' || c.relname, sum(s.n_tup_ins + s.n_tup_upd + s.n_tup_del)::bigint
        FROM pg_stat_user_tables s
        JOIN pg_class c ON c.oid = coalesce(pg_partition_root(s.relid), s.relid)
        JOIN pg_namespace n ON n.oid = c.relnamespace
        GROUP BY 1|})
;;

(* One poll: wake the watchers of Poll tables that were written, and say
   which Auto tables should change mode. *)
let tick t ~now counts =
  locked t (fun () ->
    List.fold_left
      (fun (wake, switch) e ->
        match List.assoc_opt e.table.name counts with
        | None -> (wake, switch)
        | Some n ->
          let written = match e.last with Some l -> n <> l | None -> false in
          e.last <- Some n;
          Rates.add e.rates ~now n;
          let wake = if written && e.mode = Poll then e.table.name :: wake else wake in
          let switch =
            match e.table.policy, e.mode with
            | `Auto, Notify ->
              (match Rates.rate e.rates ~window:t.burst_window with
               | Some r when r > t.max_writes_per_s -> (e, Poll, r) :: switch
               | _ -> switch)
            | `Auto, Poll when now -. e.since >= t.calm_window ->
              (match Rates.rate e.rates ~window:t.calm_window with
               | Some r when r < t.max_writes_per_s /. 4. -> (e, Notify, r) :: switch
               | _ -> switch)
            | _ -> switch
          in
          (wake, switch))
      ([], []) t.entries)
;;

let apply_switches t ~now switches =
  if switches <> [] then begin
    let before = locked t (fun () -> List.map (fun (e, _, _) -> (e, e.mode, e.since)) switches) in
    locked t (fun () -> List.iter (fun (e, mode, _) -> e.mode <- mode; e.since <- now) switches);
    match reconcile t with
    | Ok () ->
      List.iter
        (fun (e, mode, r) ->
          Log.warn (fun m ->
            m "%s: %.1f writes/s, %s" e.table.name r
              (match mode with Poll -> "polling it instead of triggers" | Notify -> "back to triggers"));
          (* Back to triggers: what was written meanwhile wasn't announced. *)
          if mode = Notify then Changes.changed t.changes e.table.name)
        switches
    | Error msg ->
      Log.err (fun m -> m "couldn't change modes (will retry): %s" msg);
      locked t (fun () -> List.iter (fun (e, mode, since) -> e.mode <- mode; e.since <- since) before)
  end
;;

let poll t =
  let rec go () =
    (* Not cancelled mid-query ({!stop}): the pool would drop the connection. *)
    (match Goo.protect (fun () -> DP.use t.db (fun (module C : Caqti_goo.CONNECTION) -> C.collect_list counts_q ())) with
     | Ok counts ->
       let now = Unix.gettimeofday () in
       let wake, switches = tick t ~now counts in
       List.iter (fun table -> Changes.changed t.changes table) wake;
       Goo.protect (fun () -> apply_switches t ~now switches)
     | Error e -> Log.warn (fun m -> m "can't read write counters: %s" (Caqti_error.show e)));
    Goo.sleep t.poll_every;
    go ()
  in
  go ()
;;

(* {1 Running} *)

let start t =
  if Option.is_some t.scope then invalid_arg "Live.start: already started";
  (match reconcile t with Ok () -> () | Error m -> failwith ("Live.start: " ^ m));
  t.scope <-
    Some (Goo.scope [ (fun () -> Listener.listen t.changes ~connected:t.connected ~url:t.url); (fun () -> poll t) ])
;;

let stop t =
  Option.iter
    (fun s ->
      Goo.cancel s;
      Goo.join s)
    t.scope;
  t.scope <- None
;;

(* {1 Pages} *)

let script = Html.script [ Html.Attributes.script_type `module_; Html.Attributes.src Datastar.js_file ] []
let render_string children = String.concat "" (List.map Html.render children)

let default_show = function
  | #Caqti_error.t as e -> Caqti_error.show e
  | _ -> "error"
;;

let short name =
  let p = "public." in
  if String.starts_with ~prefix:p name then String.sub name (String.length p) (String.length name - String.length p) else name
;;

(* A page that may miss changes: per [on_unwatched], an error, a warning
   (once per page and message) or nothing. *)
let report t req msg =
  match t.on_unwatched with
  | `Ignore -> Ok ()
  | `Fail -> Error msg
  | `Warn ->
    let fresh =
      locked t (fun () ->
        let k = Web.Request.path req ^ "|" ^ msg in
        let fresh = not (Hashtbl.mem t.warned k) in
        Hashtbl.replace t.warned k ();
        fresh)
    in
    if fresh then Log.warn (fun m -> m "%s" msg);
    Ok ()
;;

let tables_hint tables = String.concat ", " (List.map (fun tb -> Printf.sprintf "Live.table %S" (short tb)) tables)

(* Tables a page reads that the app didn't give Live.create. *)
let check_reads t req tables =
  match List.filter (fun tb -> not (watched t tb)) tables with
  | [] -> Ok ()
  | missing ->
    report t req
      (Printf.sprintf "%s reads %s, which live pages don't watch: add %s to the tables given to Live.create"
         (Web.Request.path req) (String.concat ", " missing) (tables_hint missing))
;;

(* Tables a page with [~tables] read but didn't list. *)
let check_declared t req ~declared read =
  match List.filter (fun tb -> not (List.mem tb declared)) read with
  | [] -> Ok ()
  | missing ->
    report t req
      (Printf.sprintf "%s reads %s, missing from its ~tables: it wouldn't update when they change"
         (Web.Request.path req) (String.concat ", " missing))
;;

let ( let* ) = Result.bind

(* One render: the children and the tables to watch. Tracked (see Reads)
   unless the page lists its tables, and then only to check them when
   [check_tables]. *)
let render t ~show_error ?tables ~data req =
  let tracked () =
    match DP.use t.db (fun conn -> Ok (Reads.track conn (data req))) with
    | exception Reads.Wrote ts ->
      Error (`Data ("the render wrote to " ^ String.concat ", " ts ^ " (rolled back): renders must only read"))
    | Ok (Ok r) -> Ok r
    | Ok (Error e) -> Error (`Data (show_error e))
    | Error e -> Error (`Data (Caqti_error.show e))
  in
  let unwatched = Result.map_error (fun m -> `Unwatched m) in
  match tables with
  | None ->
    let* children, read = tracked () in
    let* () = unwatched (check_reads t req read) in
    Ok (children, read)
  | Some declared ->
    let declared = List.sort_uniq String.compare (List.map Table.qualify declared) in
    let* children =
      if t.check_tables then
        let* children, read = tracked () in
        let* () = unwatched (check_declared t req ~declared read) in
        Ok children
      else
        match DP.use t.db (fun conn -> Ok (data req conn)) with
        | Ok (Ok children) -> Ok children
        | Ok (Error e) -> Error (`Data (show_error e))
        | Error e -> Error (`Data (Caqti_error.show e))
    in
    let* () = unwatched (check_reads t req declared) in
    Ok (children, declared)
;;

let stream t ?(settle = 0.05) ?every ?(also = []) ?(keys = []) ?tables ?(show_error = default_show) ~id ~data req send =
  let sub = Changes.subscribe t.changes in
  Goo.defer (fun () -> Changes.unsubscribe t.changes sub);
  let last = ref None in
  let rec loop ~retry_in =
    let since = Changes.now t.changes in
    (* Not cancelled mid-query (the browser left): the driver would drop the
       connection and reconnect, which blocks the domain. The cancellation
       lands right after, at the next blocking point. *)
    match Goo.protect (fun () -> render t ~show_error ?tables ~data req) with
    | Ok (children, tables) ->
      Changes.watch t.changes sub ~since ~keys (also @ tables);
      let html = render_string children in
      if !last <> Some html then begin
        last := Some html;
        send (Datastar.Sse.patch_elements ~selector:("#" ^ id) ~mode:`inner children)
      end;
      Changes.wait ~settle sub;
      loop ~retry_in:1.
    | Error (`Unwatched m) -> Log.err (fun f -> f "%s" m) (* ends the stream *)
    | Error (`Data m) ->
      Log.err (fun f -> f "%s: %s (retrying in %.0fs)" (Web.Request.path req) m retry_in);
      Goo.sleep retry_in;
      loop ~retry_in:(Float.min 30. (retry_in *. 2.))
  in
  let tick s () =
    let rec go () =
      Goo.sleep s;
      Changes.poke sub;
      go ()
    in
    go ()
  in
  Goo.scoped ((fun () -> loop ~retry_in:1.) :: Option.to_list (Option.map tick every))
;;

let root ~id req children =
  Html.div
    [ Datastar.Attributes.init (Datastar.Actions.get (Web.Request.target req)) ]
    [ script; Html.div [ Html.Attributes.id (Css.Id.make id) ] children ]
;;

let page t ?(id = "live") ?settle ?every ?also ?(keys = fun _ -> []) ?tables ?(show_error = default_show) ~shell ~data
    req =
  if Datastar_web.is_datastar_request req then
    Datastar_web.sse (stream t ~id ?settle ?every ?also ~keys:(keys req) ?tables ~show_error ~data req)
  else
    (* Not cut mid-query if the client leaves (see [stream]). *)
    match Goo.protect (fun () -> render t ~show_error ?tables ~data req) with
    | Ok (children, _) -> Web.Response.html (shell req (root ~id req children))
    | Error (`Unwatched m) ->
      Log.err (fun f -> f "%s" m);
      Web.Response.text ~status:`Internal_server_error "Internal error."
    | Error (`Data m) ->
      Log.err (fun f -> f "%s: %s" (Web.Request.path req) m);
      Web.Response.text ~status:`Internal_server_error "Internal error."
;;
