(* Which tables changed, for whoever watches them: an in-process registry
   fed by the listener (Notify tables) and the poller (Poll tables).

     let sub = Changes.subscribe changes in
     let since = Changes.now changes in
     ... read tables [ts] ...
     Changes.watch changes sub ~since ts;   (* wakes now if they changed meanwhile *)
     Changes.wait sub;                      (* blocks until one of them changes *)

   Tables are named schema.table ("public.orders"). Subscribers may be on
   any domain.

   By row: a subscriber that gives [~keys] for a table, e.g.
   [("public.orders", "id", id)], only wakes for changes to those values
   (any of them); for tables without [~keys] it wakes for any change. When
   an announcement doesn't say (no values: a TRUNCATE, a statement too big,
   a table without keys or with other ones, a Poll table), it wakes. Values
   are compared as Postgres prints them (uuid::text, int::text...). *)

type key = string * string * string (* table, column, value *)

type sub =
  { mutable tables: string list (* guarded by the registry's lock *)
  ; mutable filters: (string * (string * string) list) list (* table -> (column, value)s; any matches *)
  ; dirty: bool Atomic.t (* a wake-up is pending *)
  ; wake: unit Goo.Chan.t
  }

type t =
  { lock: Mutex.t (* Stdlib's: short sections that never block, from any domain *)
  ; watchers: (string, sub list) Hashtbl.t
  ; subs: (sub, unit) Hashtbl.t
  ; last_change: (string, int) Hashtbl.t (* table -> [seq] of its last change *)
  ; seq: int Atomic.t (* counts changes and losses *)
  ; mutable lost: int (* [seq] at the last loss *)
  }

let create () =
  { lock = Mutex.create (); watchers = Hashtbl.create 64; subs = Hashtbl.create 64; last_change = Hashtbl.create 64;
    seq = Atomic.make 0; lost = 0 }
;;

let locked t f = Mutex.protect t.lock f

(* {1 Subscribers} *)

let poke sub = if not (Atomic.exchange sub.dirty true) then Goo.Chan.send sub.wake ()

let subscribe t =
  let sub = { tables = []; filters = []; dirty = Atomic.make false; wake = Goo.Chan.create () } in
  locked t (fun () -> Hashtbl.replace t.subs sub ());
  sub
;;

let unwatch t sub =
  List.iter
    (fun table ->
      match List.filter (fun s -> s != sub) (Option.value (Hashtbl.find_opt t.watchers table) ~default:[]) with
      | [] -> Hashtbl.remove t.watchers table
      | rest -> Hashtbl.replace t.watchers table rest)
    sub.tables;
  sub.tables <- [];
  sub.filters <- []
;;

let unsubscribe t sub =
  locked t (fun () ->
    unwatch t sub;
    Hashtbl.remove t.subs sub)
;;

let now t = Atomic.get t.seq

let watch t sub ~since ?(keys = []) tables =
  let tables = List.sort_uniq String.compare tables in
  let filters =
    List.filter_map
      (fun table ->
        match List.filter_map (fun (t', col, v) -> if t' = table then Some (col, v) else None) keys with
        | [] -> None
        | fs -> Some (table, fs))
      tables
  in
  let missed =
    locked t (fun () ->
      unwatch t sub;
      sub.tables <- tables;
      sub.filters <- filters;
      List.iter
        (fun table -> Hashtbl.replace t.watchers table (sub :: Option.value (Hashtbl.find_opt t.watchers table) ~default:[]))
        tables;
      t.lost > since
      || List.exists (fun table -> Option.value (Hashtbl.find_opt t.last_change table) ~default:0 > since) tables)
  in
  if missed then poke sub
;;

let wait ?(settle = 0.) sub =
  Goo.Chan.recv sub.wake;
  if settle > 0. then Goo.sleep settle;
  Atomic.set sub.dirty false
;;

(* {1 Announcing} *)

let concerns sub ~table ~keys =
  match List.assoc_opt table sub.filters, keys with
  | None, _ | _, None -> true
  | Some filters, Some keys ->
    List.exists
      (fun (col, v) ->
        match List.assoc_opt col keys with
        | None -> true
        | Some vs -> List.mem v vs)
      filters
;;

let changed t ?keys table =
  let subs =
    locked t (fun () ->
      let seq = 1 + Atomic.fetch_and_add t.seq 1 in
      Hashtbl.replace t.last_change table seq;
      List.filter (fun sub -> concerns sub ~table ~keys) (Option.value (Hashtbl.find_opt t.watchers table) ~default:[]))
  in
  List.iter poke subs
;;

let lost t =
  let subs =
    locked t (fun () ->
      t.lost <- 1 + Atomic.fetch_and_add t.seq 1;
      Hashtbl.fold (fun s () acc -> s :: acc) t.subs [])
  in
  List.iter poke subs
;;

(* An announcement: "schema.table", or {"t": "schema.table", "k": {column:
   [values]}} from a table with keys. *)
let parse payload =
  let bare () = (payload, None) in
  if String.length payload = 0 || payload.[0] <> '{' then bare ()
  else
    match Yojson.Safe.from_string payload with
    | `Assoc fields ->
      (match List.assoc_opt "t" fields, List.assoc_opt "k" fields with
       | Some (`String table), Some (`Assoc keys) ->
         let values = function
           | `List vs -> Some (List.filter_map (function `String s -> Some s | _ -> None) vs)
           | _ -> None
         in
         let keys = List.filter_map (fun (col, vs) -> Option.map (fun vs -> (col, vs)) (values vs)) keys in
         (table, Some keys)
       | Some (`String table), _ -> (table, None)
       | _ -> bare ())
    | _ | (exception Yojson.Json_error _) -> bare ()
;;
