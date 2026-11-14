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
  { id: int (* the key in the registry's tables (a sub's fields change, so it can't be its own key) *)
  ; mutable tables: string list (* guarded by the registry's lock *)
  ; mutable filters: (string * (string * string) list) list (* table -> (column, value)s; any matches *)
  ; dirty: bool Atomic.t (* a wake-up is pending *)
  ; wake: unit Goo.Chan.t
  }

type t =
  { lock: Mutex.t (* Stdlib's: short sections that never block, from any domain *)
  ; watchers: (string, (int, sub) Hashtbl.t) Hashtbl.t (* table -> its watchers, by id *)
  ; subs: (int, sub) Hashtbl.t
  ; next_id: int Atomic.t
  ; last_change: (string, int) Hashtbl.t (* table -> [seq] of its last change *)
  ; seq: int Atomic.t (* counts changes and losses *)
  ; mutable lost: int (* [seq] at the last loss *)
  }

let create () =
  { lock = Mutex.create (); watchers = Hashtbl.create 64; subs = Hashtbl.create 64; next_id = Atomic.make 0;
    last_change = Hashtbl.create 64; seq = Atomic.make 0; lost = 0 }
;;

let locked t f = Mutex.protect t.lock f

(* {1 Subscribers} *)

let poke sub = if not (Atomic.exchange sub.dirty true) then Goo.Chan.send sub.wake ()

let subscribe t =
  let sub =
    { id = Atomic.fetch_and_add t.next_id 1; tables = []; filters = []; dirty = Atomic.make false;
      wake = Goo.Chan.create () }
  in
  locked t (fun () -> Hashtbl.replace t.subs sub.id sub);
  sub
;;

(* O(1) per table, however many watch it. *)
let unwatch t sub =
  List.iter
    (fun table ->
      match Hashtbl.find_opt t.watchers table with
      | None -> ()
      | Some w ->
        Hashtbl.remove w sub.id;
        if Hashtbl.length w = 0 then Hashtbl.remove t.watchers table)
    sub.tables;
  sub.tables <- [];
  sub.filters <- []
;;

let unsubscribe t sub =
  locked t (fun () ->
    unwatch t sub;
    Hashtbl.remove t.subs sub.id)
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
        (fun table ->
          let w =
            match Hashtbl.find_opt t.watchers table with
            | Some w -> w
            | None ->
              let w = Hashtbl.create 16 in
              Hashtbl.replace t.watchers table w;
              w
          in
          Hashtbl.replace w sub.id sub)
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

(* How many subscribers watch [table], and how many there are. *)
let watching t table = locked t (fun () -> match Hashtbl.find_opt t.watchers table with Some w -> Hashtbl.length w | None -> 0)
let subscribers t = locked t (fun () -> Hashtbl.length t.subs)

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
      match Hashtbl.find_opt t.watchers table with
      | None -> []
      | Some w -> Hashtbl.fold (fun _ sub acc -> if concerns sub ~table ~keys then sub :: acc else acc) w [])
  in
  List.iter poke subs
;;

let lost t =
  let subs =
    locked t (fun () ->
      t.lost <- 1 + Atomic.fetch_and_add t.seq 1;
      Hashtbl.fold (fun _ s acc -> s :: acc) t.subs [])
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
