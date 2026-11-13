(* The triggers that announce writes, installed by the app from its list of
   tables (no migration): [reconcile] makes the database match it, in one
   transaction under an advisory lock (several processes may start at
   once), with a short lock_timeout so a busy table makes it fail rather
   than wait.

   A table with keys gets one trigger per event (Postgres only gives
   transition tables to single-event triggers) announcing the key columns'
   values in the rows the statement changed:

     {"t": "public.orders", "k": {"id": ["9f1c..."]}}

   A table without keys announces its bare name, once per statement. Every
   announcement is sent on commit only; past ~8000 bytes (Postgres' limit)
   it falls back to the bare name. Triggers named live* on tables not in
   the list are dropped: the list is the truth. *)

open Caqti_request.Infix
module T = Caqti_type

let ( let* ) = Result.bind
let lit sql = Caqti_request.create ~oneshot:true T.unit T.unit Caqti_mult.zero (fun _ -> Caqti_query.L sql)

let notify_fn =
  {|CREATE OR REPLACE FUNCTION live_notify() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  PERFORM pg_notify('live', TG_TABLE_SCHEMA || '.' || TG_TABLE_NAME);
  RETURN NULL;
END $$|}
;;

let notify_keys_fn =
  {|CREATE OR REPLACE FUNCTION live_notify_keys() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE
  tbl text := TG_TABLE_SCHEMA || '.' || TG_TABLE_NAME;
  changed text := CASE TG_OP
    WHEN 'INSERT' THEN 'SELECT * FROM live_new'
    WHEN 'DELETE' THEN 'SELECT * FROM live_old'
    ELSE 'SELECT * FROM live_new UNION ALL SELECT * FROM live_old' END;
  any_row boolean;
  keys jsonb := '{}';
  col text;
  vals jsonb;
  payload text;
BEGIN
  EXECUTE format('SELECT EXISTS (%s)', changed) INTO any_row;
  IF NOT any_row THEN RETURN NULL; END IF;
  FOREACH col IN ARRAY TG_ARGV LOOP
    EXECUTE format(
      'SELECT coalesce(jsonb_agg(DISTINCT v), ''[]'') FROM (SELECT (%I)::text AS v FROM (%s) r) s WHERE v IS NOT NULL',
      col, changed) INTO vals;
    keys := keys || jsonb_build_object(col, vals);
  END LOOP;
  payload := jsonb_build_object('t', tbl, 'k', keys)::text;
  IF octet_length(payload) > 7900 THEN payload := tbl; END IF;
  PERFORM pg_notify('live', payload);
  RETURN NULL;
END $$|}
;;

let names = [ "live"; "live_insert"; "live_update"; "live_delete"; "live_truncate" ]

(* {1 Quoting} *)

let ident s = "\"" ^ String.concat "\"\"" (String.split_on_char '"' s) ^ "\""
let literal s = "'" ^ String.concat "''" (String.split_on_char '\'' s) ^ "'"

(* "public.orders" -> "public"."orders" *)
let qualified name =
  match String.index_opt name '.' with
  | Some i -> ident (String.sub name 0 i) ^ "." ^ ident (String.sub name (i + 1) (String.length name - i - 1))
  | None -> ident name
;;

(* {1 What's there} *)

let lock_q = T.(unit ->! string) "SELECT pg_advisory_xact_lock(7461766565)::text"
let lock_timeout_q = T.(unit ->! string) "SELECT set_config('lock_timeout', '2s', true)"
let exists_q = T.(string ->! option string) "SELECT to_regclass(?)::text"

let columns_q =
  T.(string ->* string)
    "SELECT attname::text FROM pg_attribute WHERE attrelid = to_regclass(?) AND attnum > 0 AND NOT attisdropped"
;;

let triggers_q =
  T.(unit ->* t3 string string string)
    {|SELECT n.nspname || '.' || c.relname, t.tgname::text, encode(t.tgargs, 'escape')
      FROM pg_trigger t JOIN pg_class c ON c.oid = t.tgrelid JOIN pg_namespace n ON n.oid = c.relnamespace
      WHERE NOT t.tgisinternal AND t.tgname IN ('live', 'live_insert', 'live_update', 'live_delete', 'live_truncate')|}
;;

(* tgargs, escaped: "id\000product_id\000" *)
let args s =
  let sep = "\\000" in
  let rec go acc s =
    match Str_find.find s sep with
    | Some i -> go (String.sub s 0 i :: acc) (String.sub s (i + 4) (String.length s - i - 4))
    | None -> List.rev (if s = "" then acc else s :: acc)
  in
  go [] s
;;

type state =
  | Plain (* the one "live" trigger *)
  | Keys of string list
  | Broken (* some of the triggers only *)

let current rows =
  let tables = List.sort_uniq String.compare (List.map (fun (t, _, _) -> t) rows) in
  List.map
    (fun table ->
      let mine = List.filter_map (fun (t, n, a) -> if t = table then Some (n, a) else None) rows in
      let has n = List.mem_assoc n mine in
      let state =
        if has "live" && List.length mine = 1 then Plain
        else if List.for_all has [ "live_insert"; "live_update"; "live_delete"; "live_truncate" ] && List.length mine = 4 then
          let keys = args (List.assoc "live_insert" mine) in
          if List.for_all (fun n -> List.sort compare (args (List.assoc n mine)) = List.sort compare keys) [ "live_update"; "live_delete" ]
          then Keys (List.sort_uniq String.compare keys)
          else Broken
        else Broken
      in
      (table, state))
    tables
;;

(* {1 Making it so} *)

let drop_all table = List.map (fun n -> Printf.sprintf "DROP TRIGGER IF EXISTS %s ON %s" (ident n) (qualified table)) names

let create table = function
  | [] ->
    [ Printf.sprintf
        "CREATE TRIGGER live AFTER INSERT OR UPDATE OR DELETE OR TRUNCATE ON %s FOR EACH STATEMENT EXECUTE FUNCTION live_notify()"
        (qualified table) ]
  | keys ->
    let t = qualified table and a = String.concat ", " (List.map literal keys) in
    [ Printf.sprintf
        "CREATE TRIGGER live_insert AFTER INSERT ON %s REFERENCING NEW TABLE AS live_new FOR EACH STATEMENT EXECUTE FUNCTION live_notify_keys(%s)"
        t a;
      Printf.sprintf
        "CREATE TRIGGER live_update AFTER UPDATE ON %s REFERENCING OLD TABLE AS live_old NEW TABLE AS live_new FOR EACH STATEMENT EXECUTE FUNCTION live_notify_keys(%s)"
        t a;
      Printf.sprintf
        "CREATE TRIGGER live_delete AFTER DELETE ON %s REFERENCING OLD TABLE AS live_old FOR EACH STATEMENT EXECUTE FUNCTION live_notify_keys(%s)"
        t a;
      Printf.sprintf "CREATE TRIGGER live_truncate AFTER TRUNCATE ON %s FOR EACH STATEMENT EXECUTE FUNCTION live_notify()" t ]
;;

type change =
  [ `Installed of string
  | `Removed of string
  ]

(* Makes the database's live triggers exactly [desired] (table, keys);
   returns what it changed. *)
let reconcile ((module C : Caqti_goo.CONNECTION) : Caqti_goo.connection) (desired : (string * string list) list) =
  let exec sql = C.exec (lit sql) () in
  let rec all = function [] -> Ok () | s :: rest -> let* () = exec s in all rest in
  C.with_transaction (fun () ->
    let* _ = C.find lock_q () in
    let* _ = C.find lock_timeout_q () in
    let* () = exec notify_fn in
    let* () = exec notify_keys_fn in
    let* rows = C.collect_list triggers_q () in
    let current = current rows in
    let check (table, keys) =
      let* found = C.find exists_q (qualified table) in
      if found = None then Error (`Msg (Printf.sprintf "live: no table %s" table))
      else
        let* cols = C.collect_list columns_q (qualified table) in
        match List.filter (fun k -> not (List.mem k cols)) keys with
        | [] -> Ok ()
        | missing -> Error (`Msg (Printf.sprintf "live: %s has no column %s" table (String.concat ", " missing)))
    in
    let rec checks = function [] -> Ok () | d :: rest -> let* () = check d in checks rest in
    let* () = checks desired in
    let install =
      List.filter
        (fun (table, keys) ->
          match List.assoc_opt table current with
          | Some Plain -> keys <> []
          | Some (Keys k) -> k <> keys
          | Some Broken | None -> true)
        desired
    in
    let remove = List.filter (fun (table, _) -> not (List.mem_assoc table desired)) current in
    let* () = all (List.concat_map (fun (table, _) -> drop_all table) remove) in
    let* () = all (List.concat_map (fun (table, keys) -> drop_all table @ create table keys) install) in
    Ok (List.map (fun (t, _) -> `Removed t) remove @ List.map (fun (t, _) -> `Installed t) install))
;;
