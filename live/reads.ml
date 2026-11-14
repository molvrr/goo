(* Which tables a piece of code reads: [track] runs [f] in a transaction
   and, before committing, asks Postgres which relations the transaction
   holds locks on (pg_locks). Every table a query uses is locked until the
   end of the transaction, so views, joins, subqueries, SQL functions and
   partitions are all seen (partitions as their root table, the one
   triggers and pages name).

   It errs on the side of more: a table a query names but never reaches
   (a branch not taken, an EXISTS not evaluated) is locked all the same.
   That's safe for live pages: at worst an extra render.

   Three round trips besides [f]'s queries: BEGIN, the locks, COMMIT.

   Writes show as stronger locks: unless [~writes:true], a transaction that
   wrote is rolled back and [Wrote] raised, since a render that writes
   would wake itself forever. *)

open Caqti_request.Infix

exception Wrote of string list

let locks =
  Caqti_type.(unit ->* t2 string bool)
    {|SELECT DISTINCT n.nspname || '.' || c.relname, l.mode NOT IN ('AccessShareLock', 'RowShareLock')
      FROM pg_locks l
      JOIN pg_class c0 ON c0.oid = l.relation
      JOIN pg_class c ON c.oid = coalesce(pg_partition_root(c0.oid), c0.oid)
      JOIN pg_namespace n ON n.oid = c.relnamespace
      WHERE l.pid = pg_backend_pid() AND l.locktype = 'relation' AND l.granted
        AND c0.relkind IN ('r', 'p') AND n.nspname <> 'information_schema' AND n.nspname !~ '^pg_'|}
;;

let track ?(writes = false) ((module C : Caqti_goo.CONNECTION) as conn) f =
  let ( let* ) = Result.bind in
  let* () = C.start () in
  match
    let* v = f conn in
    let* held = C.collect_list locks () in
    Ok (v, held)
  with
  | Ok (v, held) ->
    let tables = List.sort_uniq String.compare (List.map fst held) in
    let wrote = List.sort_uniq String.compare (List.filter_map (fun (t, w) -> if w then Some t else None) held) in
    if wrote <> [] && not writes then begin
      ignore (C.rollback ());
      raise (Wrote wrote)
    end
    else
      let* () = C.commit () in
      Ok (v, tables)
  | Error _ as e ->
    ignore (C.rollback ());
    e
  | exception e ->
    ignore (C.rollback ());
    raise e
;;
