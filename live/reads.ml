(* Which tables a piece of code reads, from Postgres' own per-transaction
   counters of table scans (pg_stat_xact_user_tables): [track] runs [f] in a
   transaction between two snapshots of the counters, and the tables whose
   counts went up are the ones it read. Views, joins, subqueries and SQL
   functions are all seen through, since the counters are the tables'.

   The counters only move inside the transaction (Postgres flushes them
   when the session goes idle), so the difference is [f]'s alone. They need
   track_counts, which is on by default. A scan that finds nothing still
   counts.

   Partitions count as their root table (the one triggers and pages name),
   since a query through the parent scans the partitions.

   The transaction is READ ONLY by default: a render that writes would wake
   itself forever, so it fails instead. *)

open Caqti_request.Infix

let counters =
  Caqti_type.(unit ->* t2 string int)
    {|SELECT n.nspname || '.' || c.relname, sum(coalesce(s.seq_scan, 0) + coalesce(s.idx_scan, 0))::bigint
      FROM pg_stat_xact_user_tables s
      JOIN pg_class c ON c.oid = coalesce(pg_partition_root(s.relid), s.relid)
      JOIN pg_namespace n ON n.oid = c.relnamespace
      GROUP BY 1|}
;;

let read_only = Caqti_type.(unit ->. unit) "SET TRANSACTION READ ONLY"

let read ~before ~after =
  List.filter_map
    (fun (table, n) -> if n > Option.value (List.assoc_opt table before) ~default:0 then Some table else None)
    after
  |> List.sort_uniq String.compare
;;

let track ?(writes = false) ((module C : Caqti_goo.CONNECTION) as conn) f =
  let ( let* ) = Result.bind in
  let* () = C.start () in
  match
    let* () = if writes then Ok () else C.exec read_only () in
    let* before = C.collect_list counters () in
    let* v = f conn in
    let* after = C.collect_list counters () in
    Ok (v, read ~before ~after)
  with
  | Ok r ->
    let* () = C.commit () in
    Ok r
  | Error _ as e ->
    ignore (C.rollback ());
    e
  | exception e ->
    ignore (C.rollback ());
    raise e
;;
