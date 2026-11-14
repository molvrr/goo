(* Live against a throwaway Postgres cluster (initdb/postgres must be on
   the PATH, as in the dev shell; skipped otherwise). Narrates like the other
   Goo tests: a line containing "unexpected" fails the run. *)

module DP = Caqti_goo.Domain_pool

let t0 = Unix.gettimeofday ()
let unexpected = Atomic.make false

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

let log fmt =
  Printf.ksprintf
    (fun s ->
      if contains s "unexpected" then Atomic.set unexpected true;
      Printf.printf "[%.3f] %s\n%!" (Unix.gettimeofday () -. t0) s)
    fmt

let expect what ok = log "%s: %s" what (if ok then "ok" else "unexpected")

(* {1 Postgres} *)

let on_path exe =
  List.exists
    (fun d -> Sys.file_exists (Filename.concat d exe))
    (String.split_on_char ':' (try Sys.getenv "PATH" with Not_found -> ""))

let run_quiet prog args =
  let null = Unix.openfile "/dev/null" [ O_WRONLY ] 0 in
  let pid = Unix.create_process prog (Array.of_list (prog :: args)) Unix.stdin null null in
  Unix.close null;
  pid

let start_postgres () =
  let dir = Printf.sprintf "/tmp/live_%d" (Unix.getpid ()) in
  let data = Filename.concat dir "data" in
  Unix.mkdir dir 0o700;
  (match Unix.waitpid [] (run_quiet "initdb" [ "-D"; data; "-A"; "trust"; "-U"; "postgres"; "--no-sync" ]) with
  | _, WEXITED 0 -> ()
  | _ -> failwith "initdb failed");
  let pid =
    run_quiet "postgres" [ "-D"; data; "-k"; dir; "-c"; "listen_addresses="; "-c"; "fsync=off"; "-c"; "max_connections=50" ]
  in
  let sock = Filename.concat dir ".s.PGSQL.5432" in
  let rec wait n =
    if not (Sys.file_exists sock) then
      if n = 0 then failwith "postgres didn't start"
      else begin
        Unix.sleepf 0.05;
        wait (n - 1)
      end
  in
  wait 200;
  let stop () =
    Unix.kill pid Sys.sigquit;
    ignore (Unix.waitpid [] pid);
    ignore (Sys.command ("rm -rf " ^ Filename.quote dir))
  in
  (Uri.of_string (Printf.sprintf "postgresql:///postgres?host=%s&user=postgres" dir), stop)

(* {1 Helpers} *)

let or_fail = function Ok x -> x | Error e -> failwith (Caqti_error.show e)
let lit sql = Caqti_request.create ~oneshot:true Caqti_type.unit Caqti_type.unit Caqti_mult.zero (fun _ -> Caqti_query.L sql)
let sql db s = or_fail (DP.use db (fun (module C : Caqti_goo.CONNECTION) -> C.exec (lit s) ()))

let triggers db =
  let q =
    Caqti_request.Infix.(
      Caqti_type.(unit ->* t2 string string)
        "SELECT c.relname::text, t.tgname::text FROM pg_trigger t JOIN pg_class c ON c.oid = t.tgrelid \
         WHERE NOT t.tgisinternal ORDER BY 1, 2")
  in
  or_fail (DP.use db (fun (module C : Caqti_goo.CONNECTION) -> C.collect_list q ()))
  |> List.map (fun (t, n) -> t ^ "." ^ n)
  |> String.concat " "

(* Whether [sub] wakes within [secs]. *)
let wakes ?(secs = 2.) sub =
  let result = Goo.Chan.create () in
  let s =
    Goo.scope
      [ (fun () -> Live.Changes.wait sub; Goo.Chan.send result true);
        (fun () -> Goo.sleep secs; Goo.Chan.send result false) ]
  in
  let woke = Goo.Chan.recv result in
  Goo.cancel s;
  Goo.join s;
  woke

let watching live ?keys table =
  let ch = Live.changes live in
  let sub = Live.Changes.subscribe ch in
  Live.Changes.watch ch sub ~since:(Live.Changes.now ch) ?keys [ table ];
  sub

let rec until ?(secs = 15.) f =
  if f () then true else if secs <= 0. then false else (Goo.sleep 0.2; until ~secs:(secs -. 0.2) f)

(* {1 Tests} *)

let tests uri =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  let db = DP.create uri in
  sql db "CREATE TABLE a (id int PRIMARY KEY, v text)";
  sql db "CREATE TABLE b (id int PRIMARY KEY, v text)";
  sql db "CREATE TABLE c (id int PRIMARY KEY)";
  sql db "CREATE TABLE d (id serial PRIMARY KEY)";
  sql db "CREATE TABLE hidden (id int)";
  sql db "CREATE TABLE p (id int, v text) PARTITION BY RANGE (id)";
  sql db "CREATE TABLE p1 PARTITION OF p FOR VALUES FROM (0) TO (100)";
  sql db "CREATE TABLE p2 PARTITION OF p FOR VALUES FROM (100) TO (200)";
  sql db "CREATE TABLE q (id int) PARTITION BY LIST (id)";
  sql db "CREATE TABLE q1 PARTITION OF q FOR VALUES IN (1)";

  log "-- Reads.track";
  let q = Caqti_request.Infix.(Caqti_type.(unit ->* int) "SELECT id FROM a UNION ALL SELECT id FROM b") in
  let _, read =
    or_fail (DP.use db (fun conn -> Live.Reads.track conn (fun (module C : Caqti_goo.CONNECTION) -> C.collect_list q ())))
  in
  expect ("read " ^ String.concat "," read) (read = [ "public.a"; "public.b" ]);
  let qp = Caqti_request.Infix.(Caqti_type.(unit ->* int) "SELECT id FROM p WHERE id >= 0") in
  let _, read =
    or_fail (DP.use db (fun conn -> Live.Reads.track conn (fun (module C : Caqti_goo.CONNECTION) -> C.collect_list qp ())))
  in
  expect ("partitions read as their root: " ^ String.concat "," read) (read = [ "public.p" ]);
  sql db "CREATE VIEW ab AS SELECT a.id FROM a JOIN b USING (id)";
  let qv = Caqti_request.Infix.(Caqti_type.(unit ->* int) "SELECT id FROM ab") in
  let _, read =
    or_fail (DP.use db (fun conn -> Live.Reads.track conn (fun (module C : Caqti_goo.CONNECTION) -> C.collect_list qv ())))
  in
  expect ("a view reads its tables: " ^ String.concat "," read) (read = [ "public.a"; "public.b" ]);
  let w = Caqti_request.Infix.(Caqti_type.(unit ->. unit) "INSERT INTO a VALUES (99, 'w')") in
  let wrote =
    match DP.use db (fun conn -> Live.Reads.track conn (fun (module C : Caqti_goo.CONNECTION) -> C.exec w ())) with
    | exception Live.Reads.Wrote ts -> ts
    | _ -> []
  in
  expect "a write in a tracked render raises Wrote" (wrote = [ "public.a" ]);
  let vs = Caqti_request.Infix.(Caqti_type.(unit ->* int) "SELECT id FROM a WHERE id = 99") in
  expect "...and isn't there" (or_fail (DP.use db (fun (module C : Caqti_goo.CONNECTION) -> C.collect_list vs ())) = []);

  log "-- Changes with many subscribers on one table";
  let ch = Live.Changes.create () in
  let n = 10_000 in
  let subs = List.init n (fun _ -> Live.Changes.subscribe ch) in
  let t0 = Unix.gettimeofday () in
  (* What a render does: watch again, after every change. *)
  for _ = 1 to 3 do
    List.iter (fun s -> Live.Changes.watch ch s ~since:(Live.Changes.now ch) [ "public.x"; "public.y" ]) subs
  done;
  let dt = Unix.gettimeofday () -. t0 in
  log "%d subscribers x 3 re-watches: %.3fs" n dt;
  expect "re-watching is cheap (not quadratic)" (dt < 1.);
  expect "all watching" (Live.Changes.watching ch "public.x" = n);
  List.iter (fun s -> Live.Changes.watch ch s ~since:(Live.Changes.now ch) [ "public.y" ]) subs;
  expect "moved off a table" (Live.Changes.watching ch "public.x" = 0 && Live.Changes.watching ch "public.y" = n);
  List.iter (Live.Changes.unsubscribe ch) subs;
  expect "unsubscribing removes them all" (Live.Changes.subscribers ch = 0 && Live.Changes.watching ch "public.y" = 0);

  log "-- a column that doesn't exist";
  let bad = Live.create ~db ~url:uri Live.[ table "a" ~keys:[ "nope" ] ] in
  expect "start fails" (match Live.start bad with () -> false | exception Failure m -> contains m "no column nope");
  expect "nothing installed" (triggers db = "");

  log "-- install from the list";
  let live =
    Live.create ~db ~url:uri ~poll_every:0.2 ~max_writes_per_s:5. ~burst_window:1. ~calm_window:2. ~on_unwatched:`Fail
      Live.[ table "a" ~mode:`Notify; table "b" ~keys:[ "id" ] ~mode:`Notify; table "c" ~mode:`Poll; table "d";
             table "p" ~keys:[ "id" ] ~mode:`Notify; table "q" ~mode:`Poll ]
  in
  Live.start live;
  expect ("triggers " ^ triggers db)
    (triggers db
     = "a.live b.live_delete b.live_insert b.live_truncate b.live_update d.live p.live_delete p.live_insert p.live_truncate p.live_update");
  expect "listening" (until (fun () -> Live.connected live));

  log "-- notify";
  let sub = watching live "public.a" in
  expect "nothing yet" (not (wakes ~secs:0.5 sub));
  sql db "INSERT INTO a VALUES (1, 'x')";
  expect "insert into a wakes" (wakes sub);
  sql db "UPDATE a SET v = 'x' WHERE false";
  expect "...a statement touching nothing wakes too (bare)" (wakes sub);

  log "-- notify by key";
  let sub = watching live ~keys:[ ("public.b", "id", "1") ] "public.b" in
  sql db "INSERT INTO b VALUES (2, 'x')";
  expect "another row doesn't wake" (not (wakes ~secs:0.5 sub));
  sql db "INSERT INTO b VALUES (1, 'x')";
  expect "its row wakes" (wakes sub);
  sql db "UPDATE b SET v = 'y' WHERE id = 3";
  expect "an update touching no row doesn't" (not (wakes ~secs:0.5 sub));

  log "-- partitioned, by key, written through the parent";
  let sub = watching live ~keys:[ ("public.p", "id", "150") ] "public.p" in
  sql db "INSERT INTO p VALUES (5, 'x')";
  expect "another row doesn't wake" (not (wakes ~secs:0.5 sub));
  sql db "INSERT INTO p VALUES (150, 'x')";
  expect "its row (in p2) wakes" (wakes sub);

  log "-- poll";
  let sub = watching live "public.c" in
  expect "nothing yet" (not (wakes ~secs:1. sub));
  let t = Unix.gettimeofday () in
  sql db "INSERT INTO c VALUES (1)";
  expect "insert into c wakes (counters)" (wakes ~secs:15. sub);
  log "...after %.1fs" (Unix.gettimeofday () -. t);

  let sub = watching live "public.q" in
  expect "partitioned, polled: nothing yet" (not (wakes ~secs:1. sub));
  sql db "INSERT INTO q1 VALUES (1)";
  expect "a write straight into the partition wakes the root" (wakes ~secs:15. sub);

  log "-- auto: too many writes, then calm";
  let stop = Atomic.make false in
  let writer = Goo.scope [ (fun () -> while not (Atomic.get stop) do sql db "INSERT INTO d DEFAULT VALUES"; Goo.sleep 0.01 done) ] in
  let t = Unix.gettimeofday () in
  expect "d moves to Poll" (until (fun () -> List.assoc "public.d" (Live.modes live) = Live.Poll));
  log "...after %.1fs" (Unix.gettimeofday () -. t);
  expect "...its trigger is gone" (not (contains (triggers db) "d.live"));
  Atomic.set stop true;
  Goo.join writer;
  let t = Unix.gettimeofday () in
  expect "back to Notify once calm" (until ~secs:30. (fun () -> List.assoc "public.d" (Live.modes live) = Live.Notify));
  log "...after %.1fs" (Unix.gettimeofday () -. t);
  expect "...trigger again" (contains (triggers db) "d.live");

  log "-- reconcile: a shorter list removes triggers";
  Live.stop live;
  let live2 = Live.create ~db ~url:uri Live.[ table "b" ~mode:`Notify ] in
  Live.start live2;
  expect "only b, plain now" (triggers db = "b.live");
  Live.stop live2;

  log "-- pages";
  let live = Live.create ~db ~url:uri ~on_unwatched:`Fail Live.[ table "a" ~mode:`Notify ] in
  Live.start live;
  ignore (until (fun () -> Live.connected live));
  let rows = Caqti_request.Infix.(Caqti_type.(unit ->* string) "SELECT v FROM a ORDER BY id") in
  let data _req (module C : Caqti_goo.CONNECTION) =
    Result.map (List.map (fun v -> Html.p [] [ Html.text v ])) (C.collect_list rows ())
  in
  let shell _req part = Html.(document (head [] [ title [] [ text "t" ] ]) (body [] [ part ])) in
  let r = Live.page live ~shell ~data (Web.Request.make "/a") in
  let body = Web.Response.body_to_string r in
  expect "GET renders shell and data" (contains body "<title>t</title>" && contains body "<p>x</p>" && contains body "data-init");
  let hidden _req (module C : Caqti_goo.CONNECTION) =
    Result.map (fun _ -> []) (C.collect_list Caqti_request.Infix.(Caqti_type.(unit ->* int) "SELECT id FROM hidden") ())
  in
  let r = Live.page live ~shell ~data:hidden (Web.Request.make "/hidden") in
  expect "reading an unwatched table: 500 with on_unwatched:`Fail"
    (Httpun.Status.to_code (Web.Response.status r) = 500);
  let r = Live.page live ~tables:[ "a" ] ~shell ~data (Web.Request.make "/a") in
  expect "~tables: GET renders without tracking" (contains (Web.Response.body_to_string r) "<p>x</p>");
  let r = Live.page live ~tables:[ "a"; "hidden" ] ~shell ~data (Web.Request.make "/a") in
  expect "~tables naming an unwatched table: 500 with on_unwatched:`Fail" (Httpun.Status.to_code (Web.Response.status r) = 500);
  let checked = Live.create ~db ~url:uri ~on_unwatched:`Fail ~check_tables:true Live.[ table "a" ~mode:`Notify; table "hidden" ] in
  let both _req (module C : Caqti_goo.CONNECTION) =
    Result.bind (C.collect_list Caqti_request.Infix.(Caqti_type.(unit ->* int) "SELECT id FROM hidden") ()) (fun _ -> data _req (module C))
  in
  let r = Live.page checked ~tables:[ "a" ] ~shell ~data:both (Web.Request.make "/both") in
  expect "check_tables: a page reading a table missing from ~tables fails" (Httpun.Status.to_code (Web.Response.status r) = 500);
  let r = Live.page checked ~tables:[ "a"; "hidden" ] ~shell ~data:both (Web.Request.make "/both") in
  expect "...and passes once listed" (Httpun.Status.to_code (Web.Response.status r) = 200);
  let writes _req (module C : Caqti_goo.CONNECTION) =
    Result.map (fun () -> []) (C.exec Caqti_request.Infix.(Caqti_type.(unit ->. unit) "INSERT INTO a VALUES (77, 'w')") ())
  in
  let r = Live.page live ~shell ~data:writes (Web.Request.make "/w") in
  let rows = Caqti_request.Infix.(Caqti_type.(unit ->* int) "SELECT id FROM a WHERE id = 77") in
  expect "a render that writes: 500, rolled back"
    (Httpun.Status.to_code (Web.Response.status r) = 500 && or_fail (DP.use db (fun (module C : Caqti_goo.CONNECTION) -> C.collect_list rows ())) = []);
  let events2 = Goo.Chan.create () in
  let s2 =
    Goo.scope
      [ (fun () -> Live.stream live ~id:"live" ~tables:[ "a" ] ~data (Web.Request.make "/a") (fun e -> Goo.Chan.send events2 e)) ]
  in
  ignore (Goo.Chan.recv events2);
  sql db "INSERT INTO a VALUES (3, 'via tables')";
  expect "~tables: the stream updates after a write" (contains (String.concat "" (List.map snd (Goo.Chan.recv events2))) "via tables");
  Goo.cancel s2;
  Goo.join s2;
  let events = Goo.Chan.create () in
  let s =
    Goo.scope [ (fun () -> Live.stream live ~id:"live" ~data (Web.Request.make "/a") (fun e -> Goo.Chan.send events e)) ]
  in
  let first = Goo.Chan.recv events in
  expect "stream sends the data" (contains (String.concat "" (List.map snd first)) "<p>x</p>");
  sql db "INSERT INTO a VALUES (2, 'novo')";
  let second = Goo.Chan.recv events in
  expect "...and again after a write" (contains (String.concat "" (List.map snd second)) "<p>novo</p>");
  Goo.cancel s;
  Goo.join s;
  Live.stop live;
  DP.close db

let () =
  if not (on_path "initdb" && on_path "postgres") then print_endline "initdb/postgres not on the PATH: skipped"
  else begin
    let uri, stop = start_postgres () in
    Fun.protect ~finally:stop (fun () -> Goo.run_main ~domains:2 (fun () -> tests uri));
    if Atomic.get unexpected then exit 1
  end
