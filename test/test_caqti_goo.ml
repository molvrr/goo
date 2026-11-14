(* Caqti_goo against a throwaway Postgres cluster (initdb/postgres must be on
   the PATH, as in the dev shell; skipped otherwise). Narrates like the other
   Goo tests: a line containing "unexpected" fails the run. *)

module R = Goo
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
      Printf.printf "[%.3f d%d] %s\n%!" (Unix.gettimeofday () -. t0)
        (try R.index (R.self ()) with Invalid_argument _ -> -1)
        s)
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
  (* Unix socket paths are short on macOS: stay under /tmp. *)
  let dir = Printf.sprintf "/tmp/caqti_goo_%d" (Unix.getpid ()) in
  let data = Filename.concat dir "data" in
  Unix.mkdir dir 0o700;
  (match Unix.waitpid [] (run_quiet "initdb" [ "-D"; data; "-A"; "trust"; "-U"; "postgres"; "--no-sync" ]) with
  | _, WEXITED 0 -> ()
  | _ -> failwith "initdb failed");
  let pid =
    run_quiet "postgres"
      [ "-D"; data; "-k"; dir; "-c"; "listen_addresses="; "-c"; "fsync=off"; "-c"; "max_connections=50" ]
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

(* {1 Queries} *)

open Caqti_request.Infix

let add = Caqti_type.(t2 int int ->! int) "SELECT ? + ?"
let sleep = Caqti_type.(float ->! int) "SELECT 1 FROM pg_sleep(?)"
let backend_pid = Caqti_type.(unit ->! int) "SELECT pg_backend_pid()"

let or_fail = function Ok x -> x | Error e -> failwith (Caqti_error.show e)
let pg_sleep db secs = or_fail (DP.use db (fun (module C : Caqti_goo.CONNECTION) -> C.find sleep secs)) |> ignore
let pid db = or_fail (DP.use db (fun (module C : Caqti_goo.CONNECTION) -> C.find backend_pid ()))

let time f =
  let t = Unix.gettimeofday () in
  f ();
  Unix.gettimeofday () -. t

(* Counts how often a task on the same domain gets to run meanwhile. *)
let with_ticker f =
  let ticks = ref 0 in
  let s = R.scope [ (fun () -> while true do R.sleep 0.01; incr ticks done) ] in
  let v = f () in
  R.cancel s;
  R.join s;
  (v, !ticks)

let pool_config n = Caqti_pool_config.create ~max_size:n ()

let tests uri =
  let d1 = (R.domains ()).(1) in

  log "-- a query";
  let db = DP.create ~pool_config:(pool_config 4) uri in
  let sum = or_fail (DP.use db (fun (module C : Caqti_goo.CONNECTION) -> C.find add (40, 2))) in
  expect (Printf.sprintf "40 + 2 = %d" sum) (sum = 42);

  log "-- waiting on Postgres doesn't block the domain";
  let elapsed, ticks =
    with_ticker (fun () -> time (fun () -> R.scoped (List.init 4 (fun _ () -> pg_sleep db 0.3))))
  in
  log "4 x pg_sleep(0.3) on 4 connections: %.2fs, ticker ran %d times" elapsed ticks;
  expect "concurrent" (elapsed < 0.6);
  expect "ticker kept running" (ticks >= 15);

  log "-- max_size bounds the connections: queries queue for one";
  let small = DP.create ~pool_config:(pool_config 2) uri in
  let elapsed = time (fun () -> R.scoped (List.init 6 (fun _ () -> pg_sleep small 0.2))) in
  log "6 x pg_sleep(0.2) on 2 connections: %.2fs" elapsed;
  expect "three rounds" (elapsed >= 0.55 && elapsed < 1.0);
  expect "two connections" (Caqti_goo.Pool.size (DP.get small) = 2);
  DP.close small;

  log "-- each domain has its own pool";
  let here = pid db and there = R.await_on d1 (fun () -> pid db) in
  expect "different backends" (here <> there);
  let there_again = R.await_on d1 (fun () -> pid db) in
  expect "d1 reuses its connection" (there = there_again);
  let size_d0 = Caqti_goo.Pool.size (DP.get db) in
  let size_d1 = R.await_on d1 (fun () -> Caqti_goo.Pool.size (DP.get db)) in
  log "pool sizes: d0 %d, d1 %d" size_d0 size_d1;

  log "-- cancelling many tasks waiting for a connection doesn't break the pool";
  let conns () =
    or_fail (DP.use db (fun (module C : Caqti_goo.CONNECTION) ->
      C.find Caqti_request.Infix.(Caqti_type.(unit ->! int) "SELECT count(*)::int FROM pg_stat_activity WHERE backend_type = 'client backend'") ()))
  in
  let before = conns () in
  let small = DP.create ~pool_config:(pool_config 2) uri in
  let finished = Atomic.make 0 in
  let n = 200 in
  let s =
    R.scope
      (List.init n (fun _ () ->
           Fun.protect ~finally:(fun () -> Atomic.incr finished) (fun () ->
               while true do
                 ignore (DP.use small (fun (module C : Caqti_goo.CONNECTION) -> C.find sleep 0.01));
                 R.check ()
               done)))
  in
  R.sleep 0.3;
  R.cancel s;
  let t = Unix.gettimeofday () in
  while Atomic.get finished < n && Unix.gettimeofday () -. t < 10. do R.sleep 0.05 done;
  expect (Printf.sprintf "%d/%d cancelled tasks finished" (Atomic.get finished) n) (Atomic.get finished = n);
  let after = conns () in
  expect (Printf.sprintf "server connections %d -> %d (no connection left behind)" before after) (after <= before + 4);
  if Atomic.get finished = n then begin
    R.join s;
    let ok = Result.is_ok (DP.use small (fun (module C : Caqti_goo.CONNECTION) -> C.find add (1, 1))) in
    expect "the pool still serves" ok
  end;
  DP.close small;

  log "-- both domains query at once";
  let elapsed =
    time (fun () ->
        R.scoped
          [
            (fun () -> R.scoped (List.init 3 (fun _ () -> pg_sleep db 0.3)));
            (fun () -> R.await_on d1 (fun () -> R.scoped (List.init 3 (fun _ () -> pg_sleep db 0.3))));
          ])
  in
  log "3 + 3 x pg_sleep(0.3): %.2fs" elapsed;
  expect "concurrent across domains" (elapsed < 0.6);

  log "-- a cancelled query";
  let s = R.scope [ (fun () -> pg_sleep db 5.) ] in
  R.sleep 0.1;
  let elapsed = time (fun () -> R.cancel s; R.join s) in
  expect (Printf.sprintf "cancelled in %.3fs" elapsed) (elapsed < 0.5);
  (* The pool must not hand out a connection with that query still running. *)
  let results = List.init 8 (fun i -> or_fail (DP.use db (fun (module C : Caqti_goo.CONNECTION) -> C.find add (i, 1)))) in
  expect "pool still answers correctly" (results = List.init 8 (fun i -> i + 1));

  log "-- a cancelled query, on the pool's only connection";
  let one = DP.create ~pool_config:(pool_config 1) uri in
  let before = pid one in
  let s = R.scope [ (fun () -> pg_sleep one 5.) ] in
  R.sleep 0.1;
  R.cancel s;
  R.join s;
  let elapsed, r =
    let t = Unix.gettimeofday () in
    let r = DP.use one (fun (module C : Caqti_goo.CONNECTION) -> C.find add (1, 1)) in
    (Unix.gettimeofday () -. t, r)
  in
  (match r with
  | Ok 2 -> log "next query answered in %.3fs" elapsed
  | Ok n -> log "next query answered %d (unexpected)" n
  | Error e -> log "next query failed (unexpected): %s" (Caqti_error.show e));
  let after = pid one in
  log "connection %s" (if before = after then "reused" else "replaced");
  DP.close one;

  DP.close db;
  log "-- after close";
  match DP.get db with
  | _ -> log "get after close (unexpected)"
  | exception Invalid_argument _ -> log "get raises Invalid_argument"

let () =
  if not (on_path "initdb" && on_path "postgres") then print_endline "initdb/postgres not on PATH: skipped"
  else begin
    ignore (Unix.alarm 60);
    let uri, stop = start_postgres () in
    Fun.protect ~finally:stop (fun () ->
        try R.run_main ~domains:2 (fun () -> tests uri)
        with e -> log "failed: %s (unexpected)" (Printexc.to_string e));
    if Atomic.get unexpected then exit 1
  end
