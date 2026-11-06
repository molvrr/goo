module R = Goo

let t0 = Unix.gettimeofday ()
let log_line fmt =
  Printf.ksprintf
    (fun s ->
      Printf.printf "[%.3f d%d] %s\n%!" (Unix.gettimeofday () -. t0)
        (try R.index (R.self ()) with Invalid_argument _ -> -1) s)
    fmt

(* These tests narrate what happens; a line containing "unexpected" marks a
   failure, and the test exits 1 at the end. *)
let unexpected = Atomic.make false

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

let log fmt =
  Printf.ksprintf
    (fun s ->
      if contains s "unexpected" then Atomic.set unexpected true;
      log_line "%s" s)
    fmt

let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

let section name f =
  log "-- %s" name;
  f ()

(* Domain 0 drives the tests; domain 1's main returns at once, leaving it
   idle and blocked on its wake pipe until work is posted to it. *)
let tests () =
  let d = R.domains () in
  let d1 = d.(1) in

  section "await_on wakes an idle domain and returns its result" (fun () ->
      let t = Unix.gettimeofday () in
      let i, v = R.await_on d1 (fun () -> (R.index (R.self ()), 6 * 7)) in
      log "got %d from domain %d in %.2fms" v i ((Unix.gettimeofday () -. t) *. 1000.));

  section "await_on re-raises the remote exception" (fun () ->
      match R.await_on d1 (fun () -> failwith "remote boom") with
      | () -> log "unexpected"
      | exception Failure m -> log "caught Failure %S" m);

  section "CPU work runs in parallel" (fun () ->
      let n = 36 in
      let t = Unix.gettimeofday () in
      ignore (fib n);
      ignore (fib n);
      let seq = Unix.gettimeofday () -. t in
      let t = Unix.gettimeofday () in
      let s = R.scope [ (fun () -> ignore (R.await_on d1 (fun () -> fib n))); (fun () -> ignore (fib n)) ] in
      R.join s;
      let par = Unix.gettimeofday () -. t in
      log "2x fib %d: sequential %.0fms, parallel %.0fms (%.1fx)" n (seq *. 1000.) (par *. 1000.) (seq /. par));

  section "cancelling the caller cancels remote work and waits for it" (fun () ->
      let s =
        R.scope
          [
            (fun () ->
              match
                R.await_on d1 (fun () ->
                    R.defer (fun () -> log "remote defer ran");
                    R.sleep 3600.)
              with
              | () -> log "unexpected"
              | exception R.Cancelled -> log "caller got Cancelled after remote wound down");
          ]
      in
      R.sleep 0.02;
      R.cancel s;
      R.join s);

  section "channel between domains with backpressure" (fun () ->
      let c = R.Chan.create ~capacity:16 () in
      let n = 100_000 in
      R.post d1 (fun () ->
          for i = 1 to n do
            R.Chan.send c i
          done;
          R.Chan.close c);
      let t = Unix.gettimeofday () in
      let rec sum acc = match R.Chan.recv c with v -> sum (acc + v) | exception R.Closed -> acc in
      let total = sum 0 in
      log "sum %d (expected %d), %.0f msgs/ms" total (n * (n + 1) / 2)
        (float n /. ((Unix.gettimeofday () -. t) *. 1000.)));

  section "rendezvous channel (capacity 0)" (fun () ->
      let c = R.Chan.create ~capacity:0 () in
      R.post d1 (fun () ->
          R.Chan.send c "hello";
          log "send returned after the receiver took it");
      R.sleep 0.02;
      log "received %S" (R.Chan.recv c));

  section "cancel and join a scope owned by another domain" (fun () ->
      let handoff = R.Chan.create () in
      R.post d1 (fun () ->
          let s =
            R.scope [ (fun () -> R.defer (fun () -> log "remote task cleaned up"); R.sleep 3600.) ]
          in
          R.Chan.send handoff s);
      let s = R.Chan.recv handoff in
      R.cancel s;
      R.join s;
      log "joined remote scope");

  section "spawn_in from another domain runs on the scope's domain" (fun () ->
      (* A scope on d1, kept alive until [stop]. *)
      let stop = R.Chan.create () and handoff = R.Chan.create () in
      R.post d1 (fun () -> R.Chan.send handoff (R.scope [ (fun () -> R.Chan.recv stop) ]));
      let s = R.Chan.recv handoff in
      let ran_on = R.Chan.create () in
      R.spawn_in s (fun () -> R.Chan.send ran_on (R.index (R.self ())));
      let i = R.Chan.recv ran_on in
      if i = 1 then log "spawned task ran on domain 1"
      else log "spawned task ran on domain %d (unexpected)" i;
      R.Chan.send stop ();
      R.join s;
      log "spawn_in to the finished scope is dropped (logged on stderr):";
      R.spawn_in s (fun () -> log "ran (unexpected)");
      R.sleep 0.05);

  section "posted failures are logged, not fatal" (fun () ->
      R.post d1 (fun () -> failwith "posted boom");
      R.sleep 0.02;
      log "still running");

  section "cancelled receiver doesn't lose a message" (fun () ->
      let c = R.Chan.create () in
      let s = R.scope [ (fun () -> ignore (R.Chan.recv c)) ] in
      R.sleep 0.01;
      R.cancel s;
      R.join s;
      R.Chan.send c 1;
      log "message still there: %d" (R.Chan.recv c))

let () =
  R.run_parallel ~domains:2 (fun i -> if i = 0 then tests ());
  log "run_parallel returned after all work finished";
  log "-- a failure in one main cancels the others";
  match
    R.run_parallel ~domains:2 (fun i ->
        if i = 0 then
          Fun.protect ~finally:(fun () -> log "main 0 unwound") (fun () -> R.sleep 3600.)
        else begin
          R.sleep 0.02;
          failwith "main 1 boom"
        end)
  with
  | () -> log "unexpected"
  | exception Failure m -> log "run_parallel raised Failure %S" m

let () = if Atomic.get unexpected then exit 1
