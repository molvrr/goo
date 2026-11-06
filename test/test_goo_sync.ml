module R = Goo
module M = Goo.Mutex
module C = Goo.Condition

let t0 = Unix.gettimeofday ()

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
      Printf.printf "[%.3f d%d] %s\n%!" (Unix.gettimeofday () -. t0)
        (try R.index (R.self ()) with Invalid_argument _ -> -1) s)
    fmt

let expect what ok = if ok then log "%s: ok" what else log "%s: unexpected" what

(* Many tasks, each yielding inside the critical section: nobody else may be
   inside at the same time. *)
let hammer m ~inside ~count n () =
  for _ = 1 to n do
    M.use m (fun () ->
        if Atomic.exchange inside true then log "two tasks inside (unexpected)";
        R.yield ();
        Atomic.incr count;
        Atomic.set inside false)
  done

let test_exclusion () =
  log "-- mutual exclusion across yields";
  let m = M.create () and inside = Atomic.make false and count = Atomic.make 0 in
  R.scoped (List.init 10 (fun _ -> hammer m ~inside ~count 100));
  expect (Printf.sprintf "count = %d" (Atomic.get count)) (Atomic.get count = 1000);
  expect "free afterwards" (M.try_lock m);
  M.unlock m

let test_fifo () =
  log "-- waiters get the mutex in FIFO order";
  let m = M.create () and order = ref [] in
  M.lock m;
  let s =
    R.scope
      (List.map
         (fun name () -> M.use m (fun () -> order := name :: !order))
         [ "a"; "b"; "c" ])
  in
  R.yield ();
  expect "try_lock fails while held" (not (M.try_lock m));
  M.unlock m;
  R.join s;
  let got = String.concat "" (List.rev !order) in
  expect (Printf.sprintf "order %s" got) (got = "abc")

let test_cancel_waiter () =
  log "-- a cancelled waiter doesn't get the mutex";
  let m = M.create () and got = ref false in
  M.lock m;
  let s = R.scope [ (fun () -> M.lock m; got := true) ] in
  R.yield ();
  R.cancel s;
  R.join s;
  M.unlock m;
  expect "waiter never locked" (not !got);
  expect "free after unlock" (M.try_lock m);
  M.unlock m

let test_cancel_after_handoff () =
  log "-- cancelled after the handoff: the task holds it, finally releases it";
  let m = M.create () in
  M.lock m;
  let s =
    R.scope
      [
        (fun () ->
          M.use m (fun () ->
              log "waiter got the mutex";
              R.sleep 3600.;
              log "slept (unexpected)"));
      ]
  in
  R.yield ();
  (* Hands the mutex to the waiter, then cancels it before it runs. *)
  M.unlock m;
  R.cancel s;
  R.join s;
  expect "free after join" (M.try_lock m);
  M.unlock m

let test_unlock_unlocked () =
  log "-- unlocking a free mutex is an error";
  match M.unlock (M.create ()) with
  | () -> log "no error (unexpected)"
  | exception Invalid_argument _ -> log "Invalid_argument: ok"

let test_condition () =
  log "-- condition: producer/consumer, no lost wake-ups";
  let m = M.create () and c = C.create () and q = Queue.create () and sum = ref 0 in
  let consumer () =
    let rec loop () =
      M.lock m;
      while Queue.is_empty q do
        C.wait c m;
        if M.try_lock m then log "wait returned without the mutex (unexpected)"
      done;
      let v = Queue.pop q in
      M.unlock m;
      if v >= 0 then begin
        sum := !sum + v;
        loop ()
      end
    in
    loop ()
  in
  let producer () =
    for i = 1 to 100 do
      M.use m (fun () -> Queue.push i q; C.signal c);
      if i mod 7 = 0 then R.yield ()
    done;
    M.use m (fun () -> Queue.push (-1) q; C.signal c)
  in
  R.scoped [ consumer; producer ];
  expect (Printf.sprintf "sum = %d" !sum) (!sum = 5050)

let test_broadcast () =
  log "-- broadcast wakes every waiter";
  let m = M.create () and c = C.create () and go = ref false and woke = ref 0 in
  let waiter () =
    M.use m (fun () ->
        while not !go do C.wait c m done;
        incr woke)
  in
  let s = R.scope (List.init 5 (fun _ -> waiter)) in
  R.yield ();
  M.use m (fun () -> go := true; C.broadcast c);
  R.join s;
  expect (Printf.sprintf "%d woke" !woke) (!woke = 5)

let test_condition_cancel () =
  log "-- cancelled in wait: the mutex is held again when Cancelled is raised";
  let m = M.create () and c = C.create () in
  let s =
    R.scope
      [
        (fun () ->
          M.lock m;
          match C.wait c m with
          | () -> log "woke (unexpected)"
          | exception R.Cancelled ->
              expect "held on Cancelled" (not (M.try_lock m));
              M.unlock m);
      ]
  in
  R.yield ();
  (* The waiter released the mutex while waiting. *)
  expect "free while waiting" (M.try_lock m);
  M.unlock m;
  R.cancel s;
  R.join s;
  log "-- already cancelled before wait: the mutex is still held";
  let me = ref None in
  let s =
    R.scope
      [
        (fun () ->
          M.lock m;
          Option.iter R.cancel !me;
          match C.wait c m with
          | () -> log "waited (unexpected)"
          | exception R.Cancelled ->
              expect "still held" (not (M.try_lock m));
              M.unlock m);
      ]
  in
  me := Some s;
  R.join s;
  expect "free at the end" (M.try_lock m);
  M.unlock m

(* Both domains contend for one mutex; domain 1 also waits on a condition
   signalled from domain 0. *)
let test_cross_domain () =
  log "-- across domains";
  let d1 = (R.domains ()).(1) in
  let m = M.create () and inside = Atomic.make false and count = Atomic.make 0 in
  let remote = R.scope [ (fun () -> R.await_on d1 (fun () -> R.scoped (List.init 4 (fun _ -> hammer m ~inside ~count 200)))) ] in
  R.scoped (List.init 4 (fun _ -> hammer m ~inside ~count 200));
  R.join remote;
  expect (Printf.sprintf "count = %d" (Atomic.get count)) (Atomic.get count = 1600);
  let c = C.create () and ready = ref false and seen = ref false in
  let s =
    R.scope
      [
        (fun () ->
          R.await_on d1 (fun () ->
              M.use m (fun () ->
                  while not !ready do C.wait c m done;
                  log "woken on domain %d" (R.index (R.self ()));
                  seen := true)));
      ]
  in
  R.sleep 0.02;
  M.use m (fun () -> ready := true; C.signal c);
  R.join s;
  expect "remote waiter woke" !seen

let () =
  (* A deadlock would hang forever; SIGALRM's default action fails the test. *)
  ignore (Unix.alarm 20);
  R.run_parallel ~domains:2 (fun i ->
      if i = 0 then begin
        test_exclusion ();
        test_fifo ();
        test_cancel_waiter ();
        test_cancel_after_handoff ();
        test_unlock_unlocked ();
        test_condition ();
        test_broadcast ();
        test_condition_cancel ();
        test_cross_domain ()
      end);
  if Atomic.get unexpected then exit 1
