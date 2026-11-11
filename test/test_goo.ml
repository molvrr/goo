module R = Goo

let t0 = Unix.gettimeofday ()
let log_line fmt = Printf.ksprintf (fun s -> Printf.printf "[%.2f] %s\n%!" (Unix.gettimeofday () -. t0) s) fmt

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

(* A task that sleeps forever-ish and reports how it ended. *)
let sleeper name () =
  Fun.protect ~finally:(fun () -> log "%s: finally" name) (fun () ->
      R.sleep 3600.;
      log "%s: woke (unexpected)" name)

let test_check () =
  log "-- check: returns at once when not cancelled, without yielding";
  let other_ran = ref false in
  R.scoped
    [
      (fun () ->
        R.check ();
        log "check returned; the other task ran meanwhile: %b%s" !other_ran
          (if !other_ran then " (unexpected)" else ""));
      (fun () -> other_ran := true);
    ];
  log "-- check: raises Cancelled once the scope is cancelled";
  let entered = R.Chan.create () in
  let s =
    R.scope
      [
        (fun () ->
          R.protect (fun () ->
              R.Chan.send entered ();
              R.sleep 0.05);
          match R.check () with
          | () -> log "check returned in a cancelled scope (unexpected)"
          | exception R.Cancelled ->
              log "check raised Cancelled";
              raise R.Cancelled);
      ]
  in
  R.Chan.recv entered;
  R.cancel s;
  R.join s

(* Background work started from a request belongs to a long-lived scope: the
   request doesn't wait for it and cancelling the request doesn't reach it. *)
let test_spawn_in () =
  log "-- spawn_in: tasks belong to the target scope, not the caller's";
  let stop = R.Chan.create () in
  let bg = R.scope [ (fun () -> R.Chan.recv stop) ] in
  let started = R.Chan.create () and finished = ref false in
  let request =
    R.scope
      [
        (fun () ->
          R.spawn_in bg (fun () ->
              R.Chan.send started ();
              R.sleep 0.05;
              finished := true);
          R.sleep 3600.);
      ]
  in
  R.Chan.recv started;
  R.cancel request;
  R.join request;
  log "request joined, background running: %b" (not !finished);
  R.Chan.send stop ();
  R.join bg;
  if !finished then log "background finished after the request was cancelled"
  else log "background cut short (unexpected)";
  log "-- spawn_in: cancelling the target scope reaches its tasks";
  let bg = R.scope [ sleeper "keeper" ] in
  R.spawn_in bg (sleeper "spawned");
  R.yield ();
  R.cancel bg;
  R.join bg;
  log "-- spawn_in: failures go to the target scope";
  let bg = R.scope [ sleeper "keeper2" ] in
  R.spawn_in bg (fun () -> failwith "bg boom");
  (match R.join bg with
  | () -> log "no error (unexpected)"
  | exception Failure msg -> log "target scope raised Failure %S" msg);
  log "-- spawn_in: a finished scope is rejected";
  match R.spawn_in bg ignore with
  | () -> log "accepted (unexpected)"
  | exception Invalid_argument _ -> log "Invalid_argument"

let test_cancel () =
  log "-- cancel reaches tasks and their spawned children";
  let s =
    R.scope
      [
        sleeper "a";
        (fun () ->
          R.spawn (sleeper "b-child");
          sleeper "b" ());
      ]
  in
  R.sleep 0.05;
  R.cancel s;
  R.join s;
  log "joined after cancel"

let test_failure () =
  log "-- a failing task cancels its siblings; join re-raises";
  let s = R.scope [ (fun () -> R.sleep 0.05; failwith "boom"); sleeper "sibling" ] in
  match R.join s with
  | () -> log "unexpected: no error"
  | exception Failure msg -> log "join raised Failure %S" msg

let test_protect () =
  log "-- protect lets cleanup block after cancel";
  let s =
    R.scope
      [
        (fun () ->
          try R.sleep 3600.
          with R.Cancelled ->
            R.protect (fun () ->
                R.sleep 0.05;
                log "cleanup finished after blocking"));
      ]
  in
  R.sleep 0.01;
  R.cancel s;
  R.join s

let test_nested () =
  log "-- cancelling a parent cancels nested scopes; join waits for them";
  let outer =
    R.scope
      [
        (fun () ->
          let inner = R.scope [ sleeper "inner" ] in
          R.join inner);
      ]
  in
  R.sleep 0.01;
  R.cancel outer;
  R.join outer;
  log "outer joined"

let test_scoped () =
  log "-- scoped waits for all tasks";
  R.scoped [ (fun () -> R.sleep 0.03; log "x done"); (fun () -> R.sleep 0.01; log "y done") ];
  log "scoped returned"

let test_server () =
  log "-- cancelling a server scope stops acceptors and handlers";
  let srv = R.socket () in
  Unix.setsockopt srv Unix.SO_REUSEADDR true;
  Unix.bind srv (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen srv 16;
  let addr = Unix.getsockname srv in
  let server =
    R.scope
      (List.init 2 (fun i () ->
           let rec go () =
             let client, _ = R.accept srv in
             R.spawn (fun () ->
                 Fun.protect ~finally:(fun () -> R.close client) (fun () ->
                     let buf = Bytes.create 64 in
                     let n = R.read client buf 0 64 in
                     R.write_string client (Bytes.sub_string buf 0 n);
                     (* Hold the connection open until cancelled. *)
                     R.sleep 3600.));
             go ()
           in
           Fun.protect ~finally:(fun () -> log "acceptor %d stopped" i) go))
  in
  let s = R.socket () in
  R.connect s addr;
  R.write_string s "ping";
  let buf = Bytes.create 64 in
  let n = R.read s buf 0 64 in
  log "client got %S" (Bytes.sub_string buf 0 n);
  R.cancel server;
  R.join server;
  let n = R.read s buf 0 64 in
  log "server closed the connection (read %d)" n;
  R.close s;
  R.close srv

let test_catch () =
  log "-- a task may catch Cancelled and finish its own way";
  let s =
    R.scope
      [
        (fun () ->
          try R.sleep 3600. with R.Cancelled -> log "caught Cancelled, returning");
      ]
  in
  R.sleep 0.01;
  R.cancel s;
  R.join s;
  log "joined"

let test_kill_stubborn () =
  log "-- kill stops a task that swallows Cancelled; only defers run";
  let iterations = ref 0 in
  let s =
    R.scope
      [
        (fun () ->
          R.defer (fun () -> log "stubborn: defer ran");
          Fun.protect ~finally:(fun () -> log "stubborn: finally (unexpected)") (fun () ->
              while true do
                incr iterations;
                try R.sleep 3600. with _ -> ()
              done));
      ]
  in
  R.sleep 0.01;
  R.kill s;
  R.join s;
  log "joined after %d iterations" !iterations

let test_defer_order () =
  log "-- defers run LIFO on normal exit, may block, and run after finally";
  R.scoped
    [
      (fun () ->
        R.defer (fun () -> log "defer 1 (registered first)");
        R.defer (fun () ->
            R.sleep 0.02;
            log "defer 2 (blocked 20ms)");
        Fun.protect ~finally:(fun () -> log "finally") (fun () -> log "body done"));
    ]

let test_defer_error () =
  log "-- a failing defer fails the scope; the other defers still run";
  let s =
    R.scope
      [
        (fun () ->
          R.defer (fun () -> log "other defer ran");
          R.defer (fun () -> failwith "defer boom"));
      ]
  in
  match R.join s with
  | () -> log "unexpected: no error"
  | exception Failure m -> log "join raised Failure %S" m

let test_kill_protected () =
  log "-- kill waits for protect to finish, then drops the task";
  let s =
    R.scope
      [
        (fun () ->
          R.defer (fun () -> log "defer ran");
          R.protect (fun () ->
              R.sleep 0.03;
              log "protected section finished");
          R.sleep 3600.;
          log "after protect (unexpected)");
      ]
  in
  R.sleep 0.01;
  R.kill s;
  R.join s;
  log "joined"

let test_kill_server () =
  log "-- killing a server: handlers' deferred close still runs";
  let srv = R.socket () in
  Unix.setsockopt srv Unix.SO_REUSEADDR true;
  Unix.bind srv (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen srv 16;
  let addr = Unix.getsockname srv in
  let server =
    R.scope
      [
        (fun () ->
          R.defer (fun () -> R.close srv; log "listener closed");
          let rec go () =
            let client, _ = R.accept srv in
            R.spawn (fun () ->
                R.defer (fun () -> R.close client; log "client socket closed");
                R.sleep 3600.);
            go ()
          in
          go ());
      ]
  in
  let s = R.socket () in
  R.connect s addr;
  R.sleep 0.01;
  R.kill server;
  R.join server;
  let buf = Bytes.create 8 in
  log "peer read %d (EOF)" (R.read s buf 0 8);
  R.close s

let main () =
  test_kill_stubborn ();
  test_defer_order ();
  test_defer_error ();
  test_kill_protected ();
  test_kill_server ();
  test_catch ();
  test_cancel ();
  test_failure ();
  test_protect ();
  test_nested ();
  test_scoped ();
  test_server ();
  test_spawn_in ();
  test_check ();
  log "-- a failure in the root scope is raised from run";
  R.spawn (fun () -> failwith "root boom")

let () =
  match R.run main with
  | () -> log "run returned (unexpected)"
  | exception Failure msg -> log "run raised Failure %S" msg

let () = if Atomic.get unexpected then exit 1
