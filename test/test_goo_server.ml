module R = Goo
module S = R.Server

let t0 = Unix.gettimeofday ()
let log_line fmt =
  Printf.ksprintf (fun s -> Printf.printf "[%.3f] %s\n%!" (Unix.gettimeofday () -. t0) s) fmt

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

let loopback = Unix.ADDR_INET (Unix.inet_addr_loopback, 0)
let ndomains = 4

(* Echo one message back, prefixed with the domain that served it. *)
let echo per_domain fd _peer =
  let i = R.index (R.self ()) in
  Atomic.incr per_domain.(i);
  let buf = Bytes.create 256 in
  let n = R.read fd buf 0 256 in
  R.write_string fd (Printf.sprintf "d%d:%s" i (Bytes.sub_string buf 0 n))

let request addr msg =
  let s = R.socket () in
  R.defer (fun () -> R.close s);
  R.connect s addr;
  R.write_string s msg;
  let buf = Bytes.create 256 in
  let n = R.read s buf 0 256 in
  Bytes.sub_string buf 0 n

let test_load strategy name =
  log "-- %s: 400 concurrent clients" name;
  let per_domain = Array.init ndomains (fun _ -> Atomic.make 0) in
  let server = S.start ~strategy loopback (echo per_domain) in
  let ok = Atomic.make 0 in
  R.scoped
    (List.init 400 (fun i () ->
         let msg = string_of_int i in
         let reply = request (S.addr server) msg in
         if String.ends_with ~suffix:(":" ^ msg) reply then Atomic.incr ok));
  log "%d/400 echoed; per domain: %s" (Atomic.get ok)
    (String.concat " " (Array.to_list (Array.mapi (fun i a -> Printf.sprintf "d%d=%d" i (Atomic.get a)) per_domain)));
  S.shutdown server

let test_graceful () =
  log "-- graceful shutdown waits for in-flight connections";
  let server =
    S.start loopback (fun fd _ ->
        R.sleep 0.05;
        R.write_string fd "late reply")
  in
  let addr = S.addr server in
  let client = R.scope [ (fun () -> log "client got %S" (request addr "x")) ] in
  R.sleep 0.01;
  S.shutdown server;
  log "shutdown returned";
  R.join client;
  match request addr "again" with
  | _ -> log "unexpected: still accepting"
  | exception Unix.Unix_error (e, _, _) -> log "new connection: %s" (Unix.error_message e)

let test_timeout () =
  log "-- shutdown ~timeout cancels hung connections";
  let server =
    S.start loopback (fun _ _ ->
        try R.sleep 3600. with R.Cancelled -> log "hung handler cancelled"; raise R.Cancelled)
  in
  let addr = S.addr server in
  let client =
    R.scope
      [
        (fun () ->
          (* The handler never read our request, so the close is a reset. *)
          match request addr "x" with
          | s -> log "client read %S (connection closed)" s
          | exception Unix.Unix_error (e, _, _) -> log "client: %s" (Unix.error_message e));
      ]
  in
  R.sleep 0.01;
  let t = Unix.gettimeofday () in
  S.shutdown ~timeout:0.05 server;
  log "shutdown took %.0fms" ((Unix.gettimeofday () -. t) *. 1000.);
  R.join client

let test_handler_failure () =
  log "-- a failing handler only affects its own connection";
  let server =
    S.start loopback (fun fd _ ->
        let buf = Bytes.create 16 in
        let n = R.read fd buf 0 16 in
        if Bytes.sub_string buf 0 n = "bad" then failwith "handler boom";
        R.write_string fd "fine")
  in
  let addr = S.addr server in
  log "bad request read %S" (request addr "bad");
  log "good request read %S" (request addr "good");
  S.shutdown server

let test_bind_failure () =
  log "-- bind failure is raised from start";
  let a = S.start ~strategy:`Shared loopback (fun _ _ -> ()) in
  (match S.start ~strategy:`Shared (S.addr a) (fun _ _ -> ()) with
  | _ -> log "unexpected: second bind succeeded"
  | exception Unix.Unix_error (e, _, _) -> log "second start: %s" (Unix.error_message e));
  S.shutdown a

let () =
  Printf.printf "platform: %s, %d domains\n%!" Platform.system ndomains;
  R.run_main ~domains:ndomains (fun () ->
      test_load `Auto "auto";
      test_load `Shared "shared";
      test_load `Reuseport "reuseport";
      test_graceful ();
      test_timeout ();
      test_handler_failure ();
      test_bind_failure ());
  log "run_parallel returned"

let () = if Atomic.get unexpected then exit 1
