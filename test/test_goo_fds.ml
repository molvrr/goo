(* The kernel poller (epoll / kqueue): more fds than select allows, fds
   closed behind Goo's back and reused, regular files, cancellation. Under
   GOO_POLLER=select the many-fds part is skipped. Narrates like the other
   Goo tests: a line containing "unexpected" fails the run. *)

let unexpected = ref false

let expect what ok =
  if not ok then unexpected := true;
  Printf.printf "%s: %s\n%!" what (if ok then "ok" else "unexpected")

(* Waits for [n] arrivals on [ch] or [secs]: how many came. *)
let count_within ch n secs =
  let got = ref 0 in
  let s = Goo.scope [ (fun () -> while !got < n do Goo.Chan.recv ch; incr got done) ] in
  let t = Unix.gettimeofday () in
  while !got < n && Unix.gettimeofday () -. t < secs do Goo.sleep 0.01 done;
  Goo.cancel s;
  Goo.join s;
  !got

let pair () =
  let a, b = Unix.socketpair ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.set_nonblock a;
  Unix.set_nonblock b;
  (a, b)

let rec pairs n acc =
  if n = 0 then Ok (List.rev acc)
  else match pair () with p -> pairs (n - 1) (p :: acc) | exception Unix.Unix_error (EMFILE, _, _) -> Error (List.length acc)

let () =
  Goo.run (fun () ->
      let backend = Goo.io_backend () in
      Printf.printf "backend: %s\n%!"
        (match backend with `Epoll -> "epoll" | `Kqueue -> "kqueue" | `Select -> "select");

      print_endline "-- many fds at once";
      (match if backend = `Select then None else Some (pairs 3000 []) with
      | None -> print_endline "select: skipped (fds past FD_SETSIZE)"
      | Some (Error n) -> Printf.printf "only %d socket pairs (ulimit -n?): skipped\n" n
      | Some (Ok ps) ->
          let n = List.length ps in
          let ch = Goo.Chan.create () in
          let s =
            Goo.scope
              (List.map
                 (fun (a, _) () ->
                   let buf = Bytes.create 1 in
                   if Goo.read a buf 0 1 = 1 then Goo.Chan.send ch ())
                 ps)
          in
          Goo.sleep 0.1;
          List.iter (fun (_, b) -> ignore (Unix.write_substring b "x" 0 1)) ps;
          let got = count_within ch n 10. in
          expect (Printf.sprintf "%d readers on %d fds all woke (%d)" n (2 * n) got) (got = n);
          Goo.cancel s;
          Goo.join s;
          List.iter (fun (a, b) -> Unix.close a; Unix.close b) ps);

      print_endline "-- an fd closed behind Goo's back, its number reused";
      let a, b = pair () in
      let s = Goo.scope [ (fun () -> Goo.await_readable a) ] in
      Goo.sleep 0.05;
      Goo.cancel s;
      Goo.join s;
      let old = (Obj.magic a : int) in
      Unix.close a;
      Unix.close b;
      let a', b' = pair () in
      Printf.printf "old fd %d, new fd %d\n" old (Obj.magic a' : int);
      let ch = Goo.Chan.create () in
      let s = Goo.scope [ (fun () -> Goo.await_readable a'; Goo.Chan.send ch ()) ] in
      Goo.sleep 0.05;
      ignore (Unix.write_substring b' "x" 0 1);
      expect "the new fd's reader wakes" (count_within ch 1 2. = 1);
      Goo.cancel s;
      Goo.join s;
      Unix.close a';
      Unix.close b';

      print_endline "-- a regular file";
      let path = Filename.temp_file "goo" ".txt" in
      let fd = Unix.openfile path [ O_RDONLY ] 0 in
      let ch = Goo.Chan.create () in
      let s = Goo.scope [ (fun () -> Goo.await_readable fd; Goo.Chan.send ch ()) ] in
      expect "awaiting a regular file returns" (count_within ch 1 2. = 1);
      Goo.cancel s;
      Goo.join s;
      Unix.close fd;
      Sys.remove path;

      print_endline "-- cancelled while waiting, then data: nobody left to wake";
      let a, b = pair () in
      let s = Goo.scope [ (fun () -> Goo.await_readable a) ] in
      Goo.sleep 0.05;
      Goo.cancel s;
      Goo.join s;
      ignore (Unix.write_substring b "x" 0 1);
      let cpu () = let t = Unix.times () in t.tms_utime +. t.tms_stime in
      let c = cpu () in
      Goo.sleep 0.5;
      let used = cpu () -. c in
      Printf.printf "cpu while idle for 0.5s: %.3fs\n" used;
      expect "the loop doesn't spin on the ready fd" (used < 0.1);
      let ch = Goo.Chan.create () in
      let s = Goo.scope [ (fun () -> Goo.await_readable a; Goo.Chan.send ch ()) ] in
      expect "a new reader still wakes" (count_within ch 1 2. = 1);
      Goo.cancel s;
      Goo.join s;
      Goo.close a;
      Unix.close b);
  if !unexpected then exit 1
