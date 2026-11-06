(* Web.Static against a temporary tree. *)

open Web

let failures = ref 0

let check name cond =
  Printf.printf "%s %s\n%!" (if cond then "ok  " else "FAIL") name;
  if not cond then incr failures

let write path s = Out_channel.with_open_bin path (fun oc -> output_string oc s)

let tree () =
  let base = Filename.concat (Filename.get_temp_dir_name ()) (Printf.sprintf "static_%d" (Unix.getpid ())) in
  let root = Filename.concat base "public" and outside = Filename.concat base "outside" in
  List.iter (fun d -> Unix.mkdir d 0o755) [ base; root; outside; Filename.concat root "sub"; Filename.concat root "empty" ];
  write (Filename.concat root "index.html") "<h1>home</h1>";
  write (Filename.concat root "app.css") "body{color:red}";
  write (Filename.concat root "report 2024.pdf") "%PDF";
  write (Filename.concat root "digits.txt") "0123456789";
  write (Filename.concat root ".secret") "token";
  write (Filename.concat root "sub/index.html") "sub home";
  write (Filename.concat root "sub/page.html") "page";
  write (Filename.concat outside "passwd") "root:x:0:0";
  write (Filename.concat root "big.bin") (String.init 300_000 (fun i -> Char.chr (i mod 251)));
  Unix.symlink "app.css" (Filename.concat root "alias.css");
  Unix.symlink (Filename.concat outside "passwd") (Filename.concat root "link-out");
  Unix.symlink outside (Filename.concat root "dir-out");
  Unix.mkfifo (Filename.concat root "fifo") 0o644;
  (base, root)

let () =
  let base, root = tree () in
  Fun.protect ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote base))) @@ fun () ->
  let app =
    App.create ()
    |> App.route Route.(get (s "assets" /? rest @-> Static.serve root))
    |> App.route Route.(get (s "assets" / s "special" /? nil @-> fun _ -> Response.text "special"))
    |> App.route Route.(get (s "anywhere" /? rest @-> Static.serve ~follow_symlinks:`Anywhere ~index:None root))
  in
  Goo.run (fun () ->
      let req ?(meth = `GET) ?(headers = []) t = App.handle app (Request.make ~meth ~headers t) in
      let status r = Httpun.Status.to_code (Response.status r) in
      let body r = Response.body_to_string r in
      let hd r k = Option.value ~default:"" (Response.header r k) in
      print_endline "-- files";
      let r = req "/assets/app.css" in
      check "200 + body" (status r = 200 && body r = "body{color:red}");
      check "content-type" (hd r "content-type" = "text/css; charset=utf-8");
      check "content-length" (hd r "content-length" = "15");
      check "nosniff, cache-control, accept-ranges"
        (hd r "x-content-type-options" = "nosniff" && hd r "cache-control" = "no-cache" && hd r "accept-ranges" = "bytes");
      check "etag + last-modified" (String.starts_with ~prefix:"W/\"" (hd r "etag") && String.ends_with ~suffix:" GMT" (hd r "last-modified"));
      check "encoded name" (body (req "/assets/report%202024.pdf") = "%PDF" && hd (req "/assets/report%202024.pdf") "content-type" = "application/pdf");
      check "symlink inside the root" (body (req "/assets/alias.css") = "body{color:red}");
      check "exact route beats rest" (body (req "/assets/special") = "special");
      check "HEAD: headers, no body"
        (let r = req ~meth:`HEAD "/assets/app.css" in
         status r = 200 && hd r "content-length" = "15" && body r = "");
      check "POST -> 405" (status (req ~meth:`POST "/assets/app.css") = 405);
      print_endline "-- directories";
      let r = req "/assets" in
      check "no slash -> 301 to slash" (status r = 301 && hd r "location" = "/assets/");
      check "query kept" (hd (req "/assets/sub?x=1") "location" = "/assets/sub/?x=1");
      check "index" (body (req "/assets/") = "<h1>home</h1>" && body (req "/assets/sub/") = "sub home");
      check "no index -> 404" (status (req "/assets/empty/") = 404);
      check "index:None -> 404" (status (req "/anywhere/") = 404);
      check "file with trailing slash -> 404" (status (req "/assets/app.css/") = 404);
      print_endline "-- refused (all 404)";
      List.iter
        (fun t -> check t (status (req t) = 404))
        [
          "/assets/..%2F..%2Fetc%2Fpasswd"; "/assets/../outside/passwd"; "/assets/%2E%2E/outside/passwd";
          "/assets/sub/..%2F.secret"; "/assets/.secret"; "/assets/%2Esecret"; "/assets/a%00b"; "/assets/a%5Cb";
          "/assets//app.css"; "/assets/missing.css"; "/assets/link-out"; "/assets/dir-out/passwd"; "/assets/fifo";
        ];
      check "`Anywhere follows links out" (body (req "/anywhere/link-out") = "root:x:0:0");
      print_endline "-- conditional";
      let r = req "/assets/app.css" in
      let etag = hd r "etag" and lm = hd r "last-modified" in
      let c = req ~headers:[ ("if-none-match", etag) ] "/assets/app.css" in
      check "If-None-Match -> 304, no body" (status c = 304 && body c = "" && hd c "etag" = etag);
      check "If-None-Match list / *"
        (status (req ~headers:[ ("if-none-match", "\"x\", " ^ etag) ] "/assets/app.css") = 304
        && status (req ~headers:[ ("if-none-match", "*") ] "/assets/app.css") = 304);
      check "other etag -> 200" (status (req ~headers:[ ("if-none-match", "W/\"nope\"") ] "/assets/app.css") = 200);
      check "If-Modified-Since = Last-Modified -> 304" (status (req ~headers:[ ("if-modified-since", lm) ] "/assets/app.css") = 304);
      check "older date -> 200"
        (status (req ~headers:[ ("if-modified-since", "Sun, 06 Nov 1994 08:49:37 GMT") ] "/assets/app.css") = 200);
      check "bad date ignored" (status (req ~headers:[ ("if-modified-since", "yesterday") ] "/assets/app.css") = 200);
      print_endline "-- ranges";
      let range h = req ~headers:[ ("range", h) ] "/assets/digits.txt" in
      let r = range "bytes=2-4" in
      check "2-4 -> 206" (status r = 206 && body r = "234" && hd r "content-range" = "bytes 2-4/10" && hd r "content-length" = "3");
      check "7- " (body (range "bytes=7-") = "789");
      check "-3" (body (range "bytes=-3") = "789");
      check "past the end clamps" (body (range "bytes=8-100") = "89");
      check "-100 is the whole file" (body (range "bytes=-100") = "0123456789");
      let r = range "bytes=10-" in
      check "10- -> 416" (status r = 416 && hd r "content-range" = "bytes */10");
      check "multi-range -> whole" (status (range "bytes=0-1,4-5") = 200);
      check "garbage -> whole" (status (range "bytes=x-y") = 200 && status (range "items=0-1") = 200);
      check "If-Range mismatch -> whole"
        (status (req ~headers:[ ("range", "bytes=0-1"); ("if-range", "Sun, 06 Nov 1994 08:49:37 GMT") ] "/assets/digits.txt") = 200);
      print_endline "-- descriptors";
      (* Each request in its own task, as the server runs them: an unsent
         body's file is closed when that task ends. *)
      let open_fds () = Array.length (Sys.readdir "/dev/fd") in
      let before = open_fds () in
      for _ = 1 to 50 do
        Goo.scoped
          [
            (fun () -> ignore (body (req "/assets/app.css")));
            (fun () -> ignore (req "/assets/app.css") (* body never sent *));
            (fun () -> ignore (req ~meth:`HEAD "/assets/app.css"));
            (fun () -> ignore (req ~headers:[ ("if-none-match", etag) ] "/assets/app.css"));
            (fun () -> ignore (body (req ~headers:[ ("range", "bytes=1-2") ] "/assets/digits.txt")));
            (fun () -> ignore (req "/assets/fifo"));
          ]
      done;
      check (Printf.sprintf "no descriptor leak (%d before, %d after)" before (open_fds ())) (open_fds () = before);
      print_endline "-- on the wire (curl)";
      if Sys.command "command -v curl >/dev/null" = 0 then begin
        let server = App.serve (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) app in
        let port = match Goo.Server.addr server with Unix.ADDR_INET (_, p) -> p | _ -> assert false in
        let curl args =
          (* The server runs on this domain: read curl's output through Goo
             rather than blocking on it. *)
          let ic = Unix.open_process_args_in "curl" (Array.of_list ("curl" :: [ "-s"; "-i"; "--max-time"; "2" ] @ args)) in
          let fd = Unix.descr_of_in_channel ic in
          Unix.set_nonblock fd;
          let b = Buffer.create 256 and buf = Bytes.create 4096 in
          let rec loop () =
            match Goo.read fd buf 0 4096 with
            | 0 -> ()
            | n ->
                Buffer.add_subbytes b buf 0 n;
                loop ()
          in
          loop ();
          ignore (Unix.close_process_in ic);
          Buffer.contents b
        in
        let url p = Printf.sprintf "http://127.0.0.1:%d%s" port p in
        let head = curl [ "-I"; url "/assets/app.css" ] in
        let has s sub =
          let n = String.length s and m = String.length sub in
          let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
          go 0
        in
        check "HEAD: length of the GET, no body" (has head "content-length: 15" && String.ends_with ~suffix:"\r\n\r\n" head);
        let nm = curl [ "-H"; "if-none-match: " ^ etag; url "/assets/app.css" ] in
        check "304 without content-length or body" (has nm "304" && (not (has nm "content-length")) && String.ends_with ~suffix:"\r\n\r\n" nm);
        let big = curl [ url "/assets/big.bin" ] in
        check "300 kB in several chunks"
          (String.ends_with ~suffix:(String.init 300_000 (fun i -> Char.chr (i mod 251))) big);
        let rg = curl [ "-H"; "range: bytes=2-4"; url "/assets/digits.txt" ] in
        check "206 body" (has rg "206" && String.ends_with ~suffix:"\r\n\r\n234" rg);
        Goo.Server.shutdown server
      end
      else print_endline "curl not found: skipped");
  Printf.printf "\n%d failure(s)\n" !failures;
  exit (if !failures = 0 then 0 else 1)
