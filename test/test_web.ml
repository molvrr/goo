open Web
module R = Goo

module Str_like = struct
  let find s sub =
    let n = String.length s and m = String.length sub in
    let rec go i = if i + m > n then None else if String.sub s i m = sub then Some i else go (i + 1) in
    go 0

  let contains s sub = Option.is_some (find s sub)
end

(* One raw HTTP/1.1 request with Connection: close. *)
let curl_raw addr path =
  let fd = Goo.socket () in
  Goo.connect fd addr;
  Goo.write_string fd (Printf.sprintf "GET %s HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" path);
  let b = Bytes.create 4096 and out = Buffer.create 256 in
  let rec go () = match Goo.read fd b 0 4096 with 0 -> () | n -> Buffer.add_subbytes out b 0 n; go () in
  go ();
  Goo.close fd;
  Buffer.contents out

let failures = ref 0

let check name cond =
  if not cond then incr failures;
  Printf.printf "%s %s\n%!" (if cond then "ok  " else "FAIL") name

let user_path () = Route.(s "users" / int /? nil)

let app =
  let log = ref [] in
  let tag name : App.middleware =
   fun next req ->
    log := name :: !log;
    Response.with_header ("x-" ^ name) "1" (next req)
  in
  ( log,
    App.create ()
    |> App.routes
         Route.
           [
             get (nil @-> fun _ -> Response.text "home");
             get (user_path () @-> fun _ id -> Response.text (Printf.sprintf "user %d" id));
             delete (user_path () @-> fun _ id -> Response.text (Printf.sprintf "deleted %d" id));
             get
               (s "users" / string / s "delete" /? nil @-> fun _ name ->
                Response.text ("confirm delete " ^ name));
             get
               (s "search" /? nil
               /& Q.string "q"
               /& Q.default 1 (Q.int "page")
               /& Q.list (Q.string "tag")
               /& Q.flag "exact"
               @-> fun _ q page tags exact ->
               Response.text
                 (Printf.sprintf "q=%s page=%d tags=[%s] exact=%b" q page (String.concat "," tags) exact));
             get (s "files" /? trail @-> fun _ -> Response.text "files");
             post (s "echo" /? nil @-> fun req -> Response.text ("echo:" ^ Request.body ~max:16 req));
             post
               (s "form" /? nil @-> fun req ->
                Response.text (String.concat ";" (List.map (fun (k, v) -> k ^ "=" ^ v) (Request.form req))));
             get (s "boom" /? nil @-> fun _ -> failwith "secret detail");
             get (s "redirect" /? nil @-> fun _ -> Response.redirect (Route.url (user_path ()) 7));
             get
               (s "stream" /? nil @-> fun _ ->
                Response.stream (fun write -> List.iter write [ "a"; "b"; "c" ]));
             get
               (s "sse" /? nil @-> fun _ ->
                Response.sse (fun send ->
                    send ~event:"greeting" ~id:"1" "hello\nworld";
                    send "bye"));
             get
               (s "forever" /? nil @-> fun _ ->
                Response.sse ~ping:0.05 (fun send ->
                    Fun.protect
                      ~finally:(fun () -> print_endline "     (forever: producer cancelled after disconnect)")
                      (fun () ->
                        send "first";
                        R.sleep 3600.)));
           ]
    |> App.use (tag "outer")
    |> App.use (tag "inner") )

let get ?(meth = `GET) ?body target =
  let _, app = app in
  App.handle app (Request.make ~meth ?body target)

let body r = Response.body_to_string r
let status r = Httpun.Status.to_code (Response.status r)

let unit_tests () =
  print_endline "-- routing (App.handle, no network)";
  check "GET / -> home" (body (get "/") = "home");
  check "GET /users/42 -> user 42" (body (get "/users/42") = "user 42");
  check "DELETE /users/42" (body (get ~meth:`DELETE "/users/42") = "deleted 42");
  check "GET /users/abc/delete (string capture)" (body (get "/users/abc/delete") = "confirm delete abc");
  check "percent-decoded capture" (body (get "/users/a%2Fb%20c/delete") = "confirm delete a/b c");
  check "GET /users/x -> 404 (int doesn't parse)" (status (get "/users/x") = 404);
  check "GET /users/42/extra -> 404" (status (get "/users/42/extra") = 404);
  let r = get ~meth:`POST "/users/42" in
  check "POST /users/42 -> 405" (status r = 405);
  check "  Allow: DELETE, GET" (Response.header r "allow" = Some "DELETE, GET");
  check "nil rejects trailing slash" (status (get "/users/42/") = 404);
  check "trail accepts both" (body (get "/files") = "files" && body (get "/files/") = "files");

  print_endline "-- typed query parameters";
  check "all params"
    (body (get "/search?q=ocaml+effects&page=3&tag=a&tag=b%2Cc&exact")
    = "q=ocaml effects page=3 tags=[a,b,c] exact=true");
  check "defaults" (body (get "/search?q=x") = "q=x page=1 tags=[] exact=false");
  let r = get "/search?page=2" in
  check "missing required -> 400" (status r = 400 && body r = "missing query parameter \"q\"");
  let r = get "/search?q=x&page=two" in
  check "malformed -> 400" (status r = 400 && body r = "query parameter \"page\": expected int, got \"two\"");

  print_endline "-- bodies, errors, middleware";
  check "POST /echo" (body (get ~meth:`POST ~body:"hi" "/echo") = "echo:hi");
  check "body over max -> 413" (status (get ~meth:`POST ~body:(String.make 17 'x') "/echo") = 413);
  check "form" (body (get ~meth:`POST ~body:"a=1&b=x+y%21" "/form") = "a=1;b=x y!");
  let r = get "/boom" in
  check "exception -> generic 500" (status r = 500 && body r = "Internal Server Error");
  let log, _ = app in
  log := [];
  let r = get "/nope" in
  check "404 goes through middleware" (status r = 404 && Response.header r "x-outer" = Some "1");
  check "middleware order: outer first" (List.rev !log = [ "outer"; "inner" ]);
  let r = get "/redirect" in
  check "redirect 303 to url" (status r = 303 && Response.header r "location" = Some "/users/7");
  check "stream body" (body (get "/stream") = "abc");
  check "sse format"
    (body (get "/sse") = "event: greeting\nid: 1\ndata: hello\ndata: world\n\ndata: bye\n\n");

  print_endline "-- reverse routing";
print_endline "-- header injection";
  let rejects f = match f () with _ -> false | exception Invalid_argument _ -> true in
  check "redirect with CRLF rejected" (rejects (fun () -> Response.redirect "/home\r\nSet-Cookie: a=1"));
  check "bare LF rejected" (rejects (fun () -> Response.with_header "x-a" "a\nb" (Response.text "")));
  check "NUL rejected" (rejects (fun () -> Response.make ~headers:[ ("x-a", "a\000") ] ""));
  check "bad header name rejected" (rejects (fun () -> Response.with_header "x a:" "v" (Response.text "")));
  check "stream/sse headers checked" (rejects (fun () -> Response.sse ~headers:[ ("x-a", "\r") ] ignore));
  check "ordinary values pass"
    (Response.header (Response.with_header "X-Tab" "a\tb é" (Response.text "")) "x-tab" = Some "a\tb é");
  let next_app =
    App.create ()
    |> App.route Route.(get (s "go" /? nil /& Q.string "next" @-> fun _ next -> Response.redirect next))
  in
  let r = App.handle next_app (Request.make "/go?next=/home%0d%0aSet-Cookie:%20pwn=1") in
  check "injected redirect -> 500, no header" (status r = 500 && Response.header r "set-cookie" = None);
  check "client headers checked"
    (rejects (fun () -> Client.get ~headers:[ ("x-a", "1\r\nx-b: 2") ] "http://127.0.0.1:9/"));
  print_endline "-- dot segments";
  let files () = Route.(s "files" / string /? nil) in
  check "url escapes .." (Route.url (files ()) ".." = "/files/%2E%2E");
  check "url escapes ." (Route.url (files ()) "." = "/files/%2E");
  check "url keeps ..x" (Route.url (files ()) "..x" = "/files/..x");
  let files_app = App.create () |> App.route Route.(get (files () @-> fun _ f -> Response.text f)) in
  check "escaped .. round-trips"
    (body (App.handle files_app (Request.make (Route.url (files ()) ".."))) = "..");
    check "url" (Route.url (user_path ()) 42 = "/users/42");
  check "url encodes segments"
    (Route.url Route.(s "users" / string / s "delete" /? nil) "a/b c" = "/users/a%2Fb%20c/delete");
  check "url with query"
    (Route.url Route.(s "search" /? nil /& Q.string "q" /& Q.opt (Q.int "page") /& Q.list (Q.string "tag"))
       "a&b" None [ "x"; "y" ]
    = "/search?q=a%26b&tag=x&tag=y");
  check "pattern"
    (Route.pattern Route.(s "search" / int /? nil /& Q.default 1 (Q.int "page") /& Q.opt (Q.string "q"))
    = "/search/:int?page=<int>&q=<string?>");
  let _, a = app in
  check "describe" (List.nth (App.describe a) 1 = "GET /users/:int")

let router_tests () =
  print_endline "-- trie router";
  let app =
    App.create ()
    |> App.routes
         Route.
           [
             (* Added before the literal, but the literal wins at that segment. *)
             get (s "users" / string /? nil @-> fun _ name -> Response.text ("user " ^ name));
             get (s "users" / s "new" /? nil @-> fun _ -> Response.text "new user form");
             post (s "items" / s "new" /? nil @-> fun _ -> Response.text "create item");
             get (s "items" / string /? nil @-> fun _ name -> Response.text ("item " ^ name));
             (* Captures at the same segment: int before string, in order. *)
             get (s "n" / int /? nil @-> fun _ n -> Response.text (Printf.sprintf "int %d" n));
             get (s "n" / string /? nil @-> fun _ n -> Response.text ("string " ^ n));
             (* Backtracking: the literal branch fails deeper, the capture takes it. *)
             get (s "a" / s "b" / s "c" /? nil @-> fun _ -> Response.text "abc");
             get (s "a" / string / s "d" /? nil @-> fun _ x -> Response.text ("a " ^ x ^ " d"));
             put (s "doc" / int /? nil @-> fun _ _ -> Response.text "put");
             patch (s "doc" / string /? nil @-> fun _ _ -> Response.text "patch");
             get (s "dir" /? trail @-> fun _ -> Response.text "dir");
           ]
  in
  let go ?(meth = `GET) target = App.handle app (Request.make ~meth target) in
  check "literal beats earlier capture" (body (go "/users/new") = "new user form");
  check "capture still matches others" (body (go "/users/bob") = "user bob");
  check "wrong method on literal falls back to capture" (body (go "/items/new") = "item new");
  check "POST reaches the literal" (body (go ~meth:`POST "/items/new") = "create item");
  check "captures in order: int" (body (go "/n/5") = "int 5");
  check "captures in order: string" (body (go "/n/five") = "string five");
  check "backtrack from literal branch" (body (go "/a/b/d") = "a b d");
  check "literal branch when it completes" (body (go "/a/b/c") = "abc");
  let r = go ~meth:`DELETE "/doc/1" in
  check "405 lists methods from every matching path"
    (status r = 405 && Response.header r "allow" = Some "PATCH, PUT");
  check "trailing slash via trie" (body (go "/dir/") = "dir" && body (go "/dir") = "dir");
  check "404" (status (go "/a/x/c") = 404)

let bench () =
  print_endline "-- lookup benchmark: 2000 routes";
  let n = 2000 in
  let routes =
    List.init n (fun i ->
        Route.(get (s "api" / s (Printf.sprintf "r%d" i) / int / s "items" /? nil @-> fun _ id -> Response.text (string_of_int id))))
  in
  let app = App.create () |> App.routes routes in
  let router = Router.create routes in
  let reqs = Array.init 1000 (fun i -> Request.make (Printf.sprintf "/api/r%d/%d/items" (i * 7 mod n) i)) in
  let linear req =
    List.find_map (fun r -> match Route.matches r req with Matched run -> Some (run ()) | _ -> None) routes
  in
  let time name iters f =
    let t = Unix.gettimeofday () in
    for _ = 1 to iters do
      Array.iter f reqs
    done;
    let per = (Unix.gettimeofday () -. t) /. float (iters * Array.length reqs) *. 1e9 in
    Printf.printf "     %-8s %8.0f ns/lookup\n%!" name per;
    per
  in
  let lin = time "linear" 2 (fun r -> ignore (linear r)) in
  let trie = time "trie" 50 (fun r -> match Router.lookup router r with Found run -> ignore (run ()) | _ -> assert false) in
  check (Printf.sprintf "trie is faster (%.0fx)" (lin /. trie)) (trie < lin);
  check "same answers as linear"
    (Array.for_all
       (fun r -> Option.map body (linear r) = Some (body (App.handle app r)))
       reqs)

(* A minimal WebSocket client on the runtime: masked frames out, unmasked in. *)
module Ws_client = struct
  type t = { fd : Unix.file_descr; mutable buf : string }

  let rec fill c =
    let b = Bytes.create 4096 in
    match R.read c.fd b 0 4096 with
    | 0 -> raise End_of_file
    | n -> c.buf <- c.buf ^ Bytes.sub_string b 0 n

  and take c n =
    if String.length c.buf < n then (fill c; take c n)
    else begin
      let s = String.sub c.buf 0 n in
      c.buf <- String.sub c.buf n (String.length c.buf - n);
      s
    end

  let rec read_until c sep =
    match Str_like.find c.buf sep with
    | Some i -> take c (i + String.length sep)
    | None -> fill c; read_until c sep

  let handshake ?(path = "/ws") addr =
    let fd = R.socket () in
    R.connect fd addr;
    let c = { fd; buf = "" } in
    R.write_string fd
      (Printf.sprintf
         "GET %s HTTP/1.1\r\nHost: x\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\nSec-WebSocket-Version: 13\r\n\r\n"
         path);
    (c, read_until c "\r\n\r\n")

  let send ?(fin = true) c opcode payload =
    let len = String.length payload in
    let b = Buffer.create (len + 14) in
    Buffer.add_char b (Char.chr ((if fin then 0x80 else 0) lor opcode));
    if len < 126 then Buffer.add_char b (Char.chr (0x80 lor len))
    else begin
      Buffer.add_char b (Char.chr (0x80 lor 126));
      Buffer.add_uint16_be b len
    end;
    let mask = "\x12\x34\x56\x78" in
    Buffer.add_string b mask;
    String.iteri (fun i ch -> Buffer.add_char b (Char.chr (Char.code ch lxor Char.code mask.[i mod 4]))) payload;
    R.write_string c.fd (Buffer.contents b)

  let recv c =
    let h = take c 2 in
    let opcode = Char.code h.[0] land 0x0f in
    let len =
      match Char.code h.[1] land 0x7f with
      | 126 -> String.get_uint16_be (take c 2) 0
      | 127 -> Int64.to_int (String.get_int64_be (take c 8) 0)
      | n -> n
    in
    (opcode, take c len)

  let close_code payload = if String.length payload >= 2 then String.get_uint16_be payload 0 else 0
end

let websocket_tests () =
  print_endline "-- websockets";
  let session_end = Atomic.make "" in
  let app =
    App.create ()
    |> App.routes
         Route.
           [
             get
               (s "ws" /? nil @-> fun _ ->
                Response.websocket ~max_message:64 (fun ws ->
                    let rec loop () =
                      match Websocket.recv ws with
                      | Some (`Text s) -> Websocket.send ws (`Text ("echo: " ^ s)); loop ()
                      | Some (`Binary b) -> Websocket.send ws (`Binary (String.uppercase_ascii b)); loop ()
                      | None -> Atomic.set session_end "recv returned None"
                    in
                    loop ()));
             get
               (s "ticks" /? nil @-> fun _ ->
                Response.websocket (fun ws ->
                    for i = 1 to 3 do
                      Websocket.send ws (`Text (Printf.sprintf "tick %d" i));
                      R.sleep 0.01
                    done));
             get
               (s "idle" /? nil @-> fun _ ->
                Response.websocket (fun _ ->
                    Fun.protect
                      ~finally:(fun () -> Atomic.set session_end "idle session cancelled")
                      (fun () -> R.sleep 3600.)));
           ]
  in
  let server = App.serve (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) app in
  let addr = R.Server.addr server in
  let c, resp = Ws_client.handshake addr in
  check "101 with the RFC 6455 accept key"
    (String.starts_with ~prefix:"HTTP/1.1 101" resp
    && Str_like.contains resp "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=");
  Ws_client.send c 0x1 "hello";
  check "text echo" (Ws_client.recv c = (0x1, "echo: hello"));
  Ws_client.send c 0x2 "bin";
  check "binary" (Ws_client.recv c = (0x2, "BIN"));
  Ws_client.send ~fin:false c 0x1 "frag";
  Ws_client.send ~fin:false c 0x0 "men";
  Ws_client.send c 0x0 "ted";
  check "fragments reassembled" (Ws_client.recv c = (0x1, "echo: fragmented"));
  Ws_client.send c 0x9 "p1";
  check "ping -> pong with payload" (Ws_client.recv c = (0xA, "p1"));
  Ws_client.send c 0x8 "\x03\xe8";
  let op, payload = Ws_client.recv c in
  check "close echoed with 1000" (op = 0x8 && Ws_client.close_code payload = 1000);
  R.sleep 0.02;
  check "session saw the close" (Atomic.get session_end = "recv returned None");
  R.close c.fd;

  let c, _ = Ws_client.handshake addr in
  Ws_client.send c 0x1 (String.make 100 'x');
  let op, payload = Ws_client.recv c in
  check "message over max_message -> 1009" (op = 0x8 && Ws_client.close_code payload = 1009);
  R.close c.fd;

  let c, _ = Ws_client.handshake ~path:"/ticks" addr in
  let msgs = List.init 3 (fun _ -> snd (Ws_client.recv c)) in
  check "server push" (msgs = [ "tick 1"; "tick 2"; "tick 3" ]);
  let op, payload = Ws_client.recv c in
  check "session return closes with 1000" (op = 0x8 && Ws_client.close_code payload = 1000);
  R.close c.fd;

  let c, _ = Ws_client.handshake ~path:"/idle" addr in
  R.sleep 0.02;
  R.close c.fd;
  R.sleep 0.05;
  check "abrupt disconnect cancels a blocked session" (Atomic.get session_end = "idle session cancelled");

  let plain = curl_raw addr "/ws" in
  check "plain GET on a websocket route -> 400" (String.starts_with ~prefix:"HTTP/1.1 400" plain);
  R.Server.shutdown ~timeout:1. server

let client_tests () =
  print_endline "-- client";
  let peer_port req =
    match Request.peer req with Some (Unix.ADDR_INET (_, p)) -> string_of_int p | _ -> "?"
  in
  let app =
    App.create ()
    |> App.routes
         Route.
           [
             get (s "users" / int /? nil @-> fun _ id -> Response.text (Printf.sprintf "user %d" id));
             post (s "echo" /? nil @-> fun req -> Response.text ("echo:" ^ Request.body req));
             get (s "peer" /? nil @-> fun req -> Response.text (peer_port req));
             get (s "slow" /? nil @-> fun _ -> R.sleep 1.; Response.text "late");
             get
               (s "chunks" /? nil @-> fun _ ->
                Response.stream (fun write ->
                    List.iter (fun c -> write c; R.sleep 0.01) [ "one;"; "two;"; "three" ]));
             get
               (s "ws" /? nil @-> fun _ ->
                Response.websocket (fun ws ->
                    let rec loop () =
                      match Websocket.recv ws with
                      | Some (`Text s) -> Websocket.send ws (`Text (String.uppercase_ascii s)); loop ()
                      | Some _ -> loop ()
                      | None -> ()
                    in
                    loop ()));
           ]
  in
  let server = App.serve (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) app in
  let base =
    match R.Server.addr server with
    | Unix.ADDR_INET (_, p) -> Printf.sprintf "http://127.0.0.1:%d" p
    | _ -> assert false
  in
  let r = Client.get (base ^ "/users/7") in
  check "GET" (Client.status r = `OK && Client.body r = "user 7");
  check "response headers" (Client.header r "content-type" = Some "text/plain; charset=utf-8");
  check "POST body" (Client.body (Client.post ~body:"hi there" (base ^ "/echo")) = "echo:hi there");
  check "404" (Client.status (Client.get (base ^ "/nope")) = `Not_found);
  check "no pool: a new connection each time"
    (Client.body (Client.get (base ^ "/peer")) <> Client.body (Client.get (base ^ "/peer")));
  Client.with_pool (fun pool ->
      let a = Client.body (Client.get ~pool (base ^ "/peer")) in
      let b = Client.body (Client.get ~pool (base ^ "/peer")) in
      let c = Client.body (Client.post ~pool ~body:"x" (base ^ "/echo")) in
      let d = Client.body (Client.get ~pool (base ^ "/peer")) in
      check "pool reuses the connection" (a = b && b = d && c = "echo:x");
      let ok = Atomic.make 0 in
      R.scoped
        (List.init 100 (fun i () ->
             if Client.body (Client.get ~pool (Printf.sprintf "%s/users/%d" base i)) = Printf.sprintf "user %d" i
             then Atomic.incr ok));
      check "100 concurrent requests through one pool" (Atomic.get ok = 100));
  let chunks =
    Client.stream (base ^ "/chunks") (fun resp read ->
        let rec go acc = match read () with Some c -> go (c :: acc) | None -> List.rev acc in
        (Client.status resp, go []))
  in
  check "stream reads chunks as they arrive"
    (fst chunks = `OK && String.concat "" (snd chunks) = "one;two;three" && List.length (snd chunks) >= 2);
  let t0 = Unix.gettimeofday () in
  (match Client.get ~timeout:0.1 (base ^ "/slow") with
  | _ -> check "timeout" false
  | exception Client.Timeout -> check "timeout raises Timeout" (Unix.gettimeofday () -. t0 < 0.5));
  (match Client.get "http://127.0.0.1:1/" with
  | _ -> check "connection refused" false
  | exception Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> check "connection refused raises" true);
  let replies =
    Websocket.connect ("ws" ^ String.sub base 4 (String.length base - 4) ^ "/ws") (fun ws ->
        Websocket.send ws (`Text "hello");
        Websocket.send ws (`Text "world");
        let a = Websocket.recv ws and b = Websocket.recv ws in
        (a, b))
  in
  check "websocket client" (replies = (Some (`Text "HELLO"), Some (`Text "WORLD")));
  (match Websocket.connect (base ^ "/users/1") (fun _ -> ()) with
  | () -> check "websocket handshake failure" false
  | exception Failure m -> check ("websocket handshake failure: " ^ m) true);
  R.Server.shutdown ~timeout:1. server

let ws_route_tests () =
  print_endline "-- ws routes";
  let echo prefix ws =
    let rec loop () =
      match Websocket.recv ws with
      | Some (`Text s) -> Websocket.send ws (`Text (prefix ^ s)); loop ()
      | Some _ -> loop ()
      | None -> ()
    in
    loop ()
  in
  let app =
    App.create ()
    |> App.routes
         Route.
           [
             get (s "room" / string /? nil @-> fun _ room -> Response.text ("page for " ^ room));
             ws (s "room" / string /? nil @-> fun _ room ws -> echo (Printf.sprintf "[%s] " room) ws);
             ws
               (s "chat" /? nil /& Q.string "nick" @-> fun _ nick ws ->
                Websocket.send ws (`Text ("hi " ^ nick));
                echo (nick ^ ": ") ws);
           ]
  in
  check "describe shows WS" (App.describe app = [ "GET /room/:string"; "WS /room/:string"; "WS /chat?nick=<string>" ]);
  let go ?(meth = `GET) ?(headers = []) t = App.handle app (Request.make ~meth ~headers t) in
  check "plain GET falls through to the page" (body (go "/room/lobby") = "page for lobby");
  check "upgrade request picks the ws route"
    (status (go ~headers:[ ("upgrade", "websocket") ] "/room/lobby") = 101);
  let r = go "/chat?nick=ana" in
  check "plain GET on a ws-only path -> 426" (status r = 426 && Response.header r "upgrade" = Some "websocket");
  let r = go ~meth:`POST "/chat?nick=ana" in
  check "POST -> 405 Allow: GET" (status r = 405 && Response.header r "allow" = Some "GET");
  check "missing query param on upgrade -> 400"
    (status (go ~headers:[ ("upgrade", "websocket") ] "/chat") = 400);
  let server = App.serve (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) app in
  let base =
    match R.Server.addr server with
    | Unix.ADDR_INET (_, p) -> Printf.sprintf "ws://127.0.0.1:%d" p
    | _ -> assert false
  in
  let reply =
    Websocket.connect (base ^ "/room/lobby") (fun ws ->
        Websocket.send ws (`Text "hello");
        Websocket.recv ws)
  in
  check "room capture reaches the session" (reply = Some (`Text "[lobby] hello"));
  let greet, echoed =
    Websocket.connect (base ^ "/chat?nick=bo") (fun ws ->
        let g = Websocket.recv ws in
        Websocket.send ws (`Text "yo");
        (g, Websocket.recv ws))
  in
  check "query param reaches the session" (greet = Some (`Text "hi bo") && echoed = Some (`Text "bo: yo"));
  check "same URL serves the page over HTTP"
    (Client.body (Client.get ("http" ^ String.sub base 2 (String.length base - 2) ^ "/room/lobby")) = "page for lobby");
  R.Server.shutdown ~timeout:1. server

(* curl on a separate domain, since it blocks. *)
let curl args =
  let result = Atomic.make None in
  let d =
    Domain.spawn (fun () ->
        let ic = Unix.open_process_args_in "curl" (Array.of_list ("curl" :: "-s" :: args)) in
        let out = In_channel.input_all ic in
        ignore (Unix.close_process_in ic);
        Atomic.set result (Some out))
  in
  let rec wait () = match Atomic.get result with Some o -> o | None -> R.sleep 0.005; wait () in
  let o = wait () in
  Domain.join d;
  o

let served_tests () =
  print_endline "-- served over HTTP (curl)";
  let _, app = app in
  let server = App.serve (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) app in
  let base =
    match R.Server.addr server with
    | Unix.ADDR_INET (_, p) -> Printf.sprintf "http://127.0.0.1:%d" p
    | _ -> assert false
  in
  check "GET /users/42" (curl [ base ^ "/users/42" ] = "user 42");
  check "keep-alive, two requests" (curl [ base ^ "/"; base ^ "/users/1" ] = "homeuser 1");
  check "query" (curl [ base ^ "/search?q=a&tag=t" ] = "q=a page=1 tags=[t] exact=false");
  check "POST body" (curl [ "-d"; "hey"; base ^ "/echo" ] = "echo:hey");
  check "413" (curl [ "-o"; "/dev/null"; "-w"; "%{http_code}"; "-d"; String.make 100 'x'; base ^ "/echo" ] = "413");
  check "405 + Allow"
    (curl [ "-X"; "POST"; "-i"; base ^ "/users/1" ] |> fun s ->
     String.starts_with ~prefix:"HTTP/1.1 405" s
     && (Str_like.contains s "allow: DELETE, GET"));
  check "500 hides the exception" (curl [ base ^ "/boom" ] = "Internal Server Error");
  check "streamed (chunked)" (curl [ base ^ "/stream" ] = "abc");
  check "sse" (curl [ base ^ "/sse" ] = "event: greeting\nid: 1\ndata: hello\ndata: world\n\ndata: bye\n\n");
  let out = curl [ "--max-time"; "0.2"; base ^ "/forever" ] in
  check "sse until client leaves" (String.starts_with ~prefix:"data: first\n\n: ping" out);
  R.sleep 0.2;
  R.Server.shutdown ~timeout:1. server

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Error);
  R.run_parallel ~domains:2 (fun i ->
      if i = 0 then begin
        unit_tests ();
        router_tests ();
        bench ();
        served_tests ();
        websocket_tests ();
        client_tests ();
        ws_route_tests ()
      end);
  Printf.printf "\n%d failure(s)\n" !failures;
  exit (if !failures = 0 then 0 else 1)
