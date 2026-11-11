module R = Goo
module H = Web.HttpunGoo

let t0 = Unix.gettimeofday ()
let log fmt =
  Printf.ksprintf (fun s -> Printf.printf "[%.3f] %s\n%!" (Unix.gettimeofday () -. t0) s) fmt

let ndomains = 4
let stream_gone = Atomic.make false

let text reqd status body =
  let headers =
    Httpun.Headers.of_list
      [ ("content-type", "text/plain"); ("content-length", string_of_int (String.length body)) ]
  in
  Httpun.Reqd.respond_with_string reqd (Httpun.Response.create ~headers status) body

let chunked = Httpun.Response.create ~headers:(Httpun.Headers.of_list [ ("transfer-encoding", "chunked") ]) `OK

let handler _peer { Gluten.reqd; _ } =
  let req = Httpun.Reqd.request reqd in
  let d = R.index (R.self ()) in
  match (req.meth, req.target) with
  | `GET, "/" -> text reqd `OK (Printf.sprintf "hello from d%d" d)
  | `POST, "/echo" ->
      let body = Httpun.Reqd.request_body reqd in
      let buf = Stdlib.Buffer.create 64 in
      let rec read () =
        Httpun.Body.Reader.schedule_read body
          ~on_eof:(fun () -> text reqd `OK ("echo:" ^ Stdlib.Buffer.contents buf))
          ~on_read:(fun bs ~off ~len ->
            Stdlib.Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
            read ())
      in
      read ()
  | `GET, "/slow" ->
      (* Blocking work goes in its own task, not in the read loop. *)
      R.spawn (fun () ->
          R.sleep 0.05;
          text reqd `OK "slow done")
  | `GET, "/stream" ->
      let w = Httpun.Reqd.respond_with_streaming reqd chunked in
      R.spawn (fun () ->
          for i = 1 to 3 do
            Httpun.Body.Writer.write_string w (Printf.sprintf "chunk %d\n" i);
            Httpun.Body.Writer.flush w ignore;
            R.sleep 0.02
          done;
          Httpun.Body.Writer.close w)
  | `GET, "/forever" ->
      let w = Httpun.Reqd.respond_with_streaming reqd chunked in
      R.spawn (fun () ->
          let rec go () =
            if Httpun.Body.Writer.is_closed w then begin
              Atomic.set stream_gone true;
              log "server: /forever client disconnected, stream closed"
            end
            else begin
              Httpun.Body.Writer.write_string w "data: tick\n\n";
              Httpun.Body.Writer.flush w ignore;
              R.sleep 0.02;
              go ()
            end
          in
          go ())
  | `GET, "/boom" -> failwith "handler boom"
  | _ -> text reqd `Not_found "not found"

(* Raw HTTP/1.1 client on the runtime: one request, Connection: close. *)
let raw_get addr path =
  let s = R.socket () in
  R.defer (fun () -> R.close s);
  R.connect s addr;
  R.write_string s (Printf.sprintf "GET %s HTTP/1.1\r\nHost: x\r\nConnection: close\r\n\r\n" path);
  let buf = Bytes.create 4096 and out = Stdlib.Buffer.create 256 in
  let rec go () =
    match R.read s buf 0 4096 with
    | 0 -> Stdlib.Buffer.contents out
    | n ->
        Stdlib.Buffer.add_subbytes out buf 0 n;
        go ()
  in
  go ()

(* curl runs on a separate domain outside the runtime, since it blocks. *)
let curl args =
  let result = Atomic.make None in
  let d =
    Domain.spawn (fun () ->
        let ic = Unix.open_process_args_in "curl" (Array.of_list ("curl" :: "-s" :: args)) in
        let out = In_channel.input_all ic in
        ignore (Unix.close_process_in ic);
        Atomic.set result (Some out))
  in
  let rec wait () =
    match Atomic.get result with
    | Some out -> out
    | None ->
        R.sleep 0.005;
        wait ()
  in
  let out = wait () in
  Domain.join d;
  out

let url addr path =
  match addr with
  | Unix.ADDR_INET (_, port) -> Printf.sprintf "http://127.0.0.1:%d%s" port path
  | _ -> assert false

let tests () =
  let server = H.Server.start (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)) handler in
  let addr = Goo_server.addr server in
  let u = url addr in
  log "GET /            -> %S" (curl [ u "/" ]);
  log "keep-alive x2    -> %S" (curl [ u "/"; u "/" ]);
  log "POST /echo       -> %S" (curl [ "-d"; "some body"; u "/echo" ]);
  log "GET /slow        -> %S" (curl [ u "/slow" ]);
  log "GET /stream      -> %S" (curl [ u "/stream" ]);
  log "GET /boom        -> %S" (curl [ "-w"; " [%{http_code}]"; u "/boom" ]);
  log "GET /missing     -> %S" (curl [ "-w"; " [%{http_code}]"; u "/missing" ]);
  ignore (curl [ "--max-time"; "0.1"; u "/forever" ]);
  R.sleep 0.1;
  log "disconnect noticed by server: %b" (Atomic.get stream_gone);
  let per = Array.make ndomains 0 and ok = Atomic.make 0 in
  R.scoped
    (List.init 500 (fun _ () ->
         let r = raw_get addr "/" in
         if String.starts_with ~prefix:"HTTP/1.1 200" r then begin
           Atomic.incr ok;
           let d = Char.code r.[String.length r - 1] - Char.code '0' in
           per.(d) <- per.(d) + 1
         end));
  log "500 concurrent raw GETs: %d ok; per domain: %s" (Atomic.get ok)
    (String.concat " " (Array.to_list (Array.mapi (Printf.sprintf "d%d=%d") per)));
  let streaming = R.scope [ (fun () -> ignore (curl [ u "/forever" ])) ] in
  R.sleep 0.05;
  let t = Unix.gettimeofday () in
  Goo_server.shutdown ~timeout:0.05 server;
  log "shutdown with an open stream took %.0fms" ((Unix.gettimeofday () -. t) *. 1000.);
  R.join streaming

let () =
  R.run_main ~domains:ndomains tests;
  log "done"
