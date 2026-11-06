type body =
  | String of string
  | Stream of ((string -> unit) -> unit)
  | Websocket of int * (Websocket.t -> unit)
type t = { status : Httpun.Status.t; headers : (string * string) list; body : body }

let lower = String.lowercase_ascii
let has name headers = List.exists (fun (k, _) -> lower k = lower name) headers
let header t name = List.find_map (fun (k, v) -> if lower k = lower name then Some v else None) t.headers

let with_header name value t =
  Codec.check_header (name, value);
  { t with headers = (name, value) :: List.filter (fun (k, _) -> lower k <> lower name) t.headers }

let add_default name value headers = if has name headers then headers else headers @ [ (name, value) ]
let make ?(status = `OK) ?(headers = []) s = { status; headers = Codec.check_headers headers; body = String s }
let empty status = make ~status ""

let typed ct ?status ?(headers = []) s =
  make ?status ~headers:(add_default "content-type" ct headers) s

let text = typed "text/plain; charset=utf-8"
let html_string = typed "text/html; charset=utf-8"
let html ?status ?headers doc = html_string ?status ?headers (Html.render_document doc)
let html_elt ?status ?headers elt = html_string ?status ?headers (Html.render elt)
let json = typed "application/json"
let redirect ?(status = `See_other) location =
  { (empty status) with headers = Codec.check_headers [ ("location", location) ] }

let stream ?(status = `OK) ?(headers = []) f = { status; headers = Codec.check_headers headers; body = Stream f }
let status t = t.status
let headers t = t.headers

let sse ?(headers = []) ?(ping = 15.) f =
  let headers =
    headers
    |> add_default "content-type" "text/event-stream"
    |> add_default "cache-control" "no-cache"
  in
  stream ~headers (fun write ->
      let send ?event ?id ?retry data =
        let b = Buffer.create (String.length data + 16) in
        Option.iter (Printf.bprintf b "event: %s\n") event;
        Option.iter (Printf.bprintf b "id: %s\n") id;
        Option.iter (Printf.bprintf b "retry: %d\n") retry;
        List.iter (Printf.bprintf b "data: %s\n") (String.split_on_char '\n' data);
        Buffer.add_char b '\n';
        write (Buffer.contents b)
      in
      (* The producer and a pinger share a scope: when the pinger hits a
         closed connection it fails the scope, cancelling the producer; when
         the producer finishes it cancels the pinger. *)
      let self = ref None in
      let s =
        Goo.scope
          [
            (fun () ->
              f send;
              Option.iter Goo.cancel !self);
            (fun () ->
              let rec loop () =
                Goo.sleep ping;
                write ": ping\n\n";
                loop ()
              in
              loop ());
          ]
      in
      self := Some s;
      Goo.join s)

let websocket ?(headers = []) ?(max_message = 1 lsl 20) session =
  { status = `Switching_protocols; headers = Codec.check_headers headers; body = Websocket (max_message, session) }

let body_to_string t =
  match t.body with
  | String s -> s
  | Websocket _ -> invalid_arg "Response.body_to_string: websocket"
  | Stream f ->
      let b = Buffer.create 256 in
      f (Buffer.add_string b);
      Buffer.contents b

(* No body goes out for HEAD, 204 and 304 (httpun drops it). Their
   content-length, if any, must be the caller's (the length a GET would
   have had), never the empty string's. *)
let bodiless reqd status =
  (Httpun.Reqd.request reqd).meth = `HEAD || status = `No_content || status = `Not_modified

let respond_string reqd status headers s =
  let headers =
    if bodiless reqd status then headers
    else add_default "content-length" (string_of_int (String.length s)) headers
  in
  Httpun.Reqd.respond_with_string reqd
    (Httpun.Response.create ~headers:(Httpun.Headers.of_list headers) status)
    s

let send ?upgrade t reqd =
  match t.body with
  | Websocket (max_message, session) -> (
      match upgrade with
      | None -> respond_string reqd `Internal_server_error [] "WebSocket upgrade unavailable"
      | Some upgrade -> (
          match Websocket.accept ~headers:t.headers ~max_message ~upgrade reqd session with
          | Ok () -> ()
          | Error msg -> respond_string reqd `Bad_request [ ("content-type", "text/plain") ] msg))
  | String s -> respond_string reqd t.status t.headers s
  | Stream _ when bodiless reqd t.status -> respond_string reqd t.status t.headers ""
  | Stream f ->
      (* Without a length or chunked encoding httpun keeps the connection
         open after the body and the client can't tell where it ends. *)
      let headers =
        if has "content-length" t.headers then t.headers
        else add_default "transfer-encoding" "chunked" t.headers
      in
      let w =
        Httpun.Reqd.respond_with_streaming ~flush_headers_immediately:true reqd
          (Httpun.Response.create ~headers:(Httpun.Headers.of_list headers) t.status)
      in
      let write chunk =
        if Httpun.Body.Writer.is_closed w then raise Goo.Closed;
        Httpun.Body.Writer.write_string w chunk;
        let flushed = Goo.Chan.create () in
        Httpun.Body.Writer.flush w (fun r -> Goo.protect (fun () -> Goo.Chan.send flushed r));
        match Goo.Chan.recv flushed with `Written -> () | `Closed -> raise Goo.Closed
      in
      Fun.protect ~finally:(fun () -> Httpun.Body.Writer.close w) (fun () -> f write)
