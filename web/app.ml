let src = Logs.Src.create "web"

module Log = (val Logs.src_log src : Logs.LOG)

type handler = Request.t -> Response.t
type middleware = handler -> handler
type t = {
  routes : unit Route.route list;
  router : Router.t Lazy.t;  (* rebuilt when routes change *)
  middleware : middleware list;
  not_found : handler;
}

let default_not_found _ = Response.text ~status:`Not_found "Not Found"
let with_routes routes t = { t with routes; router = lazy (Router.create routes) }
let create () = { routes = []; router = lazy (Router.create []); middleware = []; not_found = default_not_found }
let route r t = with_routes (t.routes @ [ r ]) t
let routes rs t = with_routes (t.routes @ rs) t
let use m t = { t with middleware = t.middleware @ [ m ] }
let not_found h t = { t with not_found = h }
let describe t = List.map Route.route_pattern t.routes

let dispatch t req =
  match Router.lookup (Lazy.force t.router) req with
  | Found run -> run ()
  | Bad_query msg -> Response.text ~status:`Bad_request msg
  | Upgrade_required ->
      Response.text ~status:`Upgrade_required ~headers:[ ("upgrade", "websocket") ] "Upgrade Required"
  | Not_found [] -> t.not_found req
  | Not_found methods ->
      Response.text ~status:`Method_not_allowed
        ~headers:[ ("allow", String.concat ", " methods) ]
        "Method Not Allowed"

let handle t =
  let chain = List.fold_right (fun m h -> m h) t.middleware (dispatch t) in
  fun req ->
    try chain req with
    | (Goo.Cancelled | Goo.Closed) as e -> raise e
    | Request.Body_too_large -> Response.text ~status:`Payload_too_large "Payload Too Large"
    | e ->
        Log.err (fun m ->
            m "%s %s: %s" (Httpun.Method.to_string (Request.meth req)) (Request.target req)
              (Printexc.to_string e));
        Response.text ~status:`Internal_server_error "Internal Server Error"

(* httpun's default would send the exception text to the client. *)
let error_handler _peer ?request:_ error start_response =
  let msg =
    match error with
    | `Bad_request -> "Bad Request"
    | `Bad_gateway -> "Bad Gateway"
    | `Internal_server_error -> "Internal Server Error"
    | `Exn e ->
        Log.err (fun m -> m "connection error: %s" (Printexc.to_string e));
        "Internal Server Error"
  in
  let body = start_response (Httpun.Headers.of_list [ ("content-type", "text/plain") ]) in
  Httpun.Body.Writer.write_string body msg;
  Httpun.Body.Writer.close body

let serve ?config ?domains ?acceptors ?backlog ?strategy addr t =
  let handle = handle t in
  HttpunGoo.Server.start ?config ?domains ?acceptors ?backlog ?strategy ~error_handler addr
    (fun peer { Gluten.reqd; upgrade } ->
      Goo.spawn (fun () ->
          let req = Request.of_reqd reqd peer in
          (try Response.send ~upgrade (handle req) reqd with Goo.Closed -> ());
          Request.drain req))
