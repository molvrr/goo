let is_datastar_request req = Web.Request.header req "datastar-request" = Some "true"

let read_signals req =
  let parse s = try Ok (Yojson.Safe.from_string s) with Yojson.Json_error e -> Error e in
  match Web.Request.meth req with
  | `GET | `DELETE -> (
      match Web.Request.query req "datastar" with
      | s :: _ -> parse s
      | [] -> Error "missing datastar query parameter")
  | _ -> parse (Web.Request.body req)

(* An event is SSE fields in order; [data] fields become one multi-line
   payload, which [Web.Response.sse] splits back into [data:] lines. *)
let send_event send (ev : Datastar.Sse.event) =
  let field k = List.assoc_opt k ev in
  let data = List.filter_map (fun (k, v) -> if k = "data" then Some v else None) ev in
  send ?event:(field "event") ?id:(field "id")
    ?retry:(Option.map int_of_string (field "retry"))
    (String.concat "\n" data)

let sse ?headers ?ping f = Web.Response.sse ?headers ?ping (fun send -> f (send_event send))
