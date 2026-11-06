exception Body_too_large

type source = Reqd of Httpun.Reqd.t | Fixed of string

type t = {
  meth : Httpun.Method.t;
  target : string;
  headers : Httpun.Headers.t;
  peer : Unix.sockaddr option;
  path : string;
  segments : string list;
  query : (string * string) list;
  source : source;
  mutable body : [ `Unread | `Read of string | `Too_large ];
}

let create ~meth ~target ~headers ~peer source =
  let path, qs = Codec.split_target target in
  {
    meth;
    target;
    headers;
    peer;
    path;
    segments = Codec.split_path path;
    query = Codec.parse_query qs;
    source;
    body = `Unread;
  }

let of_reqd reqd peer =
  let { Httpun.Request.meth; target; headers; _ } = Httpun.Reqd.request reqd in
  create ~meth ~target ~headers ~peer:(Some peer) (Reqd reqd)

let make ?(meth = `GET) ?(headers = []) ?(body = "") target =
  create ~meth ~target ~headers:(Httpun.Headers.of_list headers) ~peer:None (Fixed body)

let meth t = t.meth
let target t = t.target
let path t = t.path
let segments t = t.segments
let headers t = t.headers
let peer t = t.peer
let header t name = Httpun.Headers.get t.headers name
let query_all t = t.query
let query t key = List.filter_map (fun (k, v) -> if k = key then Some v else None) t.query

(* Bridge httpun's callbacks to a blocking read: chunks arrive in the
   connection's read task, the handler's task waits on a channel. *)
let read_reqd ~max reqd =
  let body = Httpun.Reqd.request_body reqd in
  let buf = Buffer.create 1024 in
  let finished = Goo.Chan.create () in
  let send r = Goo.protect (fun () -> Goo.Chan.send finished r) in
  let rec next () =
    Httpun.Body.Reader.schedule_read body
      ~on_eof:(fun () -> send (Ok (Buffer.contents buf)))
      ~on_read:(fun bs ~off ~len ->
        if Buffer.length buf + len > max then begin
          Httpun.Body.Reader.close body;
          send (Error Body_too_large)
        end
        else begin
          Buffer.add_string buf (Bigstringaf.substring bs ~off ~len);
          next ()
        end)
  in
  next ();
  match Goo.Chan.recv finished with Ok s -> s | Error e -> raise e

let body ?(max = 1 lsl 20) t =
  match t.body with
  | `Read s -> s
  | `Too_large -> raise Body_too_large
  | `Unread -> (
      let declared = Option.bind (header t "content-length") int_of_string_opt in
      match (declared, t.source) with
      | Some n, _ when n > max ->
          t.body <- `Too_large;
          raise Body_too_large
      | _, Fixed s when String.length s > max ->
          t.body <- `Too_large;
          raise Body_too_large
      | _, Fixed s ->
          t.body <- `Read s;
          s
      | _, Reqd reqd -> (
          match read_reqd ~max reqd with
          | s ->
              t.body <- `Read s;
              s
          | exception Body_too_large ->
              t.body <- `Too_large;
              raise Body_too_large))

let form ?max t = Codec.parse_query (body ?max t)

(* An unread body would stall the connection's next request: discard it
   without blocking. *)
let drain t =
  match (t.body, t.source) with
  | `Unread, Reqd reqd ->
      t.body <- `Read "";
      let body = Httpun.Reqd.request_body reqd in
      let rec next () =
        if not (Httpun.Body.Reader.is_closed body) then
          Httpun.Body.Reader.schedule_read body ~on_eof:ignore ~on_read:(fun _ ~off:_ ~len:_ -> next ())
      in
      next ()
  | _ -> ()
