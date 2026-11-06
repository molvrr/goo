module R = Goo
module C = HttpunGoo.Client

exception Error of string
exception Timeout

type response = { status : Httpun.Status.t; headers : Httpun.Headers.t; body : string }

let status r = r.status
let headers r = r.headers
let header r name = Httpun.Headers.get r.headers name
let body r = r.body

type conn = { http : C.t; fd : Unix.file_descr; key : string }

(* Connections are started by the keeper task, so their I/O belongs to the
   pool's scope rather than to whichever task first asked for one. *)
type pool = {
  keeper : (unit -> unit) R.Chan.t;
  scope : R.scope;
  idle : (string, conn list) Hashtbl.t;
  max_idle : int;
  mutable closed : bool;
}

let retire c =
  C.shutdown c.http;
  R.close c.fd

let create_pool ?(max_idle_per_host = 8) () =
  let keeper = R.Chan.create () in
  let rec serve () =
    match R.Chan.recv keeper with
    | f ->
        f ();
        serve ()
    | exception R.Closed -> ()
  in
  { keeper; scope = R.scope [ serve ]; idle = Hashtbl.create 8; max_idle = max_idle_per_host; closed = false }

let close_pool p =
  if not p.closed then begin
    p.closed <- true;
    Hashtbl.iter (fun _ cs -> List.iter retire cs) p.idle;
    Hashtbl.reset p.idle;
    R.Chan.close p.keeper;
    R.cancel p.scope;
    R.protect (fun () -> try R.join p.scope with _ -> ())
  end

let with_pool ?max_idle_per_host f =
  let p = create_pool ?max_idle_per_host () in
  Fun.protect ~finally:(fun () -> close_pool p) (fun () -> f p)

let rec checkout pool (ep : Codec.endpoint) =
  let fresh () =
    let fd = Codec.connect ep.addr in
    let http =
      match pool with
      | None -> C.create_connection fd
      | Some p ->
          let reply = R.Chan.create () in
          R.Chan.send p.keeper (fun () -> R.Chan.send reply (C.create_connection fd));
          R.Chan.recv reply
    in
    { http; fd; key = ep.key }
  in
  match pool with
  | Some p when p.closed -> invalid_arg "Web.Client: pool is closed"
  | Some p -> (
      match Hashtbl.find_opt p.idle ep.key with
      | Some (c :: rest) ->
          Hashtbl.replace p.idle ep.key rest;
          (* The server may have closed it while idle. *)
          if C.is_closed c.http then (retire c; checkout pool ep) else c
      | _ -> fresh ())
  | None -> fresh ()

let checkin pool c =
  match pool with
  | Some p when (not p.closed) && not (C.is_closed c.http) ->
      let cs = Option.value ~default:[] (Hashtbl.find_opt p.idle c.key) in
      if List.length cs < p.max_idle then Hashtbl.replace p.idle c.key (c :: cs) else retire c
  | _ -> retire c

let describe = function
  | `Malformed_response s -> "malformed response: " ^ s
  | `Invalid_response_body_length _ -> "invalid response body length"
  | `Exn e -> Printexc.to_string e

(* Run [f], raising [Timeout] (and cancelling [f]) after [secs]. *)
let with_timeout secs f =
  match secs with
  | None -> f ()
  | Some secs ->
      let result = ref None and self = ref None in
      let s =
        R.scope
          [
            (fun () ->
              result := Some (f ());
              Option.iter R.cancel !self);
            (fun () ->
              R.sleep secs;
              raise Timeout);
          ]
      in
      self := Some s;
      R.join s;
      Option.get !result

(* Blocking reads over httpun's callback-based body reader. *)
let body_reader rbody =
  let finished = ref false in
  fun () ->
    if !finished then None
    else begin
      let next = R.Chan.create () in
      let send v = R.protect (fun () -> R.Chan.send next v) in
      Httpun.Body.Reader.schedule_read rbody
        ~on_eof:(fun () -> send None)
        ~on_read:(fun bs ~off ~len -> send (Some (Bigstringaf.substring bs ~off ~len)));
      let v = R.Chan.recv next in
      if v = None then finished := true;
      v
    end

let exchange ?pool ?(meth = `GET) ?(headers = []) ?body ?timeout url f =
  let ep = Codec.endpoint ~schemes:[ "http" ] url in
  let lower = List.map (fun (k, v) -> (String.lowercase_ascii k, v)) headers in
  let add k v hs = if List.mem_assoc k lower then hs else hs @ [ (k, v) ] in
  let headers =
    headers
    |> add "host" ep.host
    |> add "user-agent" "web-client"
    |> (match (body, meth) with
       | Some b, _ -> add "content-length" (string_of_int (String.length b))
       | None, (`POST | `PUT) -> add "content-length" "0"
       | None, _ -> Fun.id)
    |> (if Option.is_none pool then add "connection" "close" else Fun.id)
    |> Codec.check_headers
  in
  with_timeout timeout (fun () ->
      let c = checkout pool ep in
      match
        let result = R.Chan.create () in
        let send v = R.protect (fun () -> R.Chan.send result v) in
        let writer =
          C.request c.http
            (Httpun.Request.create ~headers:(Httpun.Headers.of_list headers) meth ep.target)
            ~error_handler:(fun e -> send (Stdlib.Error (describe e)))
            ~response_handler:(fun resp rbody -> send (Ok (resp, rbody)))
        in
        Option.iter (Httpun.Body.Writer.write_string writer) body;
        Httpun.Body.Writer.close writer;
        match R.Chan.recv result with
        | Stdlib.Error msg -> raise (Error msg)
        | Ok (resp, rbody) ->
            let read = body_reader rbody in
            let v = f { status = resp.Httpun.Response.status; headers = resp.headers; body = "" } read in
            (* Drain what [f] left, so the connection can be reused. *)
            let rec drain () = match read () with Some _ -> drain () | None -> () in
            drain ();
            v
      with
      | v ->
          checkin pool c;
          v
      | exception e ->
          R.protect (fun () -> retire c);
          raise e)

let stream ?pool ?meth ?headers ?body ?timeout url f = exchange ?pool ?meth ?headers ?body ?timeout url f

let request ?pool ?meth ?headers ?body ?timeout ?(max_body = 16 lsl 20) url =
  exchange ?pool ?meth ?headers ?body ?timeout url (fun resp read ->
      let b = Buffer.create 1024 in
      let rec go () =
        match read () with
        | Some chunk ->
            if Buffer.length b + String.length chunk > max_body then raise (Error "response body too large");
            Buffer.add_string b chunk;
            go ()
        | None -> { resp with body = Buffer.contents b }
      in
      go ())

let get ?pool ?headers ?timeout url = request ?pool ?headers ?timeout url
let post ?pool ?headers ?body ?timeout url = request ?pool ~meth:`POST ?headers ?body ?timeout url
