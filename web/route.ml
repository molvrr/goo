(* [id] tells captures apart in the router's trie. *)
type 'a conv = { id : int; name : string; parse : string -> 'a option; print : 'a -> string }

let next_id = Atomic.make 0
let conv ~name ~parse ~print = { id = Atomic.fetch_and_add next_id 1; name; parse; print }

let parse_int s =
  let n = String.length s in
  let digits_from i =
    i < n
    && String.for_all (function '0' .. '9' -> true | _ -> false) (String.sub s i (n - i))
  in
  if digits_from (if n > 0 && s.[0] = '-' then 1 else 0) then int_of_string_opt s else None

let is_dot_segment = Codec.is_dot_segment
let string_conv = conv ~name:"string" ~parse:(fun s -> if is_dot_segment s then None else Some s) ~print:Fun.id

let is_filename = Codec.is_filename

let filename_conv = conv ~name:"filename" ~parse:(fun s -> if is_filename s then Some s else None) ~print:Fun.id
let int_conv = conv ~name:"int" ~parse:parse_int ~print:string_of_int

module Q = struct
  type 'a t = {
    key : string;
    desc : string;
    decode : string list -> ('a, string) result;
    encode : 'a -> string list;
  }

  let custom c key =
    {
      key;
      desc = c.name;
      decode =
        (function
        | [] -> Error (Printf.sprintf "missing query parameter %S" key)
        | v :: _ -> (
            match c.parse v with
            | Some x -> Ok x
            | None -> Error (Printf.sprintf "query parameter %S: expected %s, got %S" key c.name v)));
      encode = (fun x -> [ c.print x ]);
    }

  let string = custom (conv ~name:"string" ~parse:Option.some ~print:Fun.id)
  let int = custom int_conv

  let bool =
    custom
      (conv ~name:"bool"
         ~parse:(function "true" -> Some true | "false" -> Some false | _ -> None)
         ~print:string_of_bool)

  let opt p =
    {
      key = p.key;
      desc = p.desc ^ "?";
      decode = (function [] -> Ok None | l -> Result.map Option.some (p.decode l));
      encode = (function None -> [] | Some v -> p.encode v);
    }

  let default d p =
    { key = p.key; desc = p.desc; decode = (function [] -> Ok d | l -> p.decode l); encode = p.encode }

  let list p =
    {
      key = p.key;
      desc = p.desc ^ "*";
      decode =
        (fun vs ->
          List.fold_right
            (fun v acc ->
              match (p.decode [ v ], acc) with
              | Ok x, Ok xs -> Ok (x :: xs)
              | (Error _ as e), _ -> e
              | _, (Error _ as e) -> e)
            vs (Ok []));
      encode = List.concat_map p.encode;
    }

  let flag key =
    {
      key;
      desc = "flag";
      decode =
        (function
        | [] -> Ok false
        | ("" | "1" | "true" | "on") :: _ -> Ok true
        | ("0" | "false" | "off") :: _ -> Ok false
        | v :: _ -> Error (Printf.sprintf "query parameter %S: expected a flag, got %S" key v));
      encode = (function true -> [ "1" ] | false -> []);
    }
end

type ('f, 'r) path =
  | End : bool -> ('r, 'r) path  (* accepts a trailing slash *)
  | Lit : string * ('f, 'r) path -> ('f, 'r) path
  | Cap : 'a conv * ('f, 'r) path -> ('a -> 'f, 'r) path
  | Query : ('f, 'a -> 'r) path * 'a Q.t -> ('f, 'r) path
  | Rest : (string list -> 'r, 'r) path  (* every remaining segment *)
  | Concat : ('a, 'b) path * ('b, 'c) path -> ('a, 'c) path  (* [a]'s segments, then [b]'s *)

let s lit rest = Lit (lit, rest)
let custom c rest = Cap (c, rest)
let string rest = Cap (string_conv, rest)
let int rest = Cap (int_conv, rest)
let filename rest = Cap (filename_conv, rest)
let nil = End false
let trail = End true
let rest = Rest
let ( / ) a b rest = a (b rest)
let ( /? ) a terminator = a terminator
let ( /& ) p q = Query (p, q)
let ( // ) a b = Concat (a, b)

(* Matching builds the application of the handler without running it, so
   the handler only runs once the whole path and query have matched. It
   returns the segments left over: the end of a path that is followed by
   more ([final = false]: a scope's prefix, the left of [//]) leaves them to
   what comes next; a final end must have consumed them all. *)
type 'k m = M_ok of 'k | M_bad of string * string list | M_none

let rec run : type f r.
    (f, r) path -> final:bool -> string list -> (string -> string list) -> ((f -> r) * string list) m =
 fun p ~final segs q ->
  match p with
  | End trailing -> (
      if not final then M_ok (Fun.id, segs)
      else match segs with [] -> M_ok (Fun.id, []) | [ "" ] when trailing -> M_ok (Fun.id, []) | _ -> M_none)
  | Lit (l, rest) -> ( match segs with x :: tl when String.equal x l -> run rest ~final tl q | _ -> M_none)
  | Cap (c, rest) -> (
      match segs with
      | x :: tl -> (
          match c.parse x with
          | None -> M_none
          | Some v -> (
              match run rest ~final tl q with
              | M_ok (k, left) -> M_ok ((fun f -> k (f v)), left)
              | (M_bad _ | M_none) as m -> m))
      | [] -> M_none)
  | Rest -> M_ok ((fun f -> f segs), [])
  | Query (inner, param) -> (
      match run inner ~final segs q with
      | M_ok (k, left) -> (
          match param.decode (q param.key) with
          | Ok v -> M_ok ((fun f -> k f v), left)
          | Error e -> M_bad (e, left))
      | (M_bad _ | M_none) as m -> m)
  | Concat (a, b) -> (
      match run a ~final:false segs q with
      | M_none -> M_none
      | M_ok (k1, left) -> (
          match run b ~final left q with
          | M_ok (k2, left) -> M_ok ((fun f -> k2 (k1 f)), left)
          | (M_bad _ | M_none) as m -> m)
      | M_bad (e, left) -> (
          (* Reported only if the rest matches too. *)
          match run b ~final left q with
          | M_none -> M_none
          | M_ok (_, left) | M_bad (_, left) -> M_bad (e, left)))

type middleware = (Request.t -> Response.t) -> Request.t -> Response.t

type ('ctx, 'r) endpoint = Endpoint : ('f, 'r) path * (Request.t -> 'ctx -> 'f) -> ('ctx, 'r) endpoint

(* A route's handler gets the context of the scope it is in ([unit] at the
   top). [finish] turns the handler's result into a response: the identity
   for HTTP routes, [Response.websocket] for [ws]. A [Scope] matches its
   prefix, builds its routes' context from the captures and the outer
   context, and wraps them in its middleware. *)
type 'ctx route =
  | Route : {
      meth : Httpun.Method.t option;
      upgrade : bool;  (* only matches WebSocket upgrade requests *)
      path : ('f, 'r) path;
      handler : Request.t -> 'ctx -> 'f;
      finish : 'r -> Response.t;
    }
      -> 'ctx route
  | Scope : {
      prefix : ('f, ('ctx, Response.t) result) path;
      build : Request.t -> 'outer -> 'f;
      middleware : middleware list;
      inner : 'ctx route;
    }
      -> 'outer route

let ( @-> ) p h = Endpoint (p, fun req _ -> h req)
let ( @=> ) p h = Endpoint (p, h)
let meth m (Endpoint (path, handler)) = Route { meth = Some m; upgrade = false; path; handler; finish = Fun.id }
let get e = meth `GET e
let post e = meth `POST e
let put e = meth `PUT e
let patch e = meth (`Other "PATCH") e
let delete e = meth `DELETE e

let any (Endpoint (path, handler)) =
  Route { meth = None; upgrade = false; path; handler; finish = Fun.id }

let ws ?headers ?max_message (Endpoint (path, handler)) =
  Route
    {
      meth = Some `GET;  (* RFC 6455 4.1 *)
      upgrade = true;
      path;
      handler;
      finish = Response.websocket ?headers ?max_message;
    }

let scope ?(middleware = []) prefix build routes =
  List.map (fun inner -> Scope { prefix; build; middleware; inner }) routes

let group ?middleware prefix routes = scope ?middleware prefix (fun _req ctx -> Ok ctx) routes

let wants_upgrade req =
  match Request.header req "upgrade" with
  | Some v -> List.mem "websocket" (List.map (fun s -> String.lowercase_ascii (String.trim s)) (String.split_on_char ',' v))
  | None -> false

type outcome =
  | Matched of (unit -> Response.t)
  | Wrong_method of Httpun.Method.t
  | Needs_upgrade
  | Bad_query of string
  | No_match

(* HEAD is GET without the body (RFC 9110 9.3.2): GET routes answer it, and
   [Response.send] leaves the body out. *)
let meth_matches m req_meth = m = req_meth || (m = `GET && req_meth = `HEAD)

type 'k found = F_ok of 'k | F_wrong_method of Httpun.Method.t | F_upgrade | F_bad of string | F_none

(* What runs on a match takes the request again: scope middleware may hand
   its own to what it wraps. *)
let rec find : type ctx. ctx route -> Request.t -> string list -> (Request.t -> ctx -> Response.t) found =
 fun r req segs ->
  let q = Request.query req in
  match r with
  | Route { meth; upgrade; path; handler; finish } -> (
      match run path ~final:true segs q with
      | M_none -> F_none
      | (M_ok _ | M_bad _) when (match meth with Some m -> not (meth_matches m (Request.meth req)) | None -> false)
        ->
          F_wrong_method (Option.get meth)
      | (M_ok _ | M_bad _) when upgrade && not (wants_upgrade req) -> F_upgrade
      | M_bad (e, _) -> F_bad e
      | M_ok (k, _) -> F_ok (fun req ctx -> finish (k (handler req ctx))))
  | Scope { prefix; build; middleware; inner } -> (
      match run prefix ~final:false segs q with
      | M_none -> F_none
      | M_bad (e, left) -> (
          match find inner req left with
          | F_ok _ -> F_bad e
          | (F_wrong_method _ | F_upgrade | F_bad _ | F_none) as o -> o)
      | M_ok (k, left) -> (
          match find inner req left with
          | F_ok run ->
              F_ok
                (fun req outer ->
                  let core req = match k (build req outer) with Ok ctx -> run req ctx | Error resp -> resp in
                  List.fold_right (fun m h -> m h) middleware core req)
          | (F_wrong_method _ | F_upgrade | F_bad _ | F_none) as o -> o))

let matches r req =
  match find r req (Request.segments req) with
  | F_ok run -> Matched (fun () -> run req ())
  | F_wrong_method m -> Wrong_method m
  | F_upgrade -> Needs_upgrade
  | F_bad e -> Bad_query e
  | F_none -> No_match

let url p =
  let rec go : type f r. (f, r) path -> (string list -> (string * string) list -> r) -> f =
   fun p k ->
    match p with
    | End _ -> k [] []
    | Lit (l, rest) -> go rest (fun segs qs -> k (l :: segs) qs)
    | Cap (c, rest) ->
        fun v ->
          let seg = c.print v in
          (* A value its own capture would refuse makes a dead link. *)
          if Option.is_none (c.parse seg) then
            invalid_arg (Printf.sprintf "Route.url: %S isn't a valid %s segment" seg c.name);
          go rest (fun segs qs -> k (seg :: segs) qs)
    | Query (inner, param) ->
        go inner (fun segs qs v -> k segs (qs @ List.map (fun x -> (param.key, x)) (param.encode v)))
    | Rest -> fun segs -> k segs []
    | Concat (a, b) -> go a (fun s1 q1 -> go b (fun s2 q2 -> k (s1 @ s2) (q1 @ q2)))
  in
  go p (fun segs qs ->
      List.iter
        (fun seg ->
          if is_dot_segment seg then
            invalid_arg (Printf.sprintf "Route.url: %S can't be a path segment" seg))
        segs;
      let path = "/" ^ String.concat "/" (List.map Codec.encode_segment segs) in
      if qs = [] then path else path ^ "?" ^ Codec.render_query qs)

let rec pattern_parts : type f r. (f, r) path -> string list * string list = function
  | End _ -> ([], [])
  | Lit (l, rest) ->
      let segs, qs = pattern_parts rest in
      (l :: segs, qs)
  | Cap (c, rest) ->
      let segs, qs = pattern_parts rest in
      ((":" ^ c.name) :: segs, qs)
  | Query (inner, param) ->
      let segs, qs = pattern_parts inner in
      (segs, qs @ [ Printf.sprintf "%s=<%s>" param.key param.desc ])
  | Rest -> ([ "*" ], [])
  | Concat (a, b) ->
      let s1, q1 = pattern_parts a and s2, q2 = pattern_parts b in
      (s1 @ s2, q1 @ q2)

let render_pattern (segs, qs) =
  let path = "/" ^ String.concat "/" segs in
  if qs = [] then path else path ^ "?" ^ String.concat "&" qs

let pattern p = render_pattern (pattern_parts p)

let rec route_parts : type ctx. ctx route -> string * (string list * string list) = function
  | Route { meth; upgrade; path; _ } ->
      ( (if upgrade then "WS" else match meth with Some m -> Httpun.Method.to_string m | None -> "*"),
        pattern_parts path )
  | Scope { prefix; inner; _ } ->
      let s1, q1 = pattern_parts prefix and label, (s2, q2) = route_parts inner in
      (label, (s1 @ s2, q1 @ q2))

let route_pattern r =
  let label, parts = route_parts r in
  label ^ " " ^ render_pattern parts

type key = Key_lit of string | Key_cap of { id : int; accepts : string -> bool }

type ending = End_exact of bool | End_rest

let rec path_keys : type f r. (f, r) path -> key list * ending = function
  | End trailing -> ([], End_exact trailing)
  | Rest -> ([], End_rest)
  | Lit (l, rest) ->
      let ks, t = path_keys rest in
      (Key_lit l :: ks, t)
  | Cap (c, rest) ->
      let ks, t = path_keys rest in
      (Key_cap { id = c.id; accepts = (fun s -> Option.is_some (c.parse s)) } :: ks, t)
  | Query (inner, _) -> path_keys inner
  | Concat (a, b) ->
      let k1, _ = path_keys a and k2, t = path_keys b in
      (k1 @ k2, t)

let rec keys : type ctx. ctx route -> key list * ending = function
  | Route { path; _ } -> path_keys path
  | Scope { prefix; inner; _ } ->
      let k1, _ = path_keys prefix and k2, t = keys inner in
      (k1 @ k2, t)

let rec is_ws : type ctx. ctx route -> bool = function
  | Route { upgrade; _ } -> upgrade
  | Scope { inner; _ } -> is_ws inner
