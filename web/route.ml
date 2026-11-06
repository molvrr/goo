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

let string_conv = conv ~name:"string" ~parse:(fun s -> if s = "" then None else Some s) ~print:Fun.id
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

let s lit rest = Lit (lit, rest)
let custom c rest = Cap (c, rest)
let string rest = Cap (string_conv, rest)
let int rest = Cap (int_conv, rest)
let nil = End false
let trail = End true
let ( / ) a b rest = a (b rest)
let ( /? ) a terminator = a terminator
let ( /& ) p q = Query (p, q)

(* Matching builds the application of the handler without running it, so
   the handler only runs once the whole path and query have matched. *)
type 'k m = M_ok of 'k | M_bad of string | M_none

let rec run : type f r. (f, r) path -> string list -> (string -> string list) -> (f -> r) m =
 fun p segs q ->
  match p with
  | End trailing -> (
      match segs with
      | [] -> M_ok Fun.id
      | [ "" ] when trailing -> M_ok Fun.id
      | _ -> M_none)
  | Lit (l, rest) -> (
      match segs with x :: tl when String.equal x l -> run rest tl q | _ -> M_none)
  | Cap (c, rest) -> (
      match segs with
      | x :: tl -> (
          match c.parse x with
          | None -> M_none
          | Some v -> (
              match run rest tl q with
              | M_ok k -> M_ok (fun f -> k (f v))
              | (M_bad _ | M_none) as m -> m))
      | [] -> M_none)
  | Query (inner, param) -> (
      match run inner segs q with
      | M_ok k -> (
          match param.decode (q param.key) with
          | Ok v -> M_ok (fun f -> k f v)
          | Error e -> M_bad e)
      | (M_bad _ | M_none) as m -> m)

type 'r endpoint = Endpoint : ('f, 'r) path * (Request.t -> 'f) -> 'r endpoint

(* [finish] turns the handler's result into a response: the identity for
   HTTP routes, [Response.websocket] for [ws]. *)
type route =
  | Route : {
      meth : Httpun.Method.t option;
      upgrade : bool;  (* only matches WebSocket upgrade requests *)
      path : ('f, 'r) path;
      handler : Request.t -> 'f;
      finish : 'r -> Response.t;
    }
      -> route

let ( @-> ) p h = Endpoint (p, h)
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

let matches (Route { meth; upgrade; path; handler; finish }) req =
  match run path (Request.segments req) (Request.query req) with
  | M_none -> No_match
  | (M_ok _ | M_bad _) when (match meth with Some m -> m <> Request.meth req | None -> false) ->
      Wrong_method (Option.get meth)
  | (M_ok _ | M_bad _) when upgrade && not (wants_upgrade req) -> Needs_upgrade
  | M_bad e -> Bad_query e
  | M_ok k -> Matched (fun () -> finish (k (handler req)))

let url p =
  let rec go : type f r. (f, r) path -> (string list -> (string * string) list -> r) -> f =
   fun p k ->
    match p with
    | End _ -> k [] []
    | Lit (l, rest) -> go rest (fun segs qs -> k (l :: segs) qs)
    | Cap (c, rest) -> fun v -> go rest (fun segs qs -> k (c.print v :: segs) qs)
    | Query (inner, param) ->
        go inner (fun segs qs v -> k segs (qs @ List.map (fun x -> (param.key, x)) (param.encode v)))
  in
  go p (fun segs qs ->
      let path = "/" ^ String.concat "/" (List.map Codec.encode_segment segs) in
      if qs = [] then path else path ^ "?" ^ Codec.render_query qs)

let pattern p =
  let rec go : type f r. (f, r) path -> string list * string list = function
    | End _ -> ([], [])
    | Lit (l, rest) ->
        let segs, qs = go rest in
        (l :: segs, qs)
    | Cap (c, rest) ->
        let segs, qs = go rest in
        ((":" ^ c.name) :: segs, qs)
    | Query (inner, param) ->
        let segs, qs = go inner in
        (segs, qs @ [ Printf.sprintf "%s=<%s>" param.key param.desc ])
  in
  let segs, qs = go p in
  let path = "/" ^ String.concat "/" segs in
  if qs = [] then path else path ^ "?" ^ String.concat "&" qs

let route_pattern (Route { meth; upgrade; path; _ }) =
  (if upgrade then "WS" else match meth with Some m -> Httpun.Method.to_string m | None -> "*")
  ^ " " ^ pattern path

type key = Key_lit of string | Key_cap of { id : int; accepts : string -> bool }

let keys (Route { path = p; _ }) =
  let rec go : type f r. (f, r) path -> key list * bool = function
    | End trailing -> ([], trailing)
    | Lit (l, rest) ->
        let ks, t = go rest in
        (Key_lit l :: ks, t)
    | Cap (c, rest) ->
        let ks, t = go rest in
        (Key_cap { id = c.id; accepts = (fun s -> Option.is_some (c.parse s)) } :: ks, t)
    | Query (inner, _) -> go inner
  in
  go p

let is_ws (Route { upgrade; _ }) = upgrade
