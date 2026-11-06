(* Percent-encoding for paths and [application/x-www-form-urlencoded] query
   strings. Hand-rolled: [Uri.query_of_encoded] splits values on commas. *)

let hex = "0123456789ABCDEF"

let is_unreserved = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
  | _ -> false

let encode ~keep s =
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if is_unreserved c || keep c then Buffer.add_char b c
      else begin
        Buffer.add_char b '%';
        Buffer.add_char b hex.[Char.code c lsr 4];
        Buffer.add_char b hex.[Char.code c land 15]
      end)
    s;
  Buffer.contents b

(* A path segment: '/' must be encoded; sub-delims and ':' '@' may stay.
   Callers keep "." and ".." out: no encoding survives a browser (WHATWG
   treats %2E%2E as ".." too). *)
let encode_segment =
  encode ~keep:(function
    | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' | ':' | '@' -> true
    | _ -> false)

let encode_query_part = encode ~keep:(fun _ -> false)

let unhex c =
  match c with
  | '0' .. '9' -> Some (Char.code c - 48)
  | 'a' .. 'f' -> Some (Char.code c - 87)
  | 'A' .. 'F' -> Some (Char.code c - 55)
  | _ -> None

(* Invalid escapes are kept literally. *)
let decode ?(plus = false) s =
  let n = String.length s in
  let b = Buffer.create n in
  let rec go i =
    if i < n then
      match s.[i] with
      | '%' when i + 2 < n -> (
          match (unhex s.[i + 1], unhex s.[i + 2]) with
          | Some h, Some l ->
              Buffer.add_char b (Char.chr ((h lsl 4) lor l));
              go (i + 3)
          | _ ->
              Buffer.add_char b '%';
              go (i + 1))
      | '+' when plus ->
          Buffer.add_char b ' ';
          go (i + 1)
      | c ->
          Buffer.add_char b c;
          go (i + 1)
  in
  go 0;
  Buffer.contents b

(* "/a/b%2Fc/" -> ["a"; "b/c"; ""]; "/" -> []. *)
let split_path path =
  let p = if String.starts_with ~prefix:"/" path then String.sub path 1 (String.length path - 1) else path in
  if p = "" then [] else List.map decode (String.split_on_char '/' p)

let parse_query qs =
  if qs = "" then []
  else
    String.split_on_char '&' qs
    |> List.filter (( <> ) "")
    |> List.map (fun kv ->
           match String.index_opt kv '=' with
           | Some i ->
               ( decode ~plus:true (String.sub kv 0 i),
                 decode ~plus:true (String.sub kv (i + 1) (String.length kv - i - 1)) )
           | None -> (decode ~plus:true kv, ""))

let render_query pairs =
  String.concat "&"
    (List.map (fun (k, v) -> encode_query_part k ^ "=" ^ encode_query_part v) pairs)

(* Header fields about to be sent. A CR or LF in a value would end the
   header early and let whoever controls it add headers or a body (response
   splitting, request smuggling); NUL is rejected by most peers. Names must
   be RFC 9110 tokens. *)
let is_tchar = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
  | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_' | '`' | '|' | '~' ->
      true
  | _ -> false

let check_header (name, value) =
  if name = "" || not (String.for_all is_tchar name) then
    invalid_arg (Printf.sprintf "invalid header name %S" name);
  if String.exists (function '\r' | '\n' | '\000' -> true | _ -> false) value then
    invalid_arg (Printf.sprintf "invalid value for header %s: contains CR, LF or NUL" name)

let check_headers headers =
  List.iter check_header headers;
  headers

(* "/users?x=1" -> ("/users", "x=1") *)
let split_target target =
  match String.index_opt target '?' with
  | Some i -> (String.sub target 0 i, String.sub target (i + 1) (String.length target - i - 1))
  | None -> (target, "")

(* "http://host:8080/a?b" -> address, Host header value, request target. *)
type endpoint = { addr : Unix.sockaddr; host : string; target : string; key : string }

let endpoint ~schemes url =
  let uri = Uri.of_string url in
  let scheme = Option.value ~default:"" (Uri.scheme uri) in
  if not (List.mem scheme schemes) then
    invalid_arg (Printf.sprintf "unsupported URL %S (expected %s)" url (String.concat " or " schemes));
  let host =
    match Uri.host uri with Some h when h <> "" -> h | _ -> invalid_arg ("URL without host: " ^ url)
  in
  let port = Option.value ~default:80 (Uri.port uri) in
  let host_header = if port = 80 then host else Printf.sprintf "%s:%d" host port in
  let target = match Uri.path_and_query uri with "" -> "/" | t -> t in
  (* Blocks this domain while resolving a name; IP literals don't. *)
  let addr =
    match Unix.inet_addr_of_string host with
    | ip -> Unix.ADDR_INET (ip, port)
    | exception Failure _ -> (
        match
          Unix.getaddrinfo host (string_of_int port) [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
        with
        | { Unix.ai_addr; _ } :: _ -> ai_addr
        | [] -> raise (Unix.Unix_error (Unix.EHOSTUNREACH, "getaddrinfo", host)))
  in
  { addr; host = host_header; target; key = Printf.sprintf "%s:%d" host port }

let connect addr =
  let fd = Unix.socket ~cloexec:true (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.set_nonblock fd;
  match Goo.connect fd addr with
  | () -> fd
  | exception e ->
      Unix.close fd;
      raise e
