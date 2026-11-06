(* Content-Security-Policy builder. *)

type source =
  [ `none | `self | `unsafe_inline | `unsafe_eval | `strict_dynamic | `unsafe_hashes
  | `data | `blob | `https | `http
  | `host of string | `nonce of string | `sha256 of string ]

type sandbox_flag =
  [ `allow_downloads | `allow_forms | `allow_modals | `allow_popups
  | `allow_popups_to_escape_sandbox | `allow_same_origin | `allow_scripts
  | `allow_top_navigation | `allow_top_navigation_by_user_activation ]

type directive = string * string list
type t = directive list

(* Values end up inside a header: refuse anything that could end a source
   or a directive. *)
let token s =
  if s = "" || String.exists (fun c -> c = ';' || c = ',' || c = '\'' || c = '"' || c <= ' ' || c = '\127') s
  then invalid_arg ("Csp: invalid source " ^ s)
  else s

let string_of_source : source -> string = function
  | `none -> "'none'"
  | `self -> "'self'"
  | `unsafe_inline -> "'unsafe-inline'"
  | `unsafe_eval -> "'unsafe-eval'"
  | `strict_dynamic -> "'strict-dynamic'"
  | `unsafe_hashes -> "'unsafe-hashes'"
  | `data -> "data:"
  | `blob -> "blob:"
  | `https -> "https:"
  | `http -> "http:"
  | `host h -> token h
  | `nonce n -> "'nonce-" ^ token n ^ "'"
  | `sha256 h -> "'sha256-" ^ token h ^ "'"

(* An empty source list means nothing is allowed. *)
let fetch name (sources : source list) : directive =
  (name, match sources with [] -> ["'none'"] | s -> List.map string_of_source s)

let default_src = fetch "default-src"
let script_src = fetch "script-src"
let style_src = fetch "style-src"
let img_src = fetch "img-src"
let font_src = fetch "font-src"
let connect_src = fetch "connect-src"
let media_src = fetch "media-src"
let object_src = fetch "object-src"
let frame_src = fetch "frame-src"
let child_src = fetch "child-src"
let worker_src = fetch "worker-src"
let manifest_src = fetch "manifest-src"
let form_action = fetch "form-action"
let base_uri = fetch "base-uri"
let frame_ancestors = fetch "frame-ancestors"

let string_of_flag : sandbox_flag -> string = function
  | `allow_downloads -> "allow-downloads"
  | `allow_forms -> "allow-forms"
  | `allow_modals -> "allow-modals"
  | `allow_popups -> "allow-popups"
  | `allow_popups_to_escape_sandbox -> "allow-popups-to-escape-sandbox"
  | `allow_same_origin -> "allow-same-origin"
  | `allow_scripts -> "allow-scripts"
  | `allow_top_navigation -> "allow-top-navigation"
  | `allow_top_navigation_by_user_activation -> "allow-top-navigation-by-user-activation"

let sandbox flags : directive = ("sandbox", List.map string_of_flag flags)
let upgrade_insecure_requests : directive = ("upgrade-insecure-requests", [])

let to_string (t : t) =
  String.concat "; " (List.map (fun (name, values) -> String.concat " " (name :: values)) t)

let header t = ("content-security-policy", to_string t)
