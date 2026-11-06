(** Content-Security-Policy, built from typed directives:
    {[
      Csp.(to_string [ default_src [`none]; style_src [`unsafe_inline]; img_src [`self; `data] ])
      (* default-src 'none'; style-src 'unsafe-inline'; img-src 'self' data: *)
    ]} *)

type source =
  [ `none | `self | `unsafe_inline | `unsafe_eval | `strict_dynamic | `unsafe_hashes
  | `data | `blob | `https | `http
  | `host of string        (** e.g. ["https://cdn.example.com"], ["*.example.com"] *)
  | `nonce of string
  | `sha256 of string ]    (** base64 hash *)

type sandbox_flag =
  [ `allow_downloads | `allow_forms | `allow_modals | `allow_popups
  | `allow_popups_to_escape_sandbox | `allow_same_origin | `allow_scripts
  | `allow_top_navigation | `allow_top_navigation_by_user_activation ]

type directive
type t = directive list

(** {1 Fetch directives}

    An empty list is ['none']. [`host], [`nonce] and [`sha256] values
    containing [;], [,], quotes or whitespace raise [Invalid_argument]. *)

val default_src : source list -> directive
val script_src : source list -> directive
val style_src : source list -> directive
val img_src : source list -> directive
val font_src : source list -> directive
val connect_src : source list -> directive
val media_src : source list -> directive
val object_src : source list -> directive
val frame_src : source list -> directive
val child_src : source list -> directive
val worker_src : source list -> directive
val manifest_src : source list -> directive

(** {1 Navigation and document} *)

val form_action : source list -> directive
val base_uri : source list -> directive
val frame_ancestors : source list -> directive

(** [sandbox []] sandboxes with no exceptions. *)
val sandbox : sandbox_flag list -> directive
val upgrade_insecure_requests : directive

(** {1 Output} *)

val to_string : t -> string

(** [("content-security-policy", to_string t)] *)
val header : t -> string * string
