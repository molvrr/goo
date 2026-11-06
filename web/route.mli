(** Typed routes.

    {[
      let open Web.Route in
      [
        get (nil @-> fun _req -> Response.text "home");
        get (s "users" / int /? nil @-> fun _req id -> ...);
        delete (s "users" / string / s "delete" /? nil @-> fun _req name -> ...);
        get (s "search" /? nil /& Q.string "q" /& Q.default 1 (Q.int "page")
             @-> fun _req q page -> ...);
      ]
    ]}

    A path is built from segment combinators joined by [/], closed by [/?]
    with [nil] (exact) or [trail] (also accepts a trailing slash), and
    optionally followed by typed query parameters with [/&]. Each capture
    and parameter adds an argument to the handler, after the request, in
    order; a handler of the wrong type is a compile error.

    The operators shadow integer division where this module is opened;
    prefer a local open ([Route.(...)]) around route definitions. *)

type ('f, 'r) path
(** A path whose handler has type ['f], producing ['r]. *)

(** {1 Segments} *)

type 'a conv
(** How a captured value is parsed from and printed into a URL. *)

val conv : name:string -> parse:(string -> 'a option) -> print:('a -> string) -> 'a conv

val s : string -> ('f, 'r) path -> ('f, 'r) path
(** A literal segment. *)

val string : ('f, 'r) path -> (string -> 'f, 'r) path
(** A segment, percent-decoded, other than [""], ["."] and [".."]. It may
    still contain anything else, including ["/"] (from [%2F]), ["\\"] and
    NUL: use {!filename} for a value that names a file. *)

val filename : ('f, 'r) path -> (string -> 'f, 'r) path
(** A segment that is a single, non-hidden file name, safe to join to a
    directory: percent-decoded, non-empty, not starting with ["."] (so
    neither ["."] nor [".."]), and without ["/"], ["\\"], NUL or control
    characters. It still doesn't stop a symlink inside the directory from
    pointing outside it. *)

val int : ('f, 'r) path -> (int -> 'f, 'r) path
(** A decimal integer segment (optional leading [-]). *)

val custom : 'a conv -> ('f, 'r) path -> ('a -> 'f, 'r) path

val nil : ('r, 'r) path
(** End of the path: no more segments. On its own it matches ["/"]. *)

val trail : ('r, 'r) path
(** Like [nil] but also accepts a trailing slash. *)

val rest : (string list -> 'r, 'r) path
(** End of the path, capturing every remaining segment, percent-decoded
    and unchecked (possibly none, and [[""]] for a trailing slash):
    [get (s "docs" /? rest @-> fun _req segs -> ...)]. Tried only when no
    route without [rest] matches, deepest prefix first. The segments may be
    anything a client sends, [".."] included: {!Static.serve} validates
    them. *)

val ( / ) :
  (('b, 'r) path -> ('c, 'r) path) -> (('a, 'r) path -> ('b, 'r) path) -> ('a, 'r) path -> ('c, 'r) path

val ( /? ) : (('a, 'r) path -> ('b, 'r) path) -> ('a, 'r) path -> ('b, 'r) path

val ( // ) : ('a, 'b) path -> ('b, 'c) path -> ('a, 'c) path
(** [a // b]: [a]'s segments, then [b]'s, captures in that order; [a]'s
    end ([nil]) just joins them. For printing the URL of a route in a
    {!scope}: [url (org () // members ()) 7]. Parenthesize each side. *)

(** {1 Query parameters} *)

module Q : sig
  type 'a t

  val string : string -> string t
  val int : string -> int t
  val bool : string -> bool t
  (** ["true"] or ["false"]. *)

  val custom : 'a conv -> string -> 'a t

  val opt : 'a t -> 'a option t
  val default : 'a -> 'a t -> 'a t
  val list : 'a t -> 'a list t
  (** Every occurrence of the key ([?tag=a&tag=b]); possibly empty. *)

  val flag : string -> bool t
  (** Present (with no value, ["1"], ["true"] or ["on"]) or absent. *)
end

val ( /& ) : ('f, 'a -> 'r) path -> 'a Q.t -> ('f, 'r) path
(** Add a query parameter. A missing or malformed required parameter
    answers 400. *)

(** {1 Routes} *)

type ('ctx, 'r) endpoint
(** A path bound to a handler producing ['r], in a scope whose routes get
    a ['ctx]. *)

type 'ctx route
(** A route whose handler is given a ['ctx] by the {!scope} it is in.
    Routes given to {!App} are [unit route]s: outside any scope, or in
    scopes nested in a [unit] one. *)

val ( @-> ) : ('f, 'r) path -> (Request.t -> 'f) -> ('ctx, 'r) endpoint
(** A handler that doesn't use the scope's context. *)

val ( @=> ) : ('f, 'r) path -> (Request.t -> 'ctx -> 'f) -> ('ctx, 'r) endpoint
(** A handler given the scope's context, before the captures:
    [get (s "members" /? nil @=> fun _req org -> ...)]. *)

val get : ('ctx, Response.t) endpoint -> 'ctx route
(** Also answers HEAD, without the body. *)

val post : ('ctx, Response.t) endpoint -> 'ctx route
val put : ('ctx, Response.t) endpoint -> 'ctx route
val patch : ('ctx, Response.t) endpoint -> 'ctx route
val delete : ('ctx, Response.t) endpoint -> 'ctx route
val any : ('ctx, Response.t) endpoint -> 'ctx route
(** Any method. *)

val meth : Httpun.Method.t -> ('ctx, Response.t) endpoint -> 'ctx route

val ws :
  ?headers:(string * string) list -> ?max_message:int -> ('ctx, Websocket.t -> unit) endpoint -> 'ctx route
(** A WebSocket route: the handler takes the socket after the captures and
    query parameters, and its body is the session.
    {[
      ws (s "room" / string /? nil @-> fun _req room ws -> ...)
    ]}
    It answers GET only (RFC 6455 4.1) and only requests asking for the
    upgrade, which try it before other routes on the same path. So a [get]
    route on that path can serve the page: a plain request falls through to
    it, and if nothing else matches the answer is 426 Upgrade Required. *)

(** {1 Scopes} *)

type middleware = (Request.t -> Response.t) -> Request.t -> Response.t
(** The same as {!App.middleware}. *)

val scope :
  ?middleware:middleware list ->
  ('f, ('ctx, Response.t) result) path ->
  (Request.t -> 'outer -> 'f) ->
  'ctx route list ->
  'outer route list
(** [scope prefix build routes] mounts [routes] under [prefix], a path
    ending with [nil] whose captures go to [build] (with the request and the
    enclosing scope's context, [()] at the top). [build] returns the context
    for [routes], or a response to send instead (e.g. a 404 when the
    captured id doesn't exist):
    {[
      scope ~middleware:[ require_login ] (s "orgs" / int /? nil)
        (fun _req () id -> match Org.find id with
           | Some org -> Ok org
           | None -> Error (Response.text ~status:`Not_found "no such org"))
        [ get (s "members" /? nil @=> fun _req org -> ...);
          delete (s "members" / int /? nil @=> fun _req org member -> ...) ]
    ]}

    [middleware] runs only when one of [routes] matched (never for a 404 or
    405), outermost first and around [build]: so it can refuse the request
    before anything is loaded. Middleware may pass a different request on
    to [build] and the handler; captures and query parameters were already
    decoded from the original one. Scopes nest: an inner scope's [build] gets
    the outer one's context. Query parameters in [prefix] are decoded like
    a route's. *)

val group :
  ?middleware:middleware list ->
  (('ctx, Response.t) result, ('ctx, Response.t) result) path ->
  'ctx route list ->
  'ctx route list
(** A {!scope} with no captures, passing the context through:
    [group ~middleware:[ admin_only ] (s "admin" /? nil) [ ... ]]. *)

(** {1 Reverse routing} *)

val url : ('f, string) path -> 'f
(** Print a URL: [url (s "users" / int /? nil) 42] is ["/users/42"], with
    segments and query values percent-encoded.

    @raise Invalid_argument if a captured value would be refused by its own
    capture (e.g. ["../x"] for {!filename}), or a segment would be [""],
    ["."] or [".."], which clients resolve away whatever the encoding: either
    way the URL couldn't reach the route.

    Because of OCaml's value restriction, a path bound with [let] and used
    both as a route and with [url] must be a function:
    [let user () = s "users" / int /? nil]. *)

val pattern : ('f, 'r) path -> string
(** For logs: ["/users/:int/delete?page=<int>"]. *)

val route_pattern : 'ctx route -> string

(**/**)

type outcome =
  | Matched of (unit -> Response.t)
  | Wrong_method of Httpun.Method.t
  | Needs_upgrade
  | Bad_query of string
  | No_match

val matches : unit route -> Request.t -> outcome

type key = Key_lit of string | Key_cap of { id : int; accepts : string -> bool }
type ending = End_exact of bool  (** accepts a trailing slash *) | End_rest

val keys : 'ctx route -> key list * ending
val is_ws : 'ctx route -> bool
val wants_upgrade : Request.t -> bool
