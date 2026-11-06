(** An incoming HTTP request. *)

type t

exception Body_too_large
(** Raised by {!body} and {!form} when the body exceeds [max]; the app
    answers 413. *)

val meth : t -> Httpun.Method.t
val target : t -> string
(** The raw request target, e.g. ["/users/42?tab=info"]. *)

val path : t -> string
(** The target without the query string, still percent-encoded. *)

val segments : t -> string list
(** Decoded path segments: ["/a/b%2Fc/"] is [["a"; "b/c"; ""]]. *)

val query : t -> string -> string list
(** All values of a query parameter, decoded, in order. Prefer typed
    parameters in routes ({!Route.Q}). *)

val query_all : t -> (string * string) list
val header : t -> string -> string option
val headers : t -> Httpun.Headers.t
val peer : t -> Unix.sockaddr option

val body : ?max:int -> t -> string
(** The whole request body (default limit 1 MiB), read on first call and
    cached. Blocks the calling task until it has arrived.
    @raise Body_too_large if it exceeds [max]. *)

val form : ?max:int -> t -> (string * string) list
(** The body parsed as [application/x-www-form-urlencoded]. *)

val make :
  ?meth:Httpun.Method.t -> ?headers:(string * string) list -> ?body:string -> string -> t
(** A request for tests, e.g. [make ~meth:`POST ~body:"x" "/echo"]. *)

(**/**)

val of_reqd : Httpun.Reqd.t -> Unix.sockaddr -> t
val drain : t -> unit
