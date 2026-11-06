(** An HTTP response: status, headers and a body. *)

type t

val make : ?status:Httpun.Status.t -> ?headers:(string * string) list -> string -> t
val empty : Httpun.Status.t -> t
val text : ?status:Httpun.Status.t -> ?headers:(string * string) list -> string -> t
(** [text/plain; charset=utf-8]. *)

val html : ?status:Httpun.Status.t -> ?headers:(string * string) list -> Html.document -> t
(** A full page, [text/html; charset=utf-8], rendered with
    {!Html.render_document} (so it starts with [<!doctype html>]). *)

val html_elt : ?status:Httpun.Status.t -> ?headers:(string * string) list -> _ Html.elt -> t
(** A fragment rendered with {!Html.render}, e.g. for a partial page
    update. Same content type as {!html}. *)
val json : ?status:Httpun.Status.t -> ?headers:(string * string) list -> string -> t

val redirect : ?status:Httpun.Status.t -> string -> t
(** Default [303 See Other], which is right after a form POST. *)

val stream :
  ?status:Httpun.Status.t -> ?headers:(string * string) list -> ((string -> unit) -> unit) -> t
(** [stream f] sends a chunked body produced by [f write], running in the
    request's task. [write] blocks until the chunk is flushed to the socket
    (backpressure) and raises [Goo.Closed] once the client is gone. *)

val sse :
  ?headers:(string * string) list ->
  ?ping:float ->
  ((?event:string -> ?id:string -> ?retry:int -> string -> unit) -> unit) ->
  t
(** Server-sent events: [sse (fun send -> send ~event:"tick" "data"; ...)].
    Multi-line data is split into [data:] lines; [retry] is the client's
    reconnection delay in milliseconds. A comment is sent every
    [ping] seconds (default 15) so a disconnect is noticed even while [f] is
    idle; [f] is then cancelled. *)

val websocket : ?headers:(string * string) list -> ?max_message:int -> (Websocket.t -> unit) -> t
(** Accept a WebSocket upgrade and run the session in the request's task
    (see {!Websocket}). Messages over [max_message] bytes (default 1 MiB)
    close the connection with 1009. A request that isn't a valid upgrade is
    answered 400. *)

val with_header : string -> string -> t -> t
(** Add a header, replacing any existing one with that name. *)

val status : t -> Httpun.Status.t
val headers : t -> (string * string) list
val header : t -> string -> string option

val body_to_string : t -> string
(** The full body, running a stream to completion. For tests.
    @raise Invalid_argument for a websocket. *)

(**/**)

val send : ?upgrade:(Gluten.impl -> unit) -> t -> Httpun.Reqd.t -> unit
