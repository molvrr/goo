(** Datastar on {!Web}: reading requests sent by Datastar actions, and
    answering with Datastar SSE events. *)

val is_datastar_request : Web.Request.t -> bool
(** [true] when the request was sent by a Datastar action. *)

val read_signals : Web.Request.t -> (Yojson.Safe.t, string) result
(** Signals sent with a Datastar action: from the [datastar] query
    parameter for GET/DELETE, from the JSON body otherwise. Reading the body
    blocks the calling task.
    @raise Web.Request.Body_too_large past the default body limit. *)

val sse :
  ?headers:(string * string) list -> ?ping:float -> ((Datastar.Sse.event -> unit) -> unit) -> Web.Response.t
(** [sse (fun send -> send (Datastar.Sse.patch_elements [...]); ...)]: a
    {!Web.Response.sse} stream of Datastar events. *)
