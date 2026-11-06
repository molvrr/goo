(** WebSocket sessions, in direct style.

    {[
      Route.(get (s "ws" /? nil @-> fun _req ->
        Response.websocket (fun ws ->
          let rec loop () =
            match Websocket.recv ws with
            | Some (`Text s) -> Websocket.send ws (`Text ("echo: " ^ s)); loop ()
            | Some (`Binary _) -> loop ()
            | None -> ()
          in
          loop ())))
    ]}

    The session runs in the request's task on the connection's domain, and
    is cancelled if the connection ends while it's blocked elsewhere. Pings
    are answered, fragmented messages reassembled, and the closing handshake
    handled; returning from the session closes the connection normally. Use
    a session's [t] only from its own domain. *)

type t
type message = [ `Text of string | `Binary of string ]

val recv : t -> message option
(** The next complete message, or [None] once the connection is closed
    (and on every call after that). Blocks the calling task. *)

val send : t -> message -> unit
(** Queue a message. Doesn't wait for it to be written.
    @raise Goo.Closed if the connection is closed. *)

val ping : t -> unit

val close : ?code:int -> t -> unit
(** Start the closing handshake (default code 1000). Idempotent. *)

val is_closed : t -> bool

val connect : ?headers:(string * string) list -> ?max_message:int -> string -> (t -> 'a) -> 'a
(** [connect url session] opens a client connection ([ws://] or
    [http://]), runs [session] and closes the connection when it returns.
    @raise Failure if the handshake fails.
    @raise Invalid_argument for a header name that isn't a token or a value
    containing CR, LF or NUL. *)

(**/**)

val accept :
  ?headers:(string * string) list ->
  max_message:int ->
  upgrade:(Gluten.impl -> unit) ->
  Httpun.Reqd.t ->
  (t -> unit) ->
  (unit, string) result
