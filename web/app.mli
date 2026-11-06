(** An application: routes, middleware and serving.

    {[
      let app =
        Web.App.create ()
        |> Web.App.route Web.Route.(get (s "users" / int /? nil @-> fun _req id -> ...))
        |> Web.App.use logger
      in
      Goo.run_parallel (fun i ->
          if i = 0 then ignore (Web.App.serve addr app))
    ]} *)

type handler = Request.t -> Response.t
type middleware = handler -> handler
type t

val create : unit -> t
val route : Route.route -> t -> t
(** Add a route. Lookup uses a trie ({!Router}): at each path segment a
    literal beats a capture, so [/users/new] reaches [s "users" / s "new"]
    even if [s "users" / string] was added first; otherwise routes are tried
    in the order they were added. *)

val routes : Route.route list -> t -> t

val use : middleware -> t -> t
(** Add middleware around every request, including 404s and errors. The
    first added is the outermost. *)

val not_found : handler -> t -> t
(** Replace the default 404 handler. *)

val handle : t -> handler
(** Dispatch a request through middleware and routes, without a network.

    No route matches: the 404 handler. A path matches but not the method:
    405 with [Allow]. Only a {!Route.ws} route matches but the request isn't
    an upgrade: 426. A required query parameter is missing or malformed:
    400. The body is too large: 413. The handler raises: the exception is
    logged and the answer is a generic 500. *)

val serve :
  ?config:Httpun.Config.t ->
  ?domains:Goo.domain array ->
  ?acceptors:int ->
  ?backlog:int ->
  ?strategy:[ `Auto | `Reuseport | `Shared ] ->
  Unix.sockaddr ->
  t ->
  Goo.Server.t
(** Serve over HTTP/1.1 on {!Goo.Server}. Each request runs in its own
    task on the domain that accepted the connection, so handlers can block;
    the task belongs to the connection and is cancelled with it. *)

val describe : t -> string list
(** The routes, e.g. ["GET /users/:int"], in matching order. *)
