(** Route lookup on a trie of path segments.

    At each segment a literal child is tried before captures, and captures
    in the order they were first added; the search backtracks when a branch
    fails deeper down. Routes ending at the same node are tried in the order
    they were added, except that a WebSocket upgrade request tries {!Route.ws}
    routes first. A path that matches with the wrong method doesn't stop
    the search, so another route can still take the request; if none does,
    the methods of every matching path are reported. *)

type t

type result =
  | Found of (unit -> Response.t)
      (** Run the handler (query parameters already decoded). *)
  | Bad_query of string
      (** The first route matching path and method rejected the query. *)
  | Upgrade_required
      (** Only a WebSocket route matched, and this isn't an upgrade request. *)
  | Not_found of string list
      (** Methods allowed on matching paths, sorted; empty for a 404. *)

val create : Route.route list -> t
val lookup : t -> Request.t -> result
