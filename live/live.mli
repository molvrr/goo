(** Live pages: re-rendered on the browser (Datastar) whenever a table they
    read changes, whoever changed it.

    {[
      let live =
        Live.create ~db ~url
          Live.[ table "orders" ~keys:[ "id" ]; table "variants" ~keys:[ "product_id" ]; table "drops" ]

      let () = Live.start live (* inside Goo, at startup *)

      let show live =
        Live.page live
          ~keys:(fun req -> [ ("public.orders", "id", order_id req) ])
          ~shell:(fun _req live_part -> Html.(document (head [] []) (body [] [ live_part ])))
          ~data:(fun req conn -> Result.map view (Orders.find conn (order_id req)))
    ]}

    {b Opt-in, from code.} The app lists the tables live pages may watch;
    {!start} installs the triggers that announce their writes (no
    migration: the list is the truth, and live triggers on other tables are
    dropped). A render that reads a table not in the list is reported
    ([on_unwatched]): a page can't silently miss changes.

    {b Two modes.} [Notify]: statement-level triggers [pg_notify] on commit,
    one LISTEN connection per process wakes the pages: instant, and by row
    with [keys]. [Poll]: no triggers; Postgres' write counters
    (pg_stat_user_tables) are read every [poll_every] seconds and pages
    watching a table that was written wake (whole table, 1 to ~10 s late:
    Postgres publishes the counters lazily). Every NOTIFY goes through a
    global queue at commit, so writers pay for [Notify] on tables written
    very often; [Poll] costs them nothing, and pages wake at most once per
    poll however many writes there are.

    {b Auto.} A table declared [`Auto] (the default) starts in [Notify] and
    moves to [Poll] when it's written more than [max_writes_per_s] over
    [burst_window] seconds, and back after [calm_window] seconds under a
    quarter of that. Switching changes its triggers (under a short
    lock_timeout; retried if busy).

    {b Pages.} A normal GET renders the page; its live part asks for the
    same URL again with Datastar, which stays open as a stream of
    server-sent events. Each render runs in a transaction that records the
    tables it read ({!Reads}), and watches them; when one changes, it
    renders again and, if the HTML differs, sends it: Datastar morphs it
    into the page. The browser's request carries its cookies: every render
    is the user's, and runs in a READ ONLY transaction (a render that
    wrote would wake itself forever: it fails instead).

    {b Partitioned tables} are watched and read as their root: declare the
    parent, and write through it (a statement on a partition directly
    doesn't fire the parent's triggers; [Poll] sees it anyway).

    {b Keys} are compared as text, as Postgres prints them: a numeric
    printed as [1.0] doesn't match ["1"]. They must cover everything a page
    reads from the table, or it misses changes (a count of every row, say). *)

type mode =
  | Notify
  | Poll

type table

val table : ?keys:string list -> ?mode:[ `Auto | `Notify | `Poll ] -> string -> table
(** ["orders"] is ["public.orders"]. [keys]: columns whose values in the
    changed rows are announced, for pages that care about some rows only
    ({!page}'s [~keys]). *)

type t

val create :
  ?max_writes_per_s:float ->
  ?poll_every:float ->
  ?burst_window:float ->
  ?calm_window:float ->
  ?on_unwatched:[ `Fail | `Warn | `Ignore ] ->
  db:Caqti_goo.Domain_pool.t ->
  url:Uri.t ->
  table list ->
  t
(** Defaults: 20 writes/s, a poll every second, 60 s and 600 s windows,
    [`Warn] (logged once per page and tables). [url] is the database's,
    for the LISTEN connection: a direct connection or a session pooler,
    not a transaction pooler. *)

val start : t -> unit
(** Installs the triggers, then listens and polls in the background (in a
    scope of the calling task's) until {!stop}.
    @raise Failure if the triggers can't be installed (a table or key
    column that doesn't exist, a lock not granted in time...). *)

val stop : t -> unit

val modes : t -> (string * mode) list
val connected : t -> bool
(** Whether the LISTEN connection is up. *)

(** {1 Pages} *)

val page :
  t ->
  ?id:string ->
  ?settle:float ->
  ?every:float ->
  ?also:string list ->
  ?keys:(Web.Request.t -> Changes.key list) ->
  ?show_error:(([> Caqti_error.t ] as 'e) -> string) ->
  shell:(Web.Request.t -> [> `div ] Html.elt -> Html.document) ->
  data:(Web.Request.t -> Caqti_goo.connection -> ([< Html.flow ] Html.elt list, 'e) result) ->
  Web.Request.t ->
  Web.Response.t
(** A handler for a live page. [shell] is the page around the live part,
    rendered once; [data] is the live part's children, rendered again on
    every change (in a transaction: it mustn't open its own). [id] (default
    ["live"]) is the live part's, unique in the page; [settle] (default
    0.05 s) lets a burst of changes cause one render; [every] re-renders on
    a timer (for time, other services...); [also] watches more tables;
    [keys] narrows tables to rows, e.g.
    [fun _ -> [ ("public.orders", "id", id) ]] (the table needs those keys
    in {!table}, and they must cover everything the page reads from it). *)

val stream :
  t ->
  ?settle:float ->
  ?every:float ->
  ?also:string list ->
  ?keys:Changes.key list ->
  ?show_error:(([> Caqti_error.t ] as 'e) -> string) ->
  id:string ->
  data:(Web.Request.t -> Caqti_goo.connection -> ([< Html.flow ] Html.elt list, 'e) result) ->
  Web.Request.t ->
  (Datastar.Sse.event -> unit) ->
  unit
(** The stream behind {!page}: render, send if different, wait for a
    change, again, until cancelled. *)

(** {1 Pieces} *)

module Changes : sig
  type t
  type sub
  type key = string * string * string (** table, column, value *)

  val create : unit -> t
  val subscribe : t -> sub
  val unsubscribe : t -> sub -> unit
  val now : t -> int
  val watch : t -> sub -> since:int -> ?keys:key list -> string list -> unit
  val wait : ?settle:float -> sub -> unit
  val poke : sub -> unit
  val changed : t -> ?keys:(string * string list) list -> string -> unit
  val lost : t -> unit

  val watching : t -> string -> int
  (** How many subscribers watch the table. *)

  val subscribers : t -> int
end
with type t = Changes.t
 and type sub = Changes.sub

val changes : t -> Changes.t

module Reads : sig
  val track :
    ?writes:bool ->
    Caqti_goo.connection ->
    (Caqti_goo.connection -> ('a, ([> Caqti_error.call_or_retrieve | Caqti_error.transact ] as 'e)) result) ->
    ('a * string list, 'e) result
  (** [f conn]'s result and the tables it read (schema.table; partitions
      as their root), from Postgres' per-transaction scan counters. [f] runs
      in a transaction, READ ONLY unless [~writes:true], committed if it
      returns [Ok]. *)
end
