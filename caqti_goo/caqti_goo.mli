(** Caqti on {!Goo}.

    A query waiting on the database suspends the calling task, not its
    domain: other tasks keep running meanwhile.

    {b Postgres only}, through the libpq-based [caqti-driver-postgresql]
    (link it in). The driver connects with a blocking call, once per new
    connection; queries and results don't block. Other drivers are not
    supported: sqlite3 works but blocks the domain on every step, and pgx
    needs sockets this backend doesn't provide.

    Connections belong to the domain that opened them: use a connection or
    pool only from that domain. {!Domain_pool} gives each domain its own
    pool. *)

module Stream : Caqti_stream_sig.S with type 'a fiber := 'a
module Pool : Caqti_pool_sig.S with type 'a fiber := 'a

module type CONNECTION =
  Caqti_connection_sig.S with type 'a fiber := 'a and type ('a, 'e) stream := ('a, 'e) Stream.t

type connection = (module CONNECTION)

(** Lifetime of connections and pools, and of a pool's idle-timeout
    timers, which run in tasks owned by the switch rather than by the request
    that set them. *)
module Switch : sig
  type t

  val run : (t -> 'a) -> 'a
  (** [run f] calls [f] with a fresh switch. When [f] returns or raises,
      closes everything opened on the switch (waiting for pooled connections
      in use to be returned) and cancels its timers. *)
end

include
  Caqti_connect_sig.S
    with type 'a fiber := 'a
     and type 'a with_switch := sw:Switch.t -> 'a
     and type 'a with_stdenv := 'a
     and type ('a, 'e) stream := ('a, 'e) Stream.t
     and type ('a, 'e) pool := ('a, 'e) Pool.t
     and type connection := connection

val or_fail : ('a, [< Caqti_error.t ]) result -> 'a

(** One connection pool per domain, opened on first use by each domain.

    {[
      let db = Caqti_goo.Domain_pool.create (Uri.of_string "postgresql:///app")

      let handler _req =
        Caqti_goo.Domain_pool.use db (fun (module C) -> C.find query ())
    ]}

    Each domain's pool lives in a task posted to it, so call {!close} at
    shutdown: until then [Goo.run_parallel] doesn't return. *)
module Domain_pool : sig
  type t

  val create :
    ?pool_config:Caqti_pool_config.t ->
    ?post_connect:(connection -> (unit, Caqti_error.t) result) ->
    ?subst:(Caqti_template.Dialect.t -> Caqti_template.Query.subst) ->
    ?env:(Caqti_driver_info.t -> string -> Caqti_query.t) ->
    ?config:Caqti_connect_config.t ->
    Uri.t ->
    t
  (** Describes the pools; opens nothing. [pool_config] applies to each
      domain's pool: [max_size] is per domain. *)

  val get : t -> (connection, Caqti_error.t) Pool.t
  (** The current domain's pool, opened on first use.
      @raise Caqti_error.Exn if the driver can't be loaded.
      @raise Invalid_argument after {!close}. *)

  val use :
    ?priority:float -> t -> (connection -> ('a, Caqti_error.t) result) -> ('a, Caqti_error.t) result
  (** [Pool.use] on the current domain's pool. *)

  val close : t -> unit
  (** Close every domain's pool and wait for them to be drained. Requests
      still waiting for a connection get errors. *)
end
