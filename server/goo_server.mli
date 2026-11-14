(** TCP servers accepting on several domains of a {!Goo} runtime.

    {[
      Goo.run_main (fun () ->
          let server = Goo_server.start addr (fun fd _peer -> echo fd) in
          ...
          Goo_server.shutdown ~timeout:5. server)
    ]}

    Built on Goo's public API only. *)

type t

val start :
  ?domains:Goo.domain array ->
  ?acceptors:int ->
  ?backlog:int ->
  ?strategy:[ `Auto | `Reuseport | `Shared ] ->
  Unix.sockaddr ->
  (Unix.file_descr -> Unix.sockaddr -> unit) ->
  t
(** Listen on the address and accept on every given domain (default: all),
    with [acceptors] accept loops per domain (default 1). Returns once every
    domain is listening. [backlog] (default 4096; the kernel caps it, at
    net.core.somaxconn on Linux) is how many connections may wait to be
    accepted: a burst of new connections beyond it is dropped.

    Each connection runs [handler fd peer] as its own task on the domain
    that accepted it; [fd] is non-blocking and is closed by the runtime
    when the handler ends, so don't close it yourself (use
    [Unix.shutdown] to end it early). A handler exception is logged and
    only affects its own connection.

    [strategy]: [`Reuseport] gives each domain its own [SO_REUSEPORT]
    socket so the kernel spreads connections (balanced on Linux only);
    [`Shared] has all domains accept on one socket (every domain wakes per
    connection, but idle domains naturally take more). [`Auto] (default)
    picks [`Reuseport] when {!Platform.reuseport_balances}.
    @raise Unix.Unix_error if binding fails on any domain. *)

val addr : t -> Unix.sockaddr
(** The bound address (with the actual port when binding port 0). *)

val shutdown : ?timeout:float -> t -> unit
(** Stop accepting on every domain, close the listening sockets, then wait
    for open connections to finish. With [timeout], connections still open
    after that many seconds are cancelled. Calling it again just waits. *)

val wait : t -> unit
(** Wait until the server has been shut down and all connections ended. *)
