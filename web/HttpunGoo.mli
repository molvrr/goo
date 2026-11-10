(** httpun on {!Goo}, mirroring [httpun-eio].

    {[
      let handler _peer { Gluten.reqd; _ } =
        Httpun.Reqd.respond_with_string reqd (Httpun.Response.create `OK) "hello"
      in
      Goo.run_main (fun () ->
          let server = HttpunGoo.Server.start addr handler in
          ...)
    ]}

    Each connection runs a read task and a write task on the domain that
    accepted it. The request handler is called from the read task: return
    quickly, and {!Goo.spawn} a task for responses that need to block
    (spawned tasks belong to the connection and are cancelled when it ends,
    including when the peer goes away). While
    a request is being handled, the reader watches for the peer closing the
    connection and reports it to httpun, so streaming responses end. *)

(** How a connection's bytes move: a plain socket ({!transport_of_fd}) or
    something layered on one, such as TLS. *)
type transport = {
  read : Bigstringaf.t -> int -> int -> int;
  (** [read buf off len] blocks until some bytes arrive; [0] means EOF. *)
  write : Bigstringaf.t -> int -> int -> int;
  (** Write some of the range and return how many bytes were taken. Raise
      [End_of_file] (or [Unix.Unix_error EPIPE]) once the peer is gone. *)
  watch_eof : unit -> [ `Eof | `Data ];
  (** Block until there is something to read ([`Data], without consuming
      it) or the peer has closed its side ([`Eof]). *)
  shutdown_receive : unit -> unit;
  shutdown_send : unit -> unit;
}

val transport_of_fd : Unix.file_descr -> transport

val run_protocol :
  (module Gluten.RUNTIME with type t = 't) -> read_buffer_size:int -> 't -> Unix.file_descr -> unit
(** Drive any gluten-style protocol state machine over a connected socket
    until it closes (e.g. [Httpun_ws.Client_connection]). The descriptor is
    not closed. *)

(** HTTP/1.1 client connections. *)
module Client : sig
  type t

  val create_connection : ?config:Httpun.Config.t -> Unix.file_descr -> t
  (** Start serving a connected socket. Its I/O runs in a new scope under the
      current task, so {!shutdown} it before that task's scope can finish.
      The descriptor is not closed. *)

  val create_connection_on : ?config:Httpun.Config.t -> transport -> t
  (** {!create_connection} over any {!transport}, e.g. a TLS session. *)

  val request :
    t ->
    ?flush_headers_immediately:bool ->
    Httpun.Request.t ->
    error_handler:Httpun.Client_connection.error_handler ->
    response_handler:Httpun.Client_connection.response_handler ->
    Httpun.Body.Writer.t

  val is_closed : t -> bool
  val upgrade : t -> Gluten.impl -> unit

  val shutdown : t -> unit
  (** Close the connection and wait for its I/O to stop. *)
end

module Server : sig
  type request_handler = Unix.sockaddr -> Httpun.Reqd.t Gluten.reqd -> unit
  type error_handler = Unix.sockaddr -> Httpun.Server_connection.error_handler

  val create_connection_handler :
    ?config:Httpun.Config.t ->
    ?error_handler:error_handler ->
    request_handler:request_handler ->
    Unix.file_descr ->
    Unix.sockaddr ->
    unit
  (** Serve one connection until it closes; suitable for
      {!Goo.Server.start}. Defaults to httpun's error handler. *)

  val start :
    ?config:Httpun.Config.t ->
    ?domains:Goo.domain array ->
    ?acceptors:int ->
    ?backlog:int ->
    ?strategy:[ `Auto | `Reuseport | `Shared ] ->
    ?error_handler:error_handler ->
    Unix.sockaddr ->
    request_handler ->
    Goo.Server.t
  (** {!Goo.Server.start} with {!create_connection_handler}. *)
end
