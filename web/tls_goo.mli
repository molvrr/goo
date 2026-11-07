(** TLS client sessions on {!Goo} sockets, for {!Client}'s [https://].

    Certificates are checked against Mozilla's CA bundle (compiled in from
    [ca-certs-nss], so it works in images without a system trust store)
    unless another authenticator is given. *)

type t

val config : ?authenticator:X509.Authenticator.t -> string -> Tls.Config.client
(** A client configuration for a host name (used for SNI and to check the
    certificate) or an IP literal. Seeds mirage-crypto's RNG on first use.
    @raise Failure if the host name is invalid. *)

val client : Tls.Config.client -> Unix.file_descr -> t
(** Run the handshake over a connected non-blocking socket. The descriptor
    is not closed.
    @raise Failure if the handshake fails (e.g. the certificate is rejected). *)

val transport : t -> HttpunGoo.transport
(** Plaintext I/O over the session. close_notify goes out once both
    [shutdown_send] and [shutdown_receive] have been called (no half-close). *)
