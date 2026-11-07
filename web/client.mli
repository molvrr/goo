(** HTTP/1.1 client for [http://] and [https://].

    {[
      let r = Web.Client.get "http://127.0.0.1:8080/users/42" in
      print_endline (Web.Client.body r);

      (* Reuse connections: *)
      Web.Client.with_pool (fun pool ->
          let a = Web.Client.get ~pool "http://api/x" in
          let b = Web.Client.post ~pool ~body:"{}" "http://api/y" in
          ...)
    ]}

    Calls block the calling task. Host names are resolved with
    [getaddrinfo], which blocks the domain briefly; IP literals don't.

    [https://] uses TLS 1.2/1.3 ([ocaml-tls]). The server certificate is
    checked against Mozilla's CA bundle compiled into the binary (so it
    works without a system trust store); pass [~authenticator] to use
    something else, e.g. [Ca_certs.authenticator ()] for the OS store or
    [X509.Authenticator.cert_fingerprint] to pin a certificate. A rejected
    certificate or failed handshake raises {!Error}. *)

exception Error of string
(** The server's response couldn't be parsed or the connection failed. *)

exception Timeout

type response

val status : response -> Httpun.Status.t
val headers : response -> Httpun.Headers.t
val header : response -> string -> string option
val body : response -> string

(** {1 Connection pools} *)

type pool
(** Keeps idle keep-alive connections per host. Use it from one domain. *)

val create_pool : ?max_idle_per_host:int -> unit -> pool
(** Its connections run in a scope under the current task: {!close_pool}
    before that task's scope can finish, or use {!with_pool}. *)

val close_pool : pool -> unit
val with_pool : ?max_idle_per_host:int -> (pool -> 'a) -> 'a

(** {1 Requests}

    Headers are checked as for {!Response}: [Invalid_argument] for a name
    that isn't a token or a value containing CR, LF or NUL. *)

val request :
  ?pool:pool ->
  ?authenticator:X509.Authenticator.t ->
  ?meth:Httpun.Method.t ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?timeout:float ->
  ?max_body:int ->
  string ->
  response
(** Send a request and read the whole response (at most [max_body] bytes,
    default 16 MiB). Without a pool the connection is closed afterwards.
    @raise Timeout after [timeout] seconds.
    @raise Error if the response is malformed or too large. *)

val get :
  ?pool:pool ->
  ?authenticator:X509.Authenticator.t ->
  ?headers:(string * string) list ->
  ?timeout:float ->
  string ->
  response

val post :
  ?pool:pool ->
  ?authenticator:X509.Authenticator.t ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?timeout:float ->
  string ->
  response

val stream :
  ?pool:pool ->
  ?authenticator:X509.Authenticator.t ->
  ?meth:Httpun.Method.t ->
  ?headers:(string * string) list ->
  ?body:string ->
  ?timeout:float ->
  string ->
  (response -> (unit -> string option) -> 'a) ->
  'a
(** [stream url f] calls [f response read] once the status and headers have
    arrived ([body response] is [""]); [read ()] blocks for the next chunk of
    the body and returns [None] at the end. Whatever [f] leaves unread is
    discarded. *)
