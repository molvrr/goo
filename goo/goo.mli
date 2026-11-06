(** A single-threaded, cooperative scheduler built on effects.

    {[
      Goo.run (fun () ->
          let server = Goo.scope [ accept_loop; ticker ] in
          Goo.sleep 10.;
          Goo.cancel server;
          Goo.join server)
    ]}

    Tasks only switch at blocking points ({!sleep}, {!yield}, I/O, {!join},
    {!spawn}, {!recv}); a loop that never blocks starves every other task and
    can't be cancelled, so long CPU work should call {!yield}. *)

(** {1 Running} *)

val run : (unit -> unit) -> unit
(** [run main] is [run_parallel ~domains:1 (fun _ -> main ())]. *)

val run_parallel : ?domains:int -> (int -> unit) -> unit
(** [run_parallel ~domains:n main] starts [n] schedulers (default
    [Domain.recommended_domain_count ()]), one per domain, the first on the
    calling domain, and runs [main i] on scheduler [i]. Returns once all work
    has finished: every [main i] and everything spawned, {!post}ed or
    {!await_on}ed under it.

    A failure in any [main i] (or its tasks) cancels every scheduler and is
    re-raised. Ignores [SIGPIPE] so writes to closed peers raise [EPIPE].

    Tasks never move between domains: file descriptors, {!sub}s and scopes
    belong to the scheduler that uses them. Domains interact through {!post},
    {!await_on} and {!Chan}; {!cancel}, {!kill} and {!join} also work on
    scopes owned by other domains. *)

(** {1 Domains} *)

type domain
(** One scheduler in a {!run_parallel}. *)

val self : unit -> domain
val domains : unit -> domain array
val index : domain -> int

val post : domain -> (unit -> unit) -> unit
(** Start [f] as a task on another (or the same) domain, in its own scope,
    without waiting. Failures are logged to stderr, not propagated.
    @raise Invalid_argument once the runtime is shutting down. *)

val await_on : domain -> (unit -> 'a) -> 'a
(** Run [f] on the given domain and wait for its result or exception. If the
    caller is cancelled (or killed) meanwhile, so is [f], and the caller waits
    for it to wind down before getting {!Cancelled}. Runs [f] directly when
    the domain is the current one. *)

val spawn : (unit -> unit) -> unit
(** Start a task in the current task's scope. *)

val yield : unit -> unit
(** Let other tasks run. A cancellation point. *)

val sleep : float -> unit
(** Block for the given number of seconds. *)

(** {1 Scopes and cancellation}

    Every task belongs to a scope. Tasks spawned from a task join its scope,
    and scopes created from a task are children of its scope, so cancelling a
    scope reaches everything started under it. *)

type scope

exception Cancelled
(** Raised in a task when its scope is cancelled: at the point where it is
    blocked, or at its next blocking point. It may be caught for cleanup, and
    is raised again at every later blocking point outside {!protect}. *)

val scope : (unit -> unit) list -> scope
(** Start the tasks concurrently in a new child scope of the current one. The
    first task failure (other than {!Cancelled}) is recorded and cancels the
    scope. *)

val join : scope -> unit
(** Wait until every task in the scope and its child scopes has finished,
    including their {!defer}s, then re-raise the scope's first failure, if
    any. Cancellation alone is not a failure.
    @raise Invalid_argument when joining the current task's own scope. *)

val spawn_in : scope -> (unit -> unit) -> unit
(** Start a task in the given scope, on the scope's domain, instead of the
    current task's: it is joined, cancelled and failed with that scope, not
    the caller's. For background work owned by something longer-lived than
    the request that triggers it. Doesn't block and isn't a cancellation
    point, so it can be called from cleanup. The scope must still be running
    (a scope finishes once all its tasks have), so keep one alive with a task
    that waits until shutdown.
    @raise Invalid_argument if the scope, on the current domain, has
    finished. From another domain the check happens there: a task for a
    finished scope is dropped and logged to stderr. *)

val scoped : (unit -> unit) list -> unit
(** [scope] then [join]. If the caller is cancelled while waiting, the tasks
    are cancelled and waited for before {!Cancelled} is re-raised. *)

val cancel : scope -> unit
(** Cancel the scope and its children: their tasks get {!Cancelled} and unwind
    normally, running [try]/[with] and [Fun.protect ~finally]. Does not wait;
    use {!join}. *)

val kill : scope -> unit
(** Hard cancel: tasks are stopped at their next blocking point and dropped
    without unwinding, so they can't swallow it, but [try]/[with] and
    [Fun.protect ~finally] do {b not} run: only {!defer}s do. Prefer
    {!cancel}; use [kill] when shutdown must be guaranteed. *)

val protect : (unit -> 'a) -> 'a
(** Run [f] without being interrupted by {!cancel} or {!kill}, e.g. cleanup
    that needs to block. A pending cancellation is delivered at the next
    blocking point after [f] returns. *)

val defer : (unit -> unit) -> unit
(** Register cleanup for the current task, run when the task ends however it
    ends: returning, failing, cancelled or killed. Runs in reverse order of
    registration, inside {!protect}, in the task itself (so it may block).
    Note that defers are tied to the task, not to the enclosing function. *)

(** {1 Sockets}

    All file descriptors must be non-blocking, and each must only be waited
    on from one domain (except listening sockets, which several domains may
    accept on; stop all their acceptors before closing it). *)

exception Closed
(** Raised in tasks blocked on a file descriptor that is closed with
    {!close}. *)

val socket : unit -> Unix.file_descr
(** A non-blocking IPv4 TCP socket. *)

val connect : Unix.file_descr -> Unix.sockaddr -> unit
(** @raise Unix.Unix_error if the connection fails, e.g. [ECONNREFUSED]. *)

val accept : Unix.file_descr -> Unix.file_descr * Unix.sockaddr
(** The returned socket is non-blocking. Several tasks may accept on the same
    socket; each connection goes to one of them. *)

val read : Unix.file_descr -> bytes -> int -> int -> int
(** [read fd buf pos len] blocks until data is available; [0] means EOF.
    Don't mix with {!subscribe} on the same descriptor. *)

val write : Unix.file_descr -> bytes -> int -> int -> int
(** Write some of the buffer; returns the number of bytes written. *)

val write_all : Unix.file_descr -> bytes -> int -> int -> unit
(** Write the whole range. Concurrent writes on one descriptor may
    interleave. *)

val write_string : Unix.file_descr -> string -> unit

val read_bigarray :
  Unix.file_descr ->
  (_, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int -> int -> int
(** {!read} into a bigarray (e.g. a [Bigstringaf.t]). *)

val write_bigarray :
  Unix.file_descr ->
  (_, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int -> int -> int
(** {!write} from a bigarray; returns the number of bytes written. *)

val await_readable : Unix.file_descr -> unit
(** Block until the descriptor is readable, for building other I/O on top
    (e.g. [MSG_PEEK] or [recvfrom]). *)

val await_writable : Unix.file_descr -> unit

val close : Unix.file_descr -> unit
(** Close the descriptor, failing every task blocked on it with {!Closed}.
    Always use this instead of [Unix.close] for descriptors the scheduler may
    be watching. *)

(** {1 Channels} *)

(** Channels between tasks, on the same or different domains. *)
module Chan : sig
  type 'a t

  val create : ?capacity:int -> unit -> 'a t
  (** A FIFO channel. [send] blocks while [capacity] items are buffered
      (default: unbounded); [~capacity:0] makes every [send] wait for a
      [recv]. *)

  val send : 'a t -> 'a -> unit
  (** @raise Closed if the channel is closed. *)

  val recv : 'a t -> 'a
  (** Blocks until an item is available. Items buffered before [close] are
      still delivered.
      @raise Closed once the channel is closed and empty. *)

  val close : 'a t -> unit
  (** Wake every blocked sender and receiver with {!Closed}. *)
end

(** {1 Mutexes and conditions}

    For tasks, on the same or different domains: blocking suspends the task,
    not the domain. *)

module Mutex : sig
  type t

  val create : unit -> t

  val lock : t -> unit
  (** Block until the mutex is free, then take it. Waiters get it in FIFO
      order: [unlock] hands it straight to the next one. A cancellation
      point; a task cancelled while waiting doesn't get the mutex. *)

  val try_lock : t -> bool
  (** Take the mutex if it is free. Never blocks. *)

  val unlock : t -> unit
  (** Release the mutex, or hand it to the next waiter. Never blocks, so it is
      safe in cleanup. Any task may unlock; ownership isn't tracked.
      @raise Invalid_argument if not locked. *)

  val use : t -> (unit -> 'a) -> 'a
  (** [lock], run [f], [unlock] even if [f] raises. A {!kill} drops [f]
      without unwinding, leaving the mutex locked. *)
end

module Condition : sig
  type t

  val create : unit -> t

  val wait : t -> Mutex.t -> unit
  (** Release the mutex, block until signalled, take the mutex back. Always
      returns or raises with the mutex held again (retaking it is protected
      from cancellation). Wake-ups may be spurious: wait in a loop that
      checks the condition. A cancellation point. *)

  val signal : t -> unit
  (** Wake one waiting task, if any. Never blocks. *)

  val broadcast : t -> unit
  (** Wake every waiting task. Never blocks. *)
end

(** {1 Servers} *)

(** TCP servers accepting on several domains.

    {[
      let server = Goo.Server.start addr (fun fd _peer -> echo fd) in
      ...
      Goo.Server.shutdown ~timeout:5. server
    ]} *)
module Server : sig
  type t

  val start :
    ?domains:domain array ->
    ?acceptors:int ->
    ?backlog:int ->
    ?strategy:[ `Auto | `Reuseport | `Shared ] ->
    Unix.sockaddr ->
    (Unix.file_descr -> Unix.sockaddr -> unit) ->
    t
  (** Listen on the address and accept on every given domain (default: all),
      with [acceptors] accept loops per domain (default 1). Returns once every
      domain is listening.

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
end

(** {1 Fan-out}

    The scheduler reads a subscribed descriptor once and delivers every chunk
    to each subscriber's own queue, so slow subscribers don't miss data.
    Queues are unbounded. *)

type sub

val subscribe : Unix.file_descr -> sub
(** Receive every chunk read from the descriptor from now on. *)

val recv : sub -> string
(** The next chunk; [""] on EOF. A cancellation point even when data is
    queued.
    @raise Closed if the descriptor was closed with {!close}.
    @raise Invalid_argument after {!unsubscribe}. *)

val unsubscribe : sub -> unit
(** Stop receiving and drop queued chunks. *)
