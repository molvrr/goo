open Effect.Deep

(* Raised in tasks blocked on an fd (read/write/accept/connect/recv) when that
   fd is closed with [close], and by [Chan] operations on a closed channel. *)
exception Closed

(* Raised in a task when its scope is cancelled, at the point where it is
   blocked or at its next blocking operation. *)
exception Cancelled

(* {1 Types}

   Each domain runs one scheduler ([sched]). Tasks never migrate: a task, its
   scope, and everything it waits on belong to one scheduler and are only
   touched from that scheduler's domain. Other domains talk to a scheduler by
   posting closures to its mailbox, which wakes it through a pipe. *)

type scope = {
  owner : sched;
  parent : scope option;
  tasks : (int, task) Hashtbl.t;
  mutable children : scope list;
  mutable pending : int;  (* live tasks + unfinished child scopes *)
  mutable finished : bool;
  mutable cancelled : bool;
  mutable killed : bool;  (* hard cancel: tasks are dropped, not unwound *)
  mutable error : exn option;  (* first failure; cancels the scope *)
  mutable joiners : waiter list;
  (* Called when the scope finishes; set for units of work (see below). *)
  mutable on_finish : (scope -> unit) option;
}

and task = {
  tid : int;
  scope : scope;
  mutable protected : int;
  (* Cleanup registered with [defer], most recent first. *)
  mutable defers : (unit -> unit) list;
  (* While blocked: asks the wait to deliver cancellation. *)
  mutable cancel_wait : (unit -> unit) option;
}

(* A blocked task. [live] guarantees its continuation is resumed only once,
   whichever of readiness, close or cancellation gets there first. *)
and waiter = { task : task; k : (unit, unit) continuation; mutable live : bool }

(* A subscription to a file descriptor's incoming bytes. Every subscriber of
   the same fd receives every chunk read after it subscribed, in order, via its
   own queue, so a slow subscriber never misses data. *)
and sub = {
  sowner : sched;
  fd : Unix.file_descr;
  inbox : string Queue.t;
  mutable waiter : (task * (string, unit) continuation) option;
  mutable state : [ `Open | `Eof | `Error of exn | `Unsubscribed ];
}

and sched = {
  index : int;
  cluster : cluster;
  (* Shared with other domains. *)
  mlock : Mutex.t;
  mailbox : (unit -> unit) Queue.t;
  signaled : bool Atomic.t;  (* a wake byte is pending in the pipe *)
  wake_r : Unix.file_descr;
  wake_w : Unix.file_descr;
  (* Owned by this scheduler's domain. *)
  runq : (task * (unit -> unit)) Queue.t;
  readers : (Unix.file_descr, waiter Queue.t) Hashtbl.t;
  writers : (Unix.file_descr, waiter Queue.t) Hashtbl.t;
  subs : (Unix.file_descr, sub list) Hashtbl.t;
  timers : (float * waiter) Dynarray.t;
  rbuf : bytes;
  mutable root : scope option;
  mutable next_id : int;
  mutable stop : bool;
}

(* All schedulers of one [run_parallel]. [units] counts running units of work
   (each [main i], [post] and [await_on] runs in its own scope); when it drops
   to zero no task can create more, so every scheduler is told to stop. *)
and cluster = {
  mutable scheds : sched array;
  units : int Atomic.t;
  failure : exn option Atomic.t;  (* first fatal failure *)
  closing : bool Atomic.t;
  done_ : bool Atomic.t;
}

type domain = sched

(* What a [Block] registration decided. [Wait unregister]: the task is now
   registered somewhere that will wake it; [unregister ()] is called on
   cancellation and returns whether the registration was withdrawn (if not,
   a wake-up is already on its way and the task keeps waiting for it). *)
type block = Proceed | Fail of exn | Wait of (unit -> bool)

type _ Effect.t += Close : Unix.file_descr -> unit Effect.t
type _ Effect.t += AwaitRead : Unix.file_descr -> unit Effect.t
type _ Effect.t += AwaitWrite : Unix.file_descr -> unit Effect.t
type _ Effect.t += Spawn : (unit -> unit) -> unit Effect.t
type _ Effect.t += Yield : unit Effect.t
type _ Effect.t += Sleep : float -> unit Effect.t
type _ Effect.t += Subscribe : Unix.file_descr -> sub Effect.t
type _ Effect.t += Recv : sub -> string Effect.t
type _ Effect.t += Self : task Effect.t
type _ Effect.t += New_scope : (unit -> unit) list -> scope Effect.t
type _ Effect.t += Join : scope -> unit Effect.t
type _ Effect.t += Check : unit Effect.t
type _ Effect.t += Block : (waiter -> block) -> unit Effect.t

(* {1 Current scheduler} *)

let current_key : sched option Domain.DLS.key = Domain.DLS.new_key (fun () -> None)
let current_opt () = Domain.DLS.get current_key

let current () =
  match current_opt () with
  | Some sc -> sc
  | None -> invalid_arg "Goo: not inside run"

let is_current sc = match current_opt () with Some s -> s == sc | None -> false
let log_exn what e = prerr_endline ("Goo: " ^ what ^ ": " ^ Printexc.to_string e)

(* {1 Mailbox} *)

(* Run [f] on [sc]'s domain, from any domain. *)
let post_internal sc f =
  if not (Atomic.get sc.cluster.done_) then begin
    Mutex.protect sc.mlock (fun () -> Queue.push f sc.mailbox);
    (* Only the first post since the last drain writes a wake byte. *)
    if not (Atomic.exchange sc.signaled true) then
      try ignore (Unix.single_write sc.wake_w (Bytes.make 1 '!') 0 1)
      with Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> ()
  end

let drain_mailbox sc =
  let b = Bytes.create 64 in
  (try
     while Unix.read sc.wake_r b 0 64 > 0 do
       ()
     done
   with Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) -> ());
  (* Reset before taking: a post racing with us either lands in this batch or
     sees [false] and writes a new wake byte. *)
  Atomic.set sc.signaled false;
  let batch =
    Mutex.protect sc.mlock (fun () ->
        let q = Queue.copy sc.mailbox in
        Queue.clear sc.mailbox;
        q)
  in
  Queue.iter (fun f -> try f () with e -> log_exn "mailbox handler failed" e) batch

(* {1 Waking tasks} (only on the owner's domain) *)

let schedule sc t f = Queue.push (t, f) sc.runq

let fire w f =
  if w.live then begin
    w.live <- false;
    w.task.cancel_wait <- None;
    schedule w.task.scope.owner w.task (fun () -> f w.k)
  end

let resume w = fire w (fun k -> continue k ())

(* Wake [w] from any domain. *)
let wake_waiter w =
  let sc = w.task.scope.owner in
  if is_current sc then resume w else post_internal sc (fun () -> resume w)

(* {1 Cancellation} (only on the owner's domain) *)

let rec signal s =
  Hashtbl.iter
    (fun _ t -> if t.protected = 0 then Option.iter (fun c -> c ()) t.cancel_wait)
    s.tasks;
  List.iter signal s.children

let cancel_local s =
  let rec mark s =
    if not s.cancelled then begin
      s.cancelled <- true;
      List.iter mark s.children
    end
  in
  if not s.cancelled then begin
    mark s;
    signal s
  end

let kill_local s =
  let rec mark s =
    s.cancelled <- true;
    s.killed <- true;
    List.iter mark s.children
  in
  mark s;
  (* Re-signal even if already cancelled: waits that forward cancellation
     elsewhere (e.g. [await_on]) must forward the kill too. *)
  signal s

(* {1 Scheduler internals} *)

let fresh sc =
  sc.next_id <- sc.next_id + 1;
  sc.next_id

let park tbl fd w =
  match Hashtbl.find_opt tbl fd with
  | Some q -> Queue.push w q
  | None ->
      let q = Queue.create () in
      Queue.push w q;
      Hashtbl.replace tbl fd q

let unpark tbl fd w =
  (match Hashtbl.find_opt tbl fd with
  | None -> ()
  | Some q ->
      let rest = Queue.of_seq (Seq.filter (fun w' -> w' != w) (Queue.to_seq q)) in
      if Queue.is_empty rest then Hashtbl.remove tbl fd else Hashtbl.replace tbl fd rest);
  true

let rec wake_one tbl fd =
  match Hashtbl.find_opt tbl fd with
  | None -> ()
  | Some q ->
      let w = Queue.pop q in
      if Queue.is_empty q then Hashtbl.remove tbl fd;
      if w.live then resume w else wake_one tbl fd

let remove_timer sc w =
  let keep = Dynarray.filter (fun (_, w') -> w' != w) sc.timers in
  Dynarray.clear sc.timers;
  Dynarray.append sc.timers keep;
  true

let new_scope sc parent =
  let s =
    {
      owner = sc;
      parent;
      tasks = Hashtbl.create 4;
      children = [];
      pending = 0;
      finished = false;
      cancelled = (match parent with Some p -> p.cancelled | None -> false);
      killed = (match parent with Some p -> p.killed | None -> false);
      error = None;
      joiners = [];
      on_finish = None;
    }
  in
  Option.iter
    (fun p ->
      p.pending <- p.pending + 1;
      p.children <- s :: p.children)
    parent;
  s

let rec finish_scope s =
  s.finished <- true;
  (match (s.on_finish, s.error, s.parent, s.joiners) with
  | Some f, _, _, _ -> f s
  | None, Some e, Some _, [] -> log_exn "unjoined scope failed" e
  | _ -> ());
  List.iter resume (List.rev s.joiners);
  s.joiners <- [];
  match s.parent with
  | Some p ->
      p.children <- List.filter (fun c -> c != s) p.children;
      release p
  | None -> ()

and release s =
  s.pending <- s.pending - 1;
  if s.pending = 0 then finish_scope s

let deliver sub event =
  let wake f =
    match sub.waiter with
    | Some (t, k) ->
        sub.waiter <- None;
        t.cancel_wait <- None;
        schedule sub.sowner t (fun () -> f k)
    | None -> ()
  in
  match event with
  | `Data data ->
      if Option.is_some sub.waiter then wake (fun k -> continue k data)
      else Queue.push data sub.inbox
  | `Eof ->
      sub.state <- `Eof;
      wake (fun k -> continue k "")
  | `Error e ->
      sub.state <- `Error e;
      wake (fun k -> discontinue k e)

(* Fail everything blocked on [fd] with [Closed], then close it. *)
let close_fd sc fd =
  let fail_all tbl =
    Option.iter
      (Queue.iter (fun w -> fire w (fun k -> discontinue k Closed)))
      (Hashtbl.find_opt tbl fd);
    Hashtbl.remove tbl fd
  in
  fail_all sc.readers;
  fail_all sc.writers;
  Option.iter
    (List.iter (fun s -> if s.state = `Open then deliver s (`Error Closed)))
    (Hashtbl.find_opt sc.subs fd);
  Hashtbl.remove sc.subs fd;
  Unix.close fd

let finish t failure =
  let s = t.scope in
  Hashtbl.remove s.tasks t.tid;
  Option.iter
    (fun e ->
      if Option.is_none s.error then s.error <- Some e;
      cancel_local s)
    failure;
  release s

let rec spawn_task s fn =
  let sc = s.owner in
  let t =
    { tid = fresh sc; scope = s; protected = 0; defers = []; cancel_wait = None }
  in
  Hashtbl.replace s.tasks t.tid t;
  s.pending <- s.pending + 1;
  schedule sc t (fun () -> exec t fn ~on_done:(cleanup t))

(* The task is done (or dropped): run its defers as a protected task body,
   then finish. Defers registered while cleaning up run too. *)
and cleanup t failure =
  match t.defers with
  | [] -> finish t failure
  | ds ->
      t.defers <- [];
      t.protected <- t.protected + 1;
      let run_defers () =
        let first = ref None in
        List.iter
          (fun d -> try d () with e -> if Option.is_none !first then first := Some e)
          ds;
        Option.iter raise !first
      in
      schedule t.scope.owner t (fun () ->
          exec t run_defers ~on_done:(fun f ->
              cleanup t (if Option.is_some failure then failure else f)))

(* Deliver cancellation to [t], blocked on [k]: unwind with [Cancelled], or
   if killed, drop [k] and go straight to the defers. *)
and abort : 'a. task -> ('a, unit) continuation -> unit =
 fun t k -> if t.scope.killed then cleanup t None else discontinue k Cancelled

(* Block [t] on [k]. On cancellation, [unregister w] withdraws the wait; if it
   returns [false] a wake-up is already coming and the task keeps waiting. *)
and suspend t k ~register ~unregister =
  let w = { task = t; k; live = true } in
  register w;
  t.cancel_wait <-
    Some (fun () -> if w.live && unregister w then fire w (fun k -> abort t k))

and exec t fn ~on_done =
  let sc = t.scope.owner in
  (* Every blocking effect goes through here: a cancelled task gets
     [Cancelled] (or is dropped, if killed) instead of blocking, unless
     inside [protect]. *)
  let cancel_point : 'a. ('a, unit) continuation -> (unit -> unit) -> unit =
   fun k proceed -> if t.scope.cancelled && t.protected = 0 then abort t k else proceed ()
  in
  let start () =
    if t.scope.cancelled && t.protected = 0 then raise Cancelled else fn ()
  in
  match start () with
  | () -> on_done None
  | exception Cancelled -> on_done None
  | exception e -> on_done (Some e)
  | effect Self, k -> continue k t
  | effect Check, k -> cancel_point k (fun () -> continue k ())
  | effect Spawn fn, k ->
      cancel_point k (fun () ->
          spawn_task t.scope fn;
          continue k ())
  | effect New_scope fns, k ->
      let s = new_scope sc (Some t.scope) in
      List.iter (spawn_task s) fns;
      if s.pending = 0 then finish_scope s;
      continue k s
  | effect Join s, k ->
      if s.finished then continue k ()
      else
        cancel_point k (fun () ->
            suspend t k
              ~register:(fun w -> s.joiners <- w :: s.joiners)
              ~unregister:(fun w ->
                s.joiners <- List.filter (fun w' -> w' != w) s.joiners;
                true))
  | effect Yield, k -> cancel_point k (fun () -> schedule sc t (fun () -> continue k ()))
  | effect Sleep secs, k ->
      cancel_point k (fun () ->
          suspend t k
            ~register:(fun w -> Dynarray.add_last sc.timers (Unix.gettimeofday () +. secs, w))
            ~unregister:(remove_timer sc))
  | effect AwaitRead fd, k ->
      cancel_point k (fun () ->
          suspend t k ~register:(park sc.readers fd) ~unregister:(unpark sc.readers fd))
  | effect AwaitWrite fd, k ->
      cancel_point k (fun () ->
          suspend t k ~register:(park sc.writers fd) ~unregister:(unpark sc.writers fd))
  | effect Block f, k ->
      cancel_point k (fun () ->
          let w = { task = t; k; live = true } in
          match f w with
          | Proceed ->
              w.live <- false;
              continue k ()
          | Fail e ->
              w.live <- false;
              discontinue k e
          | Wait unregister ->
              t.cancel_wait <-
                Some
                  (fun () -> if w.live && unregister () then fire w (fun k -> abort t k)))
  | effect Recv sub, k ->
      cancel_point k (fun () ->
          sub.waiter <- Some (t, k);
          t.cancel_wait <-
            Some
              (fun () ->
                match sub.waiter with
                | Some (t', k) when t' == t ->
                    sub.waiter <- None;
                    t.cancel_wait <- None;
                    schedule sc t (fun () -> abort t k)
                | _ -> ()))
  | effect Subscribe fd, k ->
      let sub = { sowner = sc; fd; inbox = Queue.create (); waiter = None; state = `Open } in
      let others = Option.value ~default:[] (Hashtbl.find_opt sc.subs fd) in
      Hashtbl.replace sc.subs fd (sub :: others);
      continue k sub
  | effect Close fd, k ->
      close_fd sc fd;
      continue k ()

(* {1 Units of work and the cluster} *)

let shutdown cl =
  if not (Atomic.exchange cl.closing true) then
    Array.iter (fun sc -> post_internal sc (fun () -> sc.stop <- true)) cl.scheds

(* First fatal failure: record it and cancel every scheduler's root. *)
let fail_cluster cl e =
  if Atomic.compare_and_set cl.failure None (Some e) then
    Array.iter (fun sc -> post_internal sc (fun () -> Option.iter cancel_local sc.root)) cl.scheds

(* Start [f] in a new unit scope on [sc] (its domain only). The unit must
   already be counted in [units]. *)
let start_unit sc ~on_done f =
  let s = new_scope sc sc.root in
  s.on_finish <-
    Some
      (fun s ->
        on_done s;
        if Atomic.fetch_and_add sc.cluster.units (-1) = 1 then shutdown sc.cluster);
  spawn_task s f;
  s

let post_unit d ~on_start ~on_done f =
  Atomic.incr d.cluster.units;
  post_internal d (fun () -> on_start (start_unit d ~on_done f))

(* {1 Event loop} *)

let tick sc =
  let now = Unix.gettimeofday () in
  let pending = Dynarray.create () in
  Dynarray.iter
    (fun ((deadline, w) as timer) ->
      if deadline <= now then resume w else Dynarray.add_last pending timer)
    sc.timers;
  Dynarray.clear sc.timers;
  Dynarray.append sc.timers pending

(* Drop unsubscribed entries and fds nobody listens to anymore. *)
let prune_subs sc =
  Hashtbl.filter_map_inplace
    (fun _ l ->
      match List.filter (fun s -> s.state = `Open) l with [] -> None | l -> Some l)
    sc.subs

(* Read once from [fd] and fan the result out to all its subscribers. *)
let fan_out sc fd =
  let event =
    match Unix.read fd sc.rbuf 0 (Bytes.length sc.rbuf) with
    | 0 -> Some `Eof
    | n -> Some (`Data (Bytes.sub_string sc.rbuf 0 n))
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) -> None
    | exception e -> Some (`Error e)
  in
  match (event, Hashtbl.find_opt sc.subs fd) with
  | Some event, Some l ->
      List.iter (fun s -> deliver s event) l;
      (match event with `Data _ -> () | `Eof | `Error _ -> Hashtbl.remove sc.subs fd)
  | None, _ | _, None -> ()

(* Seconds until the next timer fires; -1.0 means block indefinitely. *)
let next_timeout sc =
  if not (Queue.is_empty sc.runq) then 0.0
  else
    let earliest =
      Dynarray.fold_left (fun acc (d, _) -> Float.min acc d) infinity sc.timers
    in
    if earliest = infinity then -1.0 else Float.max 0.0 (earliest -. Unix.gettimeofday ())

(* Wait for I/O (without blocking when tasks are queued, see [next_timeout])
   and wake the tasks it unblocks. *)
let poll sc =
  prune_subs sc;
  (* Select on all subscribed fds, even without a blocked waiter, so data is
     buffered for every subscriber as it arrives. *)
  let fds =
    List.sort_uniq compare
      ((sc.wake_r :: List.of_seq (Hashtbl.to_seq_keys sc.readers))
      @ List.of_seq (Hashtbl.to_seq_keys sc.subs))
  in
  let wfds = List.of_seq (Hashtbl.to_seq_keys sc.writers) in
  let ready, writable =
    match Unix.select fds wfds [] (next_timeout sc) with
    | r, w, _ -> (r, w)
    | exception Unix.Unix_error (EINTR, _, _) -> ([], [])
  in
  List.iter (wake_one sc.writers) writable;
  List.iter
    (fun fd ->
      if fd = sc.wake_r then drain_mailbox sc
      else begin
        wake_one sc.readers fd;
        if Hashtbl.mem sc.subs fd then fan_out sc fd
      end)
    ready

(* Runs until told to stop: an idle scheduler may still receive work from
   other domains, so it blocks on its wake pipe instead of returning.

   Each round runs only the tasks queued when it starts, then polls I/O.
   Tasks scheduled meanwhile wait for the next round, so a busy set of tasks
   that keeps refilling the queue can't starve tasks parked on I/O or
   timers. *)
let rec loop sc =
  if Atomic.get sc.signaled then drain_mailbox sc;
  if not sc.stop then begin
    let rec run_batch n =
      if n > 0 && not sc.stop then
        match Queue.take_opt sc.runq with
        | Some (_, task) ->
            (try task () with e -> log_exn "task failed" e);
            run_batch (n - 1)
        | None -> ()
    in
    run_batch (Queue.length sc.runq);
    tick sc;
    if not sc.stop then begin
      poll sc;
      tick sc
    end;
    loop sc
  end

(* {1 Public API} *)

(* Direct read. Don't mix with [subscribe] on the same fd. *)
let read fd buf pos len =
  let rec go () =
    match Unix.read fd buf pos len with
    | n -> n
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
        Effect.perform (AwaitRead fd);
        go ()
  in
  go ()

(* Uses [single_write] because [Unix.write] loops internally and loses the
   partial count if EAGAIN hits midway on a non-blocking fd. *)
let write fd buf pos len =
  let rec go () =
    match Unix.single_write fd buf pos len with
    | n -> n
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
        Effect.perform (AwaitWrite fd);
        go ()
  in
  go ()

let write_all fd buf pos len =
  let rec go pos len =
    if len > 0 then
      let n = write fd buf pos len in
      go (pos + n) (len - n)
  in
  go pos len

let write_string fd s = write_all fd (Bytes.unsafe_of_string s) 0 (String.length s)

let await_readable fd = Effect.perform (AwaitRead fd)
let await_writable fd = Effect.perform (AwaitWrite fd)

let read_bigarray fd buf pos len =
  let rec go () =
    match Unix.read_bigarray fd buf pos len with
    | n -> n
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
        await_readable fd;
        go ()
  in
  go ()

let write_bigarray fd buf pos len =
  let rec go () =
    match Unix.single_write_bigarray fd buf pos len with
    | n -> n
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
        await_writable fd;
        go ()
  in
  go ()

(* Non-blocking connect: EINPROGRESS means "wait until writable", after which
   SO_ERROR tells whether it succeeded. *)
let connect fd addr =
  match Unix.connect fd addr with
  | () -> ()
  | exception Unix.Unix_error ((EINPROGRESS | EINTR | EWOULDBLOCK), _, _) -> (
      Effect.perform (AwaitWrite fd);
      match Unix.getsockopt_error fd with
      | None -> ()
      | Some err -> raise (Unix.Unix_error (err, "connect", "")))

let accept fd =
  let rec go () =
    match Unix.accept ~cloexec:true fd with
    | (client, _) as res ->
        Unix.set_nonblock client;
        res
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR | ECONNABORTED), _, _) ->
        Effect.perform (AwaitRead fd);
        go ()
  in
  go ()

let close fd = Effect.perform @@ Close fd
let spawn fn = Effect.perform @@ Spawn fn
let yield () = Effect.perform Yield
let check () = Effect.perform Check
let sleep secs = Effect.perform @@ Sleep secs
let subscribe fd = Effect.perform @@ Subscribe fd

let check_owner what sc = if not (is_current sc) then invalid_arg ("Goo." ^ what ^ ": owned by another domain")

let recv sub =
  check_owner "recv" sub.sowner;
  (* Cancellation point even when data is queued. *)
  Effect.perform Check;
  match Queue.take_opt sub.inbox with
  | Some data -> data
  | None -> (
      match sub.state with
      | `Open -> Effect.perform (Recv sub)
      | `Eof -> ""
      | `Error e -> raise e
      | `Unsubscribed -> invalid_arg "Goo.recv: unsubscribed")

let unsubscribe sub =
  check_owner "unsubscribe" sub.sowner;
  sub.state <- `Unsubscribed;
  Queue.clear sub.inbox

let socket () =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.set_nonblock s;
  s

let self () = current ()
let domains () = Array.copy (current ()).cluster.scheds
let index d = d.index

let post d f =
  if Atomic.get d.cluster.closing then invalid_arg "Goo.post: runtime is shutting down";
  post_unit d ~on_start:ignore
    ~on_done:(fun s -> Option.iter (log_exn "posted task failed") s.error)
    f

let await_on d f =
  let here = current () in
  if d == here then f ()
  else begin
    if Atomic.get d.cluster.closing then
      invalid_arg "Goo.await_on: runtime is shutting down";
    let result = ref None in
    (* Only touched on [d]'s domain. *)
    let remote = ref None and request = ref `None in
    Effect.perform
      (Block
         (fun w ->
           post_unit d
             ~on_start:(fun s ->
               remote := Some s;
               match !request with
               | `Cancel -> cancel_local s
               | `Kill -> kill_local s
               | `None -> ())
             ~on_done:(fun s ->
               Option.iter (log_exn "await_on cleanup failed") s.error;
               wake_waiter w)
             (fun () -> result := Some (match f () with v -> Ok v | exception e -> Error e));
           (* Cancelling the caller cancels the remote work; the caller keeps
              waiting for it to wind down, so it never outlives the call. *)
           Wait
             (fun () ->
               let killed = w.task.scope.killed in
               post_internal d (fun () ->
                   request := if killed then `Kill else `Cancel;
                   Option.iter (if killed then kill_local else cancel_local) !remote);
               false)));
    match !result with Some (Ok v) -> v | Some (Error e) -> raise e | None -> raise Cancelled
  end

let scope fns = Effect.perform @@ New_scope fns

let on_owner s f = if is_current s.owner then f () else post_internal s.owner f
let cancel s = on_owner s (fun () -> cancel_local s)
let kill s = on_owner s (fun () -> kill_local s)

let spawn_in s fn =
  if is_current s.owner then begin
    if s.finished then invalid_arg "Goo.spawn_in: scope has finished";
    spawn_task s fn
  end
  else
    post_internal s.owner (fun () ->
        if s.finished then
          log_exn "spawn_in: task dropped" (Invalid_argument "scope finished before it started")
        else spawn_task s fn)

let defer f =
  let t = Effect.perform Self in
  t.defers <- f :: t.defers

let rec is_within s target =
  s == target || match s.parent with Some p -> is_within p target | None -> false

let join_local s =
  let self = Effect.perform Self in
  if is_within self.scope s then invalid_arg "Goo.join: joining own scope";
  Effect.perform (Join s);
  Option.iter raise s.error

let join s = if is_current s.owner then join_local s else await_on s.owner (fun () -> join_local s)

let protect f =
  let t = Effect.perform Self in
  t.protected <- t.protected + 1;
  Fun.protect ~finally:(fun () -> t.protected <- t.protected - 1) f

let scoped fns =
  let s = scope fns in
  match join s with
  | () -> ()
  | exception Cancelled ->
      cancel s;
      protect (fun () -> Effect.perform (Join s));
      raise Cancelled

(* {1 Channels} *)

module Chan = struct
  type 'a receiver = { rw : waiter; mutable value : 'a option }
  type 'a sender = { sw : waiter; v : 'a; mutable accepted : bool }

  (* All fields are protected by [lock]. Blocked tasks are woken on their
     own domain via [wake_waiter]; a waiter still in a queue is unclaimed,
     so cancellation withdraws it by removing it under the lock. *)
  type 'a t = {
    lock : Mutex.t;
    items : 'a Queue.t;
    capacity : int;
    receivers : 'a receiver Queue.t;
    senders : 'a sender Queue.t;
    mutable closed : bool;
  }

  let create ?(capacity = max_int) () =
    if capacity < 0 then invalid_arg "Goo.Chan.create: negative capacity";
    {
      lock = Mutex.create ();
      items = Queue.create ();
      capacity;
      receivers = Queue.create ();
      senders = Queue.create ();
      closed = false;
    }

  let remove q x =
    let before = Queue.length q in
    let rest = Queue.of_seq (Seq.filter (fun y -> y != x) (Queue.to_seq q)) in
    Queue.clear q;
    Queue.transfer rest q;
    Queue.length q < before

  let send c v =
    let me = ref None in
    Effect.perform
      (Block
         (fun w ->
           Mutex.protect c.lock (fun () ->
               if c.closed then Fail Closed
               else
                 match Queue.take_opt c.receivers with
                 | Some r ->
                     r.value <- Some v;
                     wake_waiter r.rw;
                     Proceed
                 | None when Queue.length c.items < c.capacity ->
                     Queue.push v c.items;
                     Proceed
                 | None ->
                     let s = { sw = w; v; accepted = false } in
                     me := Some s;
                     Queue.push s c.senders;
                     Wait (fun () -> Mutex.protect c.lock (fun () -> remove c.senders s)))));
    match !me with Some { accepted = false; _ } -> raise Closed | _ -> ()

  let recv c =
    let got = ref None and me = ref None in
    Effect.perform
      (Block
         (fun w ->
           Mutex.protect c.lock (fun () ->
               let take_sender () =
                 match Queue.take_opt c.senders with
                 | Some s ->
                     s.accepted <- true;
                     wake_waiter s.sw;
                     Some s.v
                 | None -> None
               in
               match Queue.take_opt c.items with
               | Some v ->
                   (* Room freed: admit one blocked sender. *)
                   Option.iter (fun v -> Queue.push v c.items) (take_sender ());
                   got := Some v;
                   Proceed
               | None -> (
                   match take_sender () with
                   | Some v ->
                       got := Some v;
                       Proceed
                   | None when c.closed -> Fail Closed
                   | None ->
                       let r = { rw = w; value = None } in
                       me := Some r;
                       Queue.push r c.receivers;
                       Wait (fun () -> Mutex.protect c.lock (fun () -> remove c.receivers r))))));
    match (!got, !me) with
    | Some v, _ | None, Some { value = Some v; _ } -> v
    | _ -> raise Closed

  let close c =
    Mutex.protect c.lock (fun () ->
        if not c.closed then begin
          c.closed <- true;
          Queue.iter (fun r -> wake_waiter r.rw) c.receivers;
          Queue.clear c.receivers;
          Queue.iter (fun s -> wake_waiter s.sw) c.senders;
          Queue.clear c.senders
        end)
end

(* {1 Mutexes and conditions}

   Both keep their blocked tasks in a queue under a short [Stdlib.Mutex], like
   [Chan]. A waiter still in a queue is unclaimed, so cancellation withdraws it
   by removing it; one taken out of the queue is always woken. *)

let remove_waiter q w =
  let before = Queue.length q in
  let rest = Queue.of_seq (Seq.filter (fun w' -> w' != w) (Queue.to_seq q)) in
  Queue.clear q;
  Queue.transfer rest q;
  Queue.length q < before

module Mutex = struct
  (* Invariant: [waiters] non-empty -> [locked]. [unlock] hands the lock
     straight to the first waiter, which wakes up holding it. *)
  type t = { lock : Stdlib.Mutex.t; mutable locked : bool; waiters : waiter Queue.t }

  let create () = { lock = Stdlib.Mutex.create (); locked = false; waiters = Queue.create () }

  let lock m =
    Effect.perform
      (Block
         (fun w ->
           Stdlib.Mutex.protect m.lock (fun () ->
               if not m.locked then begin
                 m.locked <- true;
                 Proceed
               end
               else begin
                 Queue.push w m.waiters;
                 Wait (fun () -> Stdlib.Mutex.protect m.lock (fun () -> remove_waiter m.waiters w))
               end)))

  let try_lock m =
    Stdlib.Mutex.protect m.lock (fun () ->
        if m.locked then false
        else begin
          m.locked <- true;
          true
        end)

  let unlock m =
    let next =
      Stdlib.Mutex.protect m.lock (fun () ->
          if not m.locked then invalid_arg "Goo.Mutex.unlock: not locked";
          match Queue.take_opt m.waiters with
          | Some w -> Some w
          | None ->
              m.locked <- false;
              None)
    in
    Option.iter wake_waiter next

  let use m f =
    lock m;
    Fun.protect ~finally:(fun () -> unlock m) f
end

module Condition = struct
  type t = { lock : Stdlib.Mutex.t; waiters : waiter Queue.t }

  let create () = { lock = Stdlib.Mutex.create (); waiters = Queue.create () }

  let wait c m =
    (* Register before releasing [m], so a [signal] sent by whoever takes [m]
       next can't be missed. If the task is already cancelled, [Block] raises
       before the callback runs and [m] is still held. *)
    let released = ref false in
    match
      Effect.perform
        (Block
           (fun w ->
             Stdlib.Mutex.protect c.lock (fun () -> Queue.push w c.waiters);
             Mutex.unlock m;
             released := true;
             Wait (fun () -> Stdlib.Mutex.protect c.lock (fun () -> remove_waiter c.waiters w))))
    with
    | () -> protect (fun () -> Mutex.lock m)
    | exception e ->
        if !released then protect (fun () -> Mutex.lock m);
        raise e

  let signal c =
    Option.iter wake_waiter (Stdlib.Mutex.protect c.lock (fun () -> Queue.take_opt c.waiters))

  let broadcast c =
    let ws =
      Stdlib.Mutex.protect c.lock (fun () ->
          let ws = List.of_seq (Queue.to_seq c.waiters) in
          Queue.clear c.waiters;
          ws)
    in
    List.iter wake_waiter ws
end

(* {1 Servers} *)


(* {1 Running} *)

let run_parallel ?domains main =
  if Option.is_some (current_opt ()) then invalid_arg "Goo.run: already running";
  let n =
    match domains with
    | None -> Domain.recommended_domain_count ()
    | Some n when n >= 1 -> n
    | Some _ -> invalid_arg "Goo.run_parallel: domains must be >= 1"
  in
  let cl =
    {
      scheds = [||];
      units = Atomic.make n;  (* one per [main i] *)
      failure = Atomic.make None;
      closing = Atomic.make false;
      done_ = Atomic.make false;
    }
  in
  let make index =
    let wake_r, wake_w = Unix.pipe ~cloexec:true () in
    Unix.set_nonblock wake_r;
    Unix.set_nonblock wake_w;
    {
      index;
      cluster = cl;
      mlock = Stdlib.Mutex.create ();
      mailbox = Queue.create ();
      signaled = Atomic.make false;
      wake_r;
      wake_w;
      runq = Queue.create ();
      readers = Hashtbl.create 5;
      writers = Hashtbl.create 5;
      subs = Hashtbl.create 5;
      timers = Dynarray.create ();
      rbuf = Bytes.create 65536;
      root = None;
      next_id = 0;
      stop = false;
    }
  in
  cl.scheds <- Array.init n make;
  Array.iter
    (fun sc ->
      let root = new_scope sc None in
      root.pending <- 1;  (* the root never finishes; units come and go *)
      sc.root <- Some root)
    cl.scheds;
  (* Writing to a closed pipe/socket must surface as EPIPE, not kill us. *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  let body sc () =
    Domain.DLS.set current_key (Some sc);
    Fun.protect
      ~finally:(fun () -> Domain.DLS.set current_key None)
      (fun () ->
        ignore
          (start_unit sc
             ~on_done:(fun s -> Option.iter (fail_cluster cl) s.error)
             (fun () -> main sc.index));
        try loop sc
        with e ->
          (* A scheduler bug: its units can never finish, so stop everyone. *)
          fail_cluster cl e;
          Atomic.set cl.closing true;
          Array.iter (fun sc -> post_internal sc (fun () -> sc.stop <- true)) cl.scheds)
  in
  let others = Array.map (fun sc -> Domain.spawn (body sc)) (Array.sub cl.scheds 1 (n - 1)) in
  body cl.scheds.(0) ();
  Array.iter Domain.join others;
  Atomic.set cl.done_ true;
  Array.iter
    (fun sc ->
      Unix.close sc.wake_r;
      Unix.close sc.wake_w)
    cl.scheds;
  Option.iter raise (Atomic.get cl.failure)

let run fn = run_parallel ~domains:1 (fun _ -> fn ())

let run_main ?domains main =
  let result = ref None in
  run_parallel ?domains (fun i -> if i = 0 then result := Some (main ()));
  (* [main] ran on this domain and returned, or [run_parallel] raised. *)
  match !result with Some v -> v | None -> assert false
