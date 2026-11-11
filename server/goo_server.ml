(* TCP servers on Goo's public API. Each domain runs a node: a posted task
   owning the domain's accept loops and its connections. *)

let log_exn what e = prerr_endline ("Goo: " ^ what ^ ": " ^ Printexc.to_string e)

(* One per domain, all owned by that domain: [unit_scope] runs the node,
   [accept] holds the accept loops, and [conns] holds connection tasks
   (kept open by a task waiting on [conns_open] until the acceptors are
   done, so it can't finish early while empty). *)
type node = { accept : Goo.scope; conns : Goo.scope; unit_scope : Goo.scope }

type t = {
  addr : Unix.sockaddr;
  nodes : node array;
  shared : Unix.file_descr option;  (* closed by [shutdown] once no one accepts on it *)
  stopping : bool Atomic.t;
}

let addr t = t.addr

let listener ~reuseport ~backlog addr =
  let fd = Unix.socket ~cloexec:true (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  match
    Unix.setsockopt fd Unix.SO_REUSEADDR true;
    if reuseport then Unix.setsockopt fd Unix.SO_REUSEPORT true;
    Unix.bind fd addr;
    Unix.listen fd backlog;
    Unix.set_nonblock fd
  with
  | () -> fd
  | exception e ->
      Unix.close fd;
      raise e

(* Out of descriptors or memory: back off instead of failing the server. *)
let rec accept_retry lfd =
  match Goo.accept lfd with
  | r -> r
  | exception Unix.Unix_error ((EMFILE | ENFILE | ENOBUFS | ENOMEM), _, _) ->
      Goo.sleep 0.01;
      accept_retry lfd

(* A handler failure is logged rather than failing [conns], which would
   cancel every other connection on the domain. *)
let connection handler fd peer () =
  Goo.defer (fun () -> Goo.close fd);
  try handler fd peer with
  | Goo.Cancelled -> raise Goo.Cancelled
  | e -> log_exn "connection handler failed" e

let node_body ~source ~acceptors ~backlog ~addr ~handler ~ready ~unit_scope () =
  match
    match source with
    | `Use (fd, own) -> (fd, own)
    | `Bind -> (listener ~reuseport:true ~backlog addr, true)
  with
  | exception e -> Goo.Chan.send ready (Error e)
  | lfd, own ->
      let conns_open = Goo.Chan.create () in
      let conns = Goo.scope [ (fun () -> try Goo.Chan.recv conns_open with Goo.Closed -> ()) ] in
      (* Runs however the node ends, including cluster-wide cancellation. *)
      Goo.defer (fun () ->
          if own then Goo.close lfd;
          Goo.Chan.close conns_open;
          try Goo.join conns with e -> log_exn "server connections failed" e);
      let rec loop () =
        let fd, peer = accept_retry lfd in
        (* [conns] is only cancelled while we accept if this loop is too
           (shutdown cancels it after the acceptors are done): then a new
           connection would never start, so close it here instead. *)
        (match Goo.check () with
         | () -> Goo.spawn_in conns (connection handler fd peer)
         | exception Goo.Cancelled ->
             Unix.close fd;
             raise Goo.Cancelled);
        loop ()
      in
      let accept = Goo.scope (List.init acceptors (fun _ -> loop)) in
      Goo.Chan.send ready (Ok { accept; conns; unit_scope });
      Goo.join accept

(* Run [node_body] on [d], in a scope it knows (to report as [unit_scope]). *)
let post_node d body =
  Goo.post d (fun () ->
      let me = Goo.Chan.create () in
      let unit_scope = Goo.scope [ (fun () -> body ~unit_scope:(Goo.Chan.recv me) ()) ] in
      Goo.Chan.send me unit_scope;
      match Goo.join unit_scope with
      | () -> ()
      | exception Goo.Cancelled -> raise Goo.Cancelled
      | exception e -> log_exn "server node failed" e)

let shutdown ?timeout t =
  if Atomic.exchange t.stopping true then Array.iter (fun n -> Goo.join n.unit_scope) t.nodes
  else begin
    let quietly f = try f () with Goo.Cancelled -> raise Goo.Cancelled | e -> log_exn "server shutdown" e in
    (* 1. Stop accepting everywhere; only then is a shared socket unused. *)
    Array.iter (fun n -> Goo.cancel n.accept) t.nodes;
    Array.iter (fun n -> quietly (fun () -> Goo.join n.accept)) t.nodes;
    Option.iter Unix.close t.shared;
    (* 2. Let connections finish, cancelling them after [timeout]. *)
    let timer =
      Option.map
        (fun secs ->
          Goo.scope
            [
              (fun () ->
                Goo.sleep secs;
                Array.iter (fun n -> Goo.cancel n.conns) t.nodes);
            ])
        timeout
    in
    Array.iter (fun n -> quietly (fun () -> Goo.join n.unit_scope)) t.nodes;
    Option.iter
      (fun s ->
        Goo.cancel s;
        quietly (fun () -> Goo.join s))
      timer
  end

let wait t = Array.iter (fun n -> Goo.join n.unit_scope) t.nodes

let start ?domains:doms ?(acceptors = 1) ?(backlog = 128) ?(strategy = `Auto) addr handler =
  let doms = match doms with Some d -> d | None -> Goo.domains () in
  if Array.length doms = 0 then invalid_arg "Goo_server.start: no domains";
  if acceptors < 1 then invalid_arg "Goo_server.start: acceptors must be >= 1";
  let reuseport =
    match strategy with
    | `Reuseport -> true
    | `Shared -> false
    | `Auto -> Platform.reuseport_balances
  in
  (* Bind the first socket here so port 0 resolves once; with SO_REUSEPORT
     the other domains then bind the same resolved address. *)
  let first = listener ~reuseport ~backlog addr in
  let addr = Unix.getsockname first in
  let ready = Goo.Chan.create () in
  Array.iteri
    (fun i d ->
      let source =
        if i = 0 then `Use (first, reuseport) else if reuseport then `Bind else `Use (first, false)
      in
      match post_node d (node_body ~source ~acceptors ~backlog ~addr ~handler ~ready) with
      | () -> ()
      | exception Invalid_argument _ -> invalid_arg "Goo_server.start: runtime is shutting down")
    doms;
  let results = Array.init (Array.length doms) (fun _ -> Goo.Chan.recv ready) in
  let nodes = Array.to_list results |> List.filter_map Result.to_option |> Array.of_list in
  let t =
    { addr; nodes; shared = (if reuseport then None else Some first); stopping = Atomic.make false }
  in
  match Array.find_map (function Error e -> Some e | Ok _ -> None) results with
  | None -> t
  | Some e ->
      shutdown ~timeout:0. t;
      raise e
