type node = {
  lits : (string, node) Hashtbl.t;
  mutable caps : (int * (string -> bool) * node) list;  (* insertion order *)
  mutable ends : (unit Route.route * bool) list;  (* routes ending here, with trailing-slash flag *)
  mutable rests : unit Route.route list;  (* [Route.rest] routes taking whatever follows *)
}

type t = node

type result =
  | Found of (unit -> Response.t)
  | Bad_query of string
  | Upgrade_required
  | Not_found of string list

let node () = { lits = Hashtbl.create 4; caps = []; ends = []; rests = [] }

let insert root route =
  let keys, ending = Route.keys route in
  let rec go n = function
    | [] -> (
        match ending with
        | Route.End_exact trailing -> n.ends <- n.ends @ [ (route, trailing) ]
        | Route.End_rest -> n.rests <- n.rests @ [ route ])
    | Route.Key_lit l :: rest ->
        go
          (match Hashtbl.find_opt n.lits l with
          | Some c -> c
          | None ->
              let c = node () in
              Hashtbl.replace n.lits l c;
              c)
          rest
    | Route.Key_cap { id; accepts } :: rest ->
        go
          (match List.find_opt (fun (id', _, _) -> id' = id) n.caps with
          | Some (_, _, c) -> c
          | None ->
              let c = node () in
              n.caps <- n.caps @ [ (id, accepts, c) ];
              c)
          rest
  in
  go root keys

let create routes =
  let root = node () in
  List.iter (insert root) routes;
  root

exception Stop of result

let lookup root req =
  let allowed = ref [] and needs_upgrade = ref false in
  (* Candidates whose structure matched: the typed matcher decodes captures
     and query parameters and checks the method. *)
  (* An upgrade request prefers WebSocket routes ending at the same node,
     so a page and its socket can share a URL whatever the order. *)
  let upgrading = Route.wants_upgrade req in
  let try_ends ends =
    let ends =
      if upgrading then
        let ws, others = List.partition (fun (r, _) -> Route.is_ws r) ends in
        ws @ others
      else ends
    in
    List.iter
      (fun (route, _) ->
        match Route.matches route req with
        | Matched run -> raise (Stop (Found run))
        | Bad_query e -> raise (Stop (Bad_query e))
        | Wrong_method m -> allowed := Httpun.Method.to_string m :: !allowed
        | Needs_upgrade -> needs_upgrade := true
        | No_match -> ())
      ends
  in
  (* [rest] routes on the way, with their depth: tried after every exact
     route, deepest (most specific prefix) first. *)
  let rests = ref [] in
  let rec walk depth n segs =
    if n.rests <> [] then rests := (depth, n.rests) :: !rests;
    match segs with
    | [] -> try_ends n.ends
    | seg :: rest as segs ->
        Option.iter (fun c -> walk (depth + 1) c rest) (Hashtbl.find_opt n.lits seg);
        List.iter (fun (_, accepts, c) -> if accepts seg then walk (depth + 1) c rest) n.caps;
        if segs = [ "" ] then try_ends (List.filter snd n.ends)
  in
  let walk_all root segs =
    walk 0 root segs;
    List.stable_sort (fun (a, _) (b, _) -> compare b a) (List.rev !rests)
    |> List.iter (fun (_, routes) -> try_ends (List.map (fun r -> (r, false)) routes))
  in
  match walk_all root (Request.segments req) with
  | () when !needs_upgrade -> Upgrade_required
  | () -> Not_found (List.sort_uniq String.compare !allowed)
  | exception Stop r -> r
