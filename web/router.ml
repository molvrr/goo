type node = {
  lits : (string, node) Hashtbl.t;
  mutable caps : (int * (string -> bool) * node) list;  (* insertion order *)
  mutable ends : (Route.route * bool) list;  (* routes ending here, with trailing-slash flag *)
}

type t = node

type result =
  | Found of (unit -> Response.t)
  | Bad_query of string
  | Upgrade_required
  | Not_found of string list

let node () = { lits = Hashtbl.create 4; caps = []; ends = [] }

let insert root route =
  let keys, trailing = Route.keys route in
  let rec go n = function
    | [] -> n.ends <- n.ends @ [ (route, trailing) ]
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
  let rec walk n = function
    | [] -> try_ends n.ends
    | seg :: rest as segs ->
        Option.iter (fun c -> walk c rest) (Hashtbl.find_opt n.lits seg);
        List.iter (fun (_, accepts, c) -> if accepts seg then walk c rest) n.caps;
        if segs = [ "" ] then try_ends (List.filter snd n.ends)
  in
  match walk root (Request.segments req) with
  | () when !needs_upgrade -> Upgrade_required
  | () -> Not_found (List.sort_uniq String.compare !allowed)
  | exception Stop r -> r
