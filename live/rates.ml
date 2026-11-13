(* Writes per second of a table, from samples of its write counter. *)

type t =
  { mutable samples: (float * int) list (* newest first *)
  ; keep: float
  }

let create ~keep = { samples = []; keep }

let add t ~now count =
  (match t.samples with
   | (_, last) :: _ when count < last -> t.samples <- [] (* counters reset *)
   | _ -> ());
  t.samples <- (now, count) :: List.filter (fun (ts, _) -> now -. ts <= t.keep) t.samples
;;

(* Since the newest sample at least [window] seconds old: None until
   there's one. *)
let rate t ~window =
  match t.samples with
  | [] -> None
  | (t1, c1) :: older ->
    (match List.find_opt (fun (ts, _) -> t1 -. ts >= window) older with
     | Some (t0, c0) -> Some (float_of_int (c1 - c0) /. (t1 -. t0))
     | None -> None)
;;

let clear t = t.samples <- []
