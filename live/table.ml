(* A table live pages may watch, as declared by the app. *)

type mode =
  | Notify (* triggers announce each write: instant, by key *)
  | Poll (* no triggers: write counters read every few seconds, whole table *)

type policy =
  [ `Auto (* Notify, Poll while it's written too often *)
  | `Notify
  | `Poll
  ]

type t =
  { name: string (* schema.table *)
  ; keys: string list
  ; policy: policy
  }

let qualify name = if String.contains name '.' then name else "public." ^ name
let make ?(keys = []) ?(mode = `Auto) name = { name = qualify name; keys = List.sort_uniq String.compare keys; policy = mode }
let initial_mode t = match t.policy with `Poll -> Poll | `Auto | `Notify -> Notify
