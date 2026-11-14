(* The kernel's readiness polling for a scheduler (see goo_poller_stubs.c):
   epoll on Linux, kqueue on the BSDs and macOS, none elsewhere (Goo then
   uses select). Masks: 1 = read, 2 = write. *)

type t = Unix.file_descr

external kind_ : unit -> int = "goo_poller_kind"
external create : unit -> t = "goo_poller_create"
external modify : t -> Unix.file_descr -> int -> int -> unit = "goo_poller_modify"
external wait_ : t -> int -> int array -> int = "goo_poller_wait"

let kind = match kind_ () with 1 -> `Epoll | 2 -> `Kqueue | _ -> `Select
let max_events = 256
let buffer () = Array.make (2 * max_events) 0

(* Up to [timeout] seconds (negative: forever); [f fd mask] for each ready fd. *)
let wait t ~timeout buf f =
  let ms = if timeout < 0. then -1 else int_of_float (Float.ceil (timeout *. 1000.)) in
  let n = wait_ t ms buf in
  for i = 0 to n - 1 do
    f (Obj.magic buf.(2 * i) : Unix.file_descr) buf.((2 * i) + 1)
  done

let close t = Unix.close t
