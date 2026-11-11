(** Facts about the platform this library was compiled for. *)

val system : string
(** The OCaml target system, e.g. ["linux"], ["macosx"], ["freebsd"],
    ["mingw64"] (from [ocamlc -config]). *)

val reuseport_balances : bool
(** Whether separate [SO_REUSEPORT] sockets bound to the same port get
    incoming connections load-balanced by the kernel (Linux 3.9+). On macOS
    and the BSDs the option exists but doesn't balance, so a single shared
    listening socket should be used instead. *)
