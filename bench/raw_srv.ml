(* Minimal keep-alive HTTP responder straight on Goo.Server: no httpun. *)
let resp = "HTTP/1.1 200 OK\r\ncontent-length: 5\r\n\r\nhello"

let handler fd _peer =
  let buf = Bytes.create 4096 in
  let rec loop () =
    let n = Goo.read fd buf 0 4096 in
    if n > 0 then begin
      (* wrk sends one small request at a time; count request ends. *)
      let s = Bytes.sub_string buf 0 n in
      let rec count i acc =
        match String.index_from_opt s i '\r' with
        | Some j when j + 3 < String.length s && String.sub s j 4 = "\r\n\r\n" -> count (j + 4) (acc + 1)
        | Some j -> count (j + 1) acc
        | None -> acc
      in
      for _ = 1 to count 0 0 do Goo.write_string fd resp done;
      loop ()
    end
  in
  loop ()

let () =
  let domains = int_of_string Sys.argv.(1) in
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3003) in
  Goo.run_parallel ~domains (fun i ->
      if i = 0 then Goo.Server.wait (Goo.Server.start addr handler))
