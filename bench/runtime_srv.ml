let app =
  let open Web in
  App.create () |> App.route Route.(get (nil @-> fun _ -> Response.text "hello"))

let () =
  let domains = int_of_string Sys.argv.(1) in
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3002) in
  Goo.run_parallel ~domains (fun i ->
      if i = 0 then Goo.Server.wait (Web.App.serve addr app))
