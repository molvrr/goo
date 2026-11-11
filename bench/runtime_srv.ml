let app =
  let open Web in
  App.create () |> App.route Route.(get (nil @-> fun _ -> Response.text "hello"))

let () =
  let domains = int_of_string Sys.argv.(1) in
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 3002) in
  Goo.run_main ~domains (fun () -> Goo_server.wait (Web.App.serve addr app))
