module D = Datastar
module DW = Datastar_web
module Req = Web.Request

let check name expected actual =
  if expected <> actual then begin
    Printf.printf "FAIL %s\n  expected: %s\n  actual:   %s\n" name expected actual;
    exit 1
  end

let signals r =
  match DW.read_signals r with Ok j -> Yojson.Safe.to_string j | Error e -> "error: " ^ e

let () =
  check "datastar request" "true"
    (string_of_bool (DW.is_datastar_request (Req.make ~headers:[ ("datastar-request", "true") ] "/")));
  check "plain request" "false" (string_of_bool (DW.is_datastar_request (Req.make "/")));
  check "signals from query" {|{"count":1}|} (signals (Req.make "/x?datastar=%7B%22count%22%3A1%7D"));
  check "signals from body" {|{"q":"hi"}|} (signals (Req.make ~meth:`POST ~body:{|{"q":"hi"}|} "/x"));
  check "missing query" "error: missing datastar query parameter" (signals (Req.make "/x"));
  check "bad json" "error" (String.sub (signals (Req.make ~meth:`POST ~body:"{" "/x")) 0 5);
  (* The response streams in a Goo task. *)
  let body = ref "" in
  Goo.run (fun () ->
      body :=
        Web.Response.body_to_string
          (DW.sse (fun send ->
               send (D.Sse.patch_elements ~id:"1" ~retry:500 ~selector:"#x" [ Html.li [] [ Html.text "a" ] ]);
               send (D.Sse.patch_signals (`Assoc [ ("n", `Int 2) ])))));
  check "sse events"
    "event: datastar-patch-elements\nid: 1\nretry: 500\ndata: selector #x\ndata: elements <li>a</li>\n\n\
     event: datastar-patch-signals\ndata: signals {\"n\":2}\n\n"
    !body;
  print_endline "ok"
