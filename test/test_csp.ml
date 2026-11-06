let check name expected actual =
  if expected <> actual then begin
    Printf.printf "FAIL %s\n  expected: %s\n  actual:   %s\n" name expected actual;
    exit 1
  end

let () =
  check "csp" "default-src 'none'; style-src 'unsafe-inline'; img-src 'self' data: https://cdn.x 'nonce-abc'; sandbox allow-popups; upgrade-insecure-requests"
    Csp.(to_string
      [ default_src [`none]; style_src [`unsafe_inline]
      ; img_src [`self; `data; `host "https://cdn.x"; `nonce "abc"]
      ; sandbox [`allow_popups]; upgrade_insecure_requests ]);
  check "csp empty list" "img-src 'none'; sandbox" Csp.(to_string [img_src []; sandbox []]);
  check "csp rejects injection" "rejected"
    (try ignore (Csp.(to_string [img_src [`host "x; script-src *"]])); "accepted"
     with Invalid_argument _ -> "rejected");
  print_endline "ok"
