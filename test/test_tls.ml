(* Web.Client over https://, against a local [openssl s_server] with a
   throwaway self-signed certificate. Skipped without openssl on the PATH. *)

module C = Web.Client

let failures = ref 0

let check name ok =
  Printf.printf "%s %s\n%!" (if ok then "ok  " else "FAIL") name;
  if not ok then incr failures

let starts_with ~prefix s = String.length s >= String.length prefix && String.sub s 0 (String.length prefix) = prefix

let read_file f = In_channel.with_open_bin f In_channel.input_all

let wait_for_port port =
  let rec go n =
    let fd = Unix.socket PF_INET SOCK_STREAM 0 in
    match Unix.connect fd (ADDR_INET (Unix.inet_addr_loopback, port)) with
    | () -> Unix.close fd
    | exception Unix.Unix_error _ when n > 0 ->
        Unix.close fd;
        Unix.sleepf 0.05;
        go (n - 1)
  in
  go 100

let () =
  if Sys.command "command -v openssl >/dev/null" <> 0 then print_endline "-- skipped (no openssl)"
  else begin
    let dir = Filename.temp_dir "goo-tls" "" in
    let key = Filename.concat dir "key.pem" and cert = Filename.concat dir "cert.pem" in
    let rc =
      Sys.command
        (Printf.sprintf
           "openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 -nodes -days 1 -subj \
            /CN=localhost -addext subjectAltName=DNS:localhost,IP:127.0.0.1 -keyout %s -out %s 2>/dev/null"
           (Filename.quote key) (Filename.quote cert))
    in
    assert (rc = 0);
    let port = 20000 + (Unix.getpid () mod 20000) in
    let devnull = Unix.openfile "/dev/null" [ O_RDWR ] 0 in
    let server =
      Unix.create_process "openssl"
        [| "openssl"; "s_server"; "-quiet"; "-www"; "-accept"; string_of_int port; "-cert"; cert; "-key"; key |]
        devnull devnull devnull
    in
    Fun.protect
      ~finally:(fun () ->
        Unix.kill server Sys.sigterm;
        ignore (Unix.waitpid [] server))
      (fun () ->
        wait_for_port port;
        let url = Printf.sprintf "https://127.0.0.1:%d/" port in
        let pinned fingerprint = X509.Authenticator.cert_fingerprint ~time:(fun () -> None) ~hash:`SHA256 ~fingerprint in
        let fingerprint =
          X509.Certificate.fingerprint `SHA256 (Result.get_ok (X509.Certificate.decode_pem (read_file cert)))
        in
        Goo.run (fun () ->
            print_endline "-- https client";
            (match C.get ~timeout:5. url with
            | _ -> check "self-signed rejected by the default CA bundle" false
            | exception C.Error m ->
                check "self-signed rejected by the default CA bundle" (starts_with ~prefix:"tls: certificate rejected" m));
            (match C.get ~timeout:5. ~authenticator:(pinned (String.make 32 'x')) url with
            | _ -> check "wrong pin rejected" false
            | exception C.Error m -> check "wrong pin rejected" (starts_with ~prefix:"tls: certificate rejected" m));
            let r = C.get ~timeout:5. ~authenticator:(pinned fingerprint) url in
            check "pinned GET" (C.status r = `OK && String.length (C.body r) > 0);
            (* s_server -www answers HTTP/1.0 and closes: the pool must notice
               and reconnect. *)
            C.with_pool (fun pool ->
                let ok () = C.status (C.get ~pool ~timeout:5. ~authenticator:(pinned fingerprint) url) = `OK in
                check "pooled GETs" (ok () && ok () && ok ()));
            Printf.printf "\n%d failure(s)\n" !failures));
    if !failures > 0 then exit 1
  end
