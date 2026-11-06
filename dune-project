(lang dune 3.26)

(name goo)

(generate_opam_files true)

(license ISC)

(package
 (name goo)
 (synopsis "A cooperative, multi-domain scheduler built on effects")
 (depends ocaml))

(package
 (name html)
 (synopsis "Typed HTML and CSS builders")
 (depends ocaml))

(package
 (name csp)
 (synopsis "Typed Content-Security-Policy headers")
 (depends ocaml))

(package
 (name datastar)
 (synopsis "Datastar attributes, actions and SSE events")
 (depends ocaml html yojson web))

(package
 (name web)
 (synopsis "HTTP/1.1 and WebSocket server and client on Goo")
 (depends ocaml goo html uri logs base64 digestif angstrom faraday bigstringaf httpun-types))

(package
 (name caqti-goo)
 (synopsis "Caqti on Goo (Postgres)")
 (depends ocaml goo caqti caqti-driver-postgresql mtime ipaddr domain-name uri logs))
