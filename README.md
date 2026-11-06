# goo

A dune workspace of OCaml libraries for web apps on Goo, with no Eio.

| Package | Library | What |
|---|---|---|
| `goo` | `goo` | Cooperative, multi-domain scheduler built on effects: tasks, scopes and cancellation, sockets, channels, mutexes/conditions, a TCP server |
| `html` | `html` | Typed HTML (`Html`) and CSS (`Css`) builders |
| `csp` | `csp` | Typed Content-Security-Policy headers |
| `datastar` | `datastar` | Datastar attributes, action expressions and SSE events |
| | `datastar.web` | Datastar requests and SSE responses on `web` |
| `web` | `web` | HTTP/1.1 + WebSocket server and client on Goo (httpun), routes, responses |
| `caqti-goo` | `caqti-goo` | Caqti on Goo, Postgres only, with per-domain pools |

`vendor/` holds patched httpun-ws and the httpun / gluten it builds against
(see `vendor/README.md`).

```sh
nix develop
dune build
dune test
```

`bench/` has two servers for wrk: `runtime_srv` (Web) and `raw_srv` (bare
`Goo.Server`).
