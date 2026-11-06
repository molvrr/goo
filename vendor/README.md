# Vendored dependencies

Vendored so we can patch httpun-ws (see "Local patches"). Dune prefers these
over installed copies; the root `dune` has `(vendored_dirs vendor)`. Anything
compiled against a vendored library must be vendored too, hence httpun and
gluten (used by httpun-ws).

| Dir | Upstream | Version | Packages kept |
|---|---|---|---|
| `gluten/` | github.com/anmonteiro/gluten | 0.5.2 | `gluten` (`gluten-eio` removed) |
| `httpun/` | github.com/anmonteiro/httpun | 0.2.0 | `httpun` (`httpun-eio` removed) |
| `httpun-ws/` | github.com/anmonteiro/httpun-ws | 0.2.0 | `httpun-ws` |

`httpun-types` comes from nix.
## Local patches

Marked `LOCAL PATCH` in the source.

1. `httpun-ws/lib/websocket_connection.ml`: `_next_read_operation` advanced the frame queue
   by only one frame per call in the ``Read` case, so when several frames arrived in one read the
   later ones stayed queued while the runtime blocked on the socket. It now recurses like the
   ``Close` case.
2. `httpun-ws/lib/websocket.ml`: `Close_code.of_code`/`of_code_exn` indexed the code table with
   `code land 0b1111` instead of `code - 1000` (1000 became 1008, 1009 became 1002, ...).

Otherwise the files match upstream except for build trimming:
- `piaf/dune`: reduced to `(vendored_dirs vendor)` (removed the gh-actions rule that ran `nix eval`).
- `gluten/dune-project`: removed the lwt/async/mirage package stanzas.
- `httpun/`, `h2/`, `httpun-ws/`: minimal `dune-project` written by hand.

## Planned patches (see notes)

1. `gluten/eio/gluten_eio.ml`: `writev` maps `Connection_reset` to `` `Closed ``; while the reader is yielded, watch the socket for EOF (`await_readable` + `MSG_PEEK`).
2. `httpun/lib/reqd.ml`: `report_error` Streaming branch drains pending body flushes (`close_and_drain`).
3. `h2/lib`: drain the streaming response body when a stream is reset or finishes.
4. `piaf/lib`: `Request_info.closed` promise; cancel the response fiber on close; close the body stream; `ws.ml` pushes `None` on `eof ~error`.
