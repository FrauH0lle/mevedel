# Project live collaboration from host-authoritative state

Status: accepted

Live collaboration keeps the original Emacs process authoritative and exposes
only an allowlisted semantic projection of its canonical session state. A
browser viewer is an independent presentation adapter: it never receives an
Emacs server capability, session lease, execution target, filesystem access,
or tool authority, and its scrolling and disclosure state do not affect the
host view. Structurally excluded records remain host-only, while included
visible prompts, responses, paths, source, and tool results are shared verbatim
and may contain secrets; the projection is not a redaction boundary. This
rejects terminal or frame mirroring because read-only transport prevents useful
navigation while writable transport grants Emacs-level authority, and it
rejects deriving collaboration from durable publications because those serve
cold resume rather than a live running host. Reachability and any future relay
remain transport layers around the same projection and do not acquire host
authority.

The repository-owned spike covers the loopback listener, packaged viewer,
WebSocket authentication and lifecycle, exact origin policy, bounded output,
per-frame acknowledgement tokens, and teardown. External HTTPS tunnel and
browser evidence is a manual follow-up:
this environment has no tunnel credentials or configured endpoint, so that
item is unavailable and remains unrun. No tunnel is started or simulated.

## Spike evidence

On 2026-08-13, with GNU ELPA `web-server` available (set `MEVEDEL_WEB_SERVER_DIR` when the installed tree is not the pinned 0.1.2 directory), these
repository-owned checks passed outside the restricted test sandbox:

- `timeout 20s ./test/collaboration-transport-spike.sh`: served the packaged
  viewer over one `127.0.0.1` listener, rejected a wrong Origin, completed a
  WebSocket ping/pong exchange, and reported listener teardown passed.
- `timeout 30s ./test/collaboration-product-spike.sh`: authenticated one
  browser client, exercised the UTF-8 snapshot and read-only inbound boundary,
  checked the one-guest and stale/auth lifecycle, rejected forged and missing
  acknowledgement tokens without advancing output, exchanged matching
  ping/pong, disconnected a real non-reading guest at the bounded output queue,
  closed an authenticated slowloris frame at the idle deadline, observed the
  final ended status, and reported listener teardown. It passed twice
  consecutively.

The product client retains no token or log after the shell trap removes its
temporary directory. The external HTTPS tunnel and real-browser item remains
unrun for the availability reason above; the loopback result must not be read
as evidence for public reachability.
