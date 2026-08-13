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
