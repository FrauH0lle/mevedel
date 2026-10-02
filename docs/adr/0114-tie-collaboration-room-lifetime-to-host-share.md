# Tie collaboration room lifetime to the host share

Status: accepted

Supersedes the host TTL and relay maximum-age decisions in ADR 0099.

## Current decision

A browser collaboration room follows one unambiguous lifecycle: the
logical host share. It remains valid until the user stops sharing, the owning
session or data buffer ends, or Emacs exits. Temporary network loss does not
end the share; the host transport reconnects and guests may rejoin with the
same bearer link. Starting a later share after teardown generates fresh
credentials.

The relay likewise imposes no absolute room age. It retains a room only while
its host connection is live, garbage-collects it immediately on disconnect,
and uses WebSocket keepalive to detect dead peers. The host sends its own periodic WebSocket ping so a stale connection after
suspend becomes observable and reconnects. Relay pings alone are answered inside
websocket.el and do not notify the host application.

A workspace lobby is the one deliberate exception. Its purpose is a
bookmark that survives restarts, which a share-scoped credential cannot be,
so its credentials persist in the workspace state directory and a restarted
Emacs recreates the same relay room. Rotating the credentials, not stopping
the lobby, is its revocation operation. The rooms a lobby opens follow the
ordinary share lifecycle; the lobby hands out a fresh link to each one on
request, so they need no persistence of their own.

## Consequences

The accepted cost is that a bearer remains valid for the entire host share,
even across periods with no guests. Explicitly stopping the share is the
revocation operation. Artifacts inherit the same room lifetime and need no
separate retention policy.

A lobby link is a long-lived bearer: it stays valid across restarts and
periods without guests until rotated. A full or owner lobby link can open,
and an owner link create, sessions in that workspace, and a full link can
read, upload and remove the project's files
([ADR 0122](0122-let-full-links-change-project-files-directly.md)), so it
carries more standing authority than any single room link.

Shared whiteboards and documents follow the same access lifetime. Their
accepted content is durable session state and survives the room; ending a
share revokes browser access, not the host's original. Unsynchronized browser
drafts retain a recovery download but do not authorize writes into a later
share. See [ADR 0120](0120-edit-shared-content-through-the-session-host.md).

## Decision history

The lobby exception was added when a phone needed one stable entry point to
a host's sessions. Every session room got a fresh link per share, and the
browser's stored room list held only links that died with their shares, so
there was no address to bookmark. Persisting one per-workspace credential
gave that address while every session room kept its share-scoped bearer.

Two real collaboration sessions ended while still in use because the
host-side one-hour timer measured absolute room age, not inactivity. An idle
timeout was considered, but passive reading produces no application traffic
and a connected browser tab is not reliable evidence of user attention. Any
activity rule would therefore either terminate a reader or let an abandoned
tab retain the room indefinitely.

The former maximum-age
sweep only forced a reconnect: the Emacs host automatically recreated the
same room, so the sweep neither bounded share lifetime nor revoked its bearer.

Keepalive has to run in both directions for the reconnect this lifecycle
relies on. websocket.el answers a relay ping inside its own filter and never
surfaces one, so the host cannot observe the relay's pings stopping: a
suspended machine woke with a socket its kernel still called open and a room
the relay had already collected, and the bearer link stayed dead until the
session happened to send something. The host therefore writes its own ping on
the same interval. The relay's side is closed by then, the peer answers with
a reset, and the redial re-creates the room -- which is what makes "temporary
network loss does not end the share" true of a suspend and not only of a blip
the host was awake for.
