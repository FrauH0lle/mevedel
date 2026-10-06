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
and uses WebSocket keepalive to detect dead peers. The host sends its own
periodic WebSocket ping and requires inbound bytes within a liveness window,
so a connection that died silently, after a suspend or a network change, is
dropped and redialed while guests are still retrying. Relay pings and pongs
are answered or consumed inside websocket.el and do not notify the host
application, so the host observes them at its connection's process filter.

A workspace lobby is the one deliberate exception. Its purpose is a
bookmark that survives restarts, which a share-scoped credential cannot be,
so its credentials persist in the workspace state directory. Its lifetime is
the user's intent rather than one Emacs process: a lobby runs until it is
stopped, its workspace root is recorded in the user directory while it runs,
and an Emacs exit leaves that record, so the next `mevedel-install` restarts
the lobby on the same relay room. Stopping the lobby removes the record.
Rotating the credentials, not stopping the lobby, is its revocation
operation. The rooms a lobby opens follow the ordinary share lifecycle and
end with Emacs; the lobby hands out a fresh link to each one on request, so
they need no persistence of their own.

## Consequences

The accepted cost is that a bearer remains valid for the entire host share,
even across periods with no guests. Explicitly stopping the share is the
revocation operation. Artifacts inherit the same room lifetime and need no
separate retention policy.

A lobby link is a long-lived bearer: it stays valid across restarts and
periods without guests until rotated, and a lobby left running is live again
whenever mevedel is installed, until it is explicitly stopped. A full or owner lobby link can open,
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

The lobby first persisted only its credentials: a restarted Emacs could
recreate the same room, but only when someone ran `/collab lobby` again.
Nothing did so at startup, so after every restart the bookmark showed "Room
closed" until the user returned to the keyboard, which defeated the phone
entry point the lobby exists for. The running state is therefore recorded
too, as a list of workspace roots under `mevedel-user-dir`, because startup
has no workspace from which to find per-workspace state. Session rooms were
not given the same treatment: their bearer is scoped to one share by design,
and the lobby already hands out fresh links to them.

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

The ping alone relied on that reset. A network change leaves no peer to send
one: packets to the old address are dropped, and the kernel keeps
retransmitting for minutes before it reports the connection broken, while
browser guests give up after three. The host now also requires inbound bytes
at least every 45 seconds and pings every 15 seconds instead of 30,
so a healthy connection always has a recent pong. websocket.el consumes
pongs without a callback, so the host wraps its connection's process filter
to timestamp every read instead of patching the library. The window spans
several pings, and the host reads any input waiting in the socket before
judging, because Emacs runs due timers before reading output after a busy
spell. It also outlasts the relay's 40-second dead-host detection, so a
redial normally finds the room collected rather than meeting the relay's
second-host refusal.
