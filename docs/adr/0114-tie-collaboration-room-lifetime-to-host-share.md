# Tie collaboration room lifetime to the host share

Status: accepted

Supersedes the host TTL and relay maximum-age decisions in ADR 0099.

Two real collaboration sessions ended while still in use because the
host-side one-hour timer measured absolute room age, not inactivity. An idle
timeout was considered, but passive reading produces no application traffic
and a connected browser tab is not reliable evidence of user attention. Any
activity rule would therefore either terminate a reader or let an abandoned
tab retain the room indefinitely.

A browser collaboration room now follows one unambiguous lifecycle: the
logical host share. It remains valid until the user stops sharing, the owning
session or data buffer ends, or Emacs exits. Temporary network loss does not
end the share; the host transport reconnects and guests may rejoin with the
same bearer link. Starting a later share after teardown generates fresh
credentials.

The relay likewise imposes no absolute room age. It retains a room only while
its host connection is live, garbage-collects it immediately on disconnect,
and uses WebSocket keepalive to detect dead peers. The former maximum-age
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

The accepted cost is that a bearer remains valid for the entire host share,
even across periods with no guests. Explicitly stopping the share is the
revocation operation. Artifacts inherit the same room lifetime and need no
separate retention policy.
