# Project live collaboration from host-authoritative state

Status: accepted (amended 2026-08-29, twice; 2026-08-30; 2026-08-31;
room lifetime superseded by ADR 0114)

Live collaboration keeps the original Emacs process authoritative and exposes
only an allowlisted semantic projection of its canonical session state. A
browser viewer is an independent presentation adapter: it never receives an
Emacs server capability, session lease, execution target, filesystem access,
or tool execution authority, and its scrolling and disclosure state do not
affect the host view. Structurally excluded records remain host-only, while
included visible prompts, responses, paths, source, and tool results are
shared verbatim and may contain secrets; the projection is not a redaction
boundary. This rejects terminal or frame mirroring because a structured
projection shares exactly what it describes instead of Emacs input, and it
rejects deriving collaboration from durable publications because those serve
cold resume rather than a live running host. Reachability and any future
relay remain transport layers around the same projection and do not acquire
host authority.

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

## Amendment: relay transport and write-token interaction input (2026-08-17)

Two decisions moved when the relay transport replaced the loopback listener.

**The host is a dialing client, not a listener.** The loopback HTTP/WebSocket
listener is replaced by a small self-hosted content-blind relay (`relay/` in
this repository); Emacs dials it for local and remote sharing alike, and the
relay serves the static viewer. What moved the decision: the first real usage
is multi-device (a phone or laptop reaching a session hosted elsewhere),
which a loopback listener cannot serve without an externally operated tunnel
this ADR already declined to own; and the listener's defense obligations
(origin policy, pre-upgrade caps, slowloris deadlines, ACK-window pump,
single-viewer gate) plus the `web-server` dependency -- whose MELPA recipe
collides with `simple-httpd` under straight.el -- existed only to make Emacs
a safe server. Dialing out deletes the obligation rather than servicing it.
Frames are sealed end to end (AES-256-GCM, key only in the URL fragment), so
the relay carries strictly less trust than the tunnel the original decision
contemplated: it routes opaque envelopes by a plaintext peer-id prefix and
holds no state beyond live connections.

**Write-token bearers may submit input.** A full share link carries a write
token; its bearer may submit prompts into the ordinary pending-input queue
(badged with the guest name, which never enters model context) and interrupt
the running request. What moved the decision: the original "writable
transport grants Emacs-level authority" dichotomy was about terminal
mirroring, and the projection architecture dissolved it -- a structured
guest frame grants exactly the action it describes, not Emacs input.

Amended: that claim held for the action a frame names but not for its size.
The prompt frame carried a byte budget while the interaction-answer frame
carried none, and an answer reaches the same pending-input queue and the
same model-visible context a prompt does -- so the answer path was a second
input channel with the prompt path's authority and none of its budget.  What
moved the decision was tracing where an answer lands: the same per-string
budget now covers every guest-supplied string, enforced host-side, since the
viewer is not the only client a bearer can use.  The budget is per string
rather than per frame because a questionnaire's answer count comes from the
model's own question list, not from the guest. The
host-user-on-their-own-phone case makes a session that cannot be steered
until the user returns to Emacs strictly worse than one they can prompt and
stop remotely, and the authority boundary is explicit: possession of the
full link is the credential, bounded by per-share random rooms and the
explicit host share lifecycle described by ADR 0114. Execution still happens
only on the host, view links carry no input authority, and lease transfer,
save, rewind, fork, publication, and execution-target selection remain
impossible from the browser regardless of link strength.

The spike evidence above described the loopback listener and remains
historically accurate for that transport; it does not describe the relay
implementation. The relay contract is covered by `go test` in `relay/` and
by the elisp stub-relay suite in
`test/test-mevedel-collaboration-transport.el`.

## Amendment: room creation is authenticated, and guest input is scoped and typed (2026-08-29)

Three decisions moved after daily phone use of the viewer.

**The relay can authenticate room creation.** The landing note accepted an
open host endpoint because a stranger's room carries only their own
ciphertext. That reasoning was about confidentiality and stays true; what it
did not cover is that an open endpoint lets anyone create rooms, hold idle
connections, and drive outbound Web Push on a server the operator runs for
themselves. The optional `-host-token`, checked at the host upgrade and
answered 404 on mismatch, closes that without touching the trust model: the
relay still holds no key and reads no payload. Public-facing deployments
should configure it. Omitting it explicitly selects tokenless mode for
localhost and test deployments.

The token was briefly mandatory. Deployment experience moved that decision:
a server-wide credential had to be generated and copied to every Emacs
installation even when the relay was reachable only on localhost. Making the
existing token optional preserves the public-deployment security boundary
without imposing it where the network boundary already supplies isolation.
Guests remain tokenless, because a guest's authority is the bearer link and
adding a second secret would not make the first one stronger.

**A guest's transcript filter is also a send scope.** The filter was
view-only, which meant reading a directive thread and replying put the reply
somewhere the guest was not looking. A prompt frame now carries the
directive it was composed under. Guest-scoped input is restricted to
`discuss`: the viewer knows a directive id and nothing else, and choosing
between plan, retry, and request-changes would need both a UI the guest does
not have and an authority argument the bearer link does not make. Discussion
is also the directive action that mutates nothing, which keeps the boundary
where the rest of this ADR puts it. Skill-inertness survives for free --
directive dispatch never plans skills and already refuses slash lines -- but
that is a property of the current actions, not of the scope mechanism: a
scoped action that did plan skills would have to honour `:inert-skills` at
the delivery seam.

**Attachments are typed, not photographic.** The attach path accepted
`image/*` because it was built for camera photos. Nothing downstream was
image-specific: an attachment becomes an `@file` mention with a read grant,
and Read decides text or media from the extension. The allowlist therefore
widens to PDFs and text types rather than growing a parallel path, and the
"text versus media" question the backlog recorded turns out not to be one.
Saved names stay host-generated, so a guest filename can never steer a
write.

What this cost: the decoded attachment budget dropped from 1.5 MiB to
1.25 MiB. Base64 costs a third and the prompt text shares the frame, so the
old budget could produce a prompt frame over the relay's 2 MiB read limit --
which the relay answers by closing the connection. Images are downscaled to
fit; anything else is refused, because a log cannot be made smaller by
resampling.

## Amendment: allowlisted skill frames and per-session rooms (2026-08-29)

Two decisions moved in the viewer overhaul that followed the daily-use
review.

**A typed skill frame from a host-curated allowlist may invoke a skill.**
The previous amendment kept guest input skill-inert, and for free text that
holds unchanged and permanently: no slash parser runs over anything a guest
types, and `$skill` tokens stay literal. What moved is that skill invocation
now exists as its own frame kind. The deferral's real objection was
authority -- which trusted prompt bodies may an untrusted bearer inject, and
how would a guest even discover them -- and a host-curated
`mevedel-collaboration-guest-skills` allowlist answers both: the host picks
the roster, the welcome frame publishes it to write-token guests as
buttons, and the frame names exactly one entry, validated against the same
defcustom when it arrives and again when the queued invocation is
delivered, so shrinking the list takes effect immediately. The invocation
enters the ordinary pending-input queue and runs the same deterministic
slash-planning path host-typed input runs, so remote invocation has no
special semantics. What moved the decision: daily phone control stopped at
free text, and the workflows that make controlling a session useful --
plan, review, compaction -- were exactly the part the phone could not
reach.

**One room per shared session, never a session switcher inside a room.**
The landing note accepted one room per Emacs process as a limitation and
reserved an additive `session-list`/`switch-session` extension. The
limitation fell -- the singleton became a registry keyed by the owning data
buffer, and each shared session now has its own key, bearer links, and
guest set -- but the reserved extension is rejected rather than deferred:
one bearer link that grants every shared session is a strictly worse
credential than one link per session, and "switching" on a guest device is
a browser tab. Interaction prompts now also route only to the room whose
session owns them, which the single-room design never had to distinguish.

## Amendment: notification opt-in retains the browser bearer (2026-08-30)

The statement that the room key lives only in the URL fragment no longer
describes notification-enabled viewers. Opting into browser notifications now
stores the full bearer link in origin-scoped local storage so an installed PWA
can reconnect after it has been suspended and relaunched. What moved the
decision was the browser lifecycle: an in-memory key is lost across that
ordinary Home Screen lifecycle.

The viewer clears the stored bearer when it observes the room ending or its
bounded reconnect window expires. If the viewer is never opened again, site
data may retain the link beyond the room's lifetime; room collection and the
explicit host share lifecycle still makes that bearer unusable after teardown.
The disclosure states this persistence explicitly. The relay remains
content-blind because it still receives ciphertext rather than the key.

## Amendment: content-blind background Web Push (2026-08-31)

Daily phone use showed that reconnect credentials alone do not wake a
suspended viewer. Notification opt-in now registers a browser-native Push API
subscription through a service worker and sends its endpoint, inside the
sealed guest channel, to the authenticated Emacs host. The host forwards only
the guest id, current relay peer id, and endpoint as relay control metadata.
Pending interactions target the writable subscribers that received the
interaction; a completed turn wakes every subscriber in the room.
The viewer reports whether it is actively focused, and the relay suppresses
Web Push for active subscriptions. A push-enabled viewer receives Web Push
when its page is away, suspended, or disconnected; only browsers without Push
use the live-page Notification fallback. The Emacs host retains each endpoint
as routing metadata and replays it as inactive when its relay transport
reconnects, so an ordinary host network blip does not strand a suspended
viewer.

The relay sends an empty Web Push POST authenticated by a VAPID key persisted
in an operator-owned state file. Persistence keeps existing restricted Push
subscriptions valid across relay restarts. Consequently the browser push
service receives no prompt, transcript,
room id, room key, notification title, or other session content. The service
worker displays fixed generic text, and opening it lets the viewer reconnect
for end-to-end-sealed state. Endpoints are bounded HTTPS URLs and delivery
refuses private or otherwise non-public resolved addresses. Browser opt-out
and true room teardown delete the subscription; relay connection teardown may
discard its copy because the live Emacs room can replay it. No push library or
provider-specific service is added; the relay implements the small VAPID
request with Go's standard library and the viewer uses platform APIs.

Each room uses its own service-worker registration scope, derived locally from
the bearer fragment. The scope is registration metadata, not a fetched URL;
the service-worker script request and every push remain credential-free. This
keeps one room's opt-out, opt-in preference, or VAPID-key replacement from
affecting another room. A notification click reconstructs the matching room
link as a URL fragment, which browsers do not send to the relay.

This is Web Push, not an iOS-only path: it covers supporting Android browsers
and desktop PWAs too. iOS/iPadOS requires the viewer to be installed on the
Home Screen before Web Push is available. If that installed app starts with a
fresh storage partition, it asks the user to paste the full share link and
parses it entirely in the client; the bearer is never submitted as form or
request data. Browsers without an active Push subscription keep the live-page
Notification fallback.

## Amendment: session artifacts as records, bytes on demand (2026-09-01)

The model can now build things a guest is meant to look at -- an HTML
mockup, a Markdown document -- and the room needed a way to show them.
The alternative considered was relay-side file hosting with its own TTL;
it was rejected because it ends the relay's content-blindness and adds
storage, quotas, and a URL that leaks outside the room. Instead an
artifact is a file the model applies into `<save-path>/artifacts/` with the
canonical ApplyPatch tool. Persisted selected-only patch render data is the
publication seam: it reflects what patch review actually applied, unlike the
authored patch arguments, and supports several artifact destinations in one
call. The folder remains the cockpit inventory and byte source; collaboration
authority is the settled transcript record. Each record carries only name and
size (a later-deleted file projects as missing), while its resolved path stays
host-side in an unserialized field. The relay is unchanged.

Bytes travel only on demand. A guest's `artifact-get` names a record id,
which the host resolves against root records or agent records already
published to that guest -- never a guest-supplied path. Agent artifact ids are
first hashed with their canonical agent path because record ids are otherwise
only transcript-local. The host accepts only nonnegative JavaScript-safe
request ids, re-verifies the resolved path inside the artifacts directory,
bounds in size, throttles per guest, and answers as targeted sealed
`artifact` frames chunked under the wire bound. Any guest may fetch:
the card is read state like the tool record that carries it. The viewer
renders HTML inside `<iframe sandbox="allow-scripts">` without
`allow-same-origin` (the pair together would hand the artifact the
viewer origin holding the decrypted transcript and room key) and with a
prepended `default-src 'none'` CSP, so the bundled `artifact` skill's
self-contained rule is enforced, not merely requested. "Open in tab"
opens a same-origin shell synchronously from the click with the bytes
already in hand, containing only the same sandboxed frame. Guests
author artifacts through the model only; there is no upload path.
An artifact's public lifetime remains the room's, so no artifact-specific
retention exists.

Portable session publication now includes recursive regular files below
`artifacts/`; a session-local tombstone removes a deleted file from an
overlaid immutable manifest. Existing publication materialization therefore
carries artifacts through Resume, Save As, and both Fork forms without a
second artifact manifest. Rewind deliberately preserves the current folder:
these files are durable session state, not per-turn file snapshots.
The cockpit and the next save still consume the fixed folder, so an owned cold
Resume replaces that cache from digest-verified manifest bytes after fencing
and head revalidation. Read-only inspection does not mutate the cache.
