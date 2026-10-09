# Project live collaboration from host-authoritative state

Status: accepted

## Current decision

The original Emacs process owns live session state and execution. A browser is
an independent presentation adapter over an allowlisted semantic projection of
the canonical transcript, agents, tasks, artifacts, and interactions. It receives
neither an Emacs server capability nor direct session lease, filesystem, target,
or tool-execution authority. Visible content is shared verbatim and can contain
secrets; projection is not redaction. Browser scrolling and disclosure state do
not affect the host view.

Emacs dials the repository's self-hosted relay for local and remote sharing. The
relay serves the viewer and routes AES-256-GCM sealed frames without the room
key. Link fragments carry bearer credentials; each shared session has its own
room, key, guests, and links. The optional host token restricts relay room
creation independently of guest bearer authority. [ADR 0114](0114-tie-collaboration-room-lifetime-to-host-share.md)
owns room lifetime, reconnect, and explicit share revocation.

Link tiers grant progressively more typed actions:

- **View:** inspect projected state and fetch already-published artifacts.
- **Full:** additionally queue prompts, interrupt the running request, answer
  eligible interactions, invoke host-admitted commands or skills, and edit
  shared whiteboards and documents through the host.
- **Owner:** additionally change permission mode and create a same-workspace
  isolated session directly. A full-link creation request instead requires
  approval from Emacs or an owner-link guest.

The host validates credentials, action, audience, byte limits, and current
admission at receipt and, for queued invocations, delivery. Free text never runs
a slash or skill parser. Browsers can combine up to six explicitly selected
skills in one prompt; the typed names are checked independently of literal
message arguments, and cannot accompany a slash command. Directive-scoped
prompts select discussion only.
Interaction answers share the per-string input bound used by prompts.
Attachments have allowlisted types and host-generated filenames. Browser input
uses the ordinary pending-input path; guest attribution is visible to users but
excluded from model context. Browser permissions are one-shot, not a way to
install reusable rules. Save, Rewind, Fork, lease transfer, publication, and
execution-target changes remain host operations.

Tool display comes from the host's bounded semantic presentation tree and
selected direct-tool identity. Display metadata is allowlisted rather than
exposing raw internal render data. Delivered Skill dependencies are historical
captured bodies, not current file reads. Each browser owns nested disclosures.

Artifact cards are settled live or archived transcript records; bytes are fetched on demand by
record ID, resolved and bounded by the host. HTML runs in a sandboxed iframe
with scripts permitted but no same-origin authority and a restrictive CSP.
Artifacts live in the workspace artifact store, not in relay file storage. See
[Artifact store](../view.md#artifact-store).

Compaction reduces model context without withdrawing browser history. Earlier
segments load as read-only disclosures through the existing session reader.
The room caches only their published artifact metadata, keeping sidebar access
independent of the current context segment; transcript bodies load on demand.

Shared whiteboards and documents allow direct browser editing through typed,
host-committed operations. [ADR 0120](0120-edit-shared-content-through-the-session-host.md)
owns this extension beyond model-authored artifacts.

Opt-in notifications use browser-native Push with room-scoped service workers.
The host forwards endpoint routing metadata to the relay, which sends an empty
VAPID-authenticated push. The worker shows generic text; opening the viewer
retrieves sealed detail. Focused viewers suppress push, and browsers without an
active subscription use live-page notifications. Bearers can persist in browser
storage for tab reload, room navigation, and notification-enabled PWA recovery;
that persistence is disclosed and does not extend a stopped share's authority.

[Browser collaboration](../collaboration.md) owns commands, configuration,
protocol surfaces, browser storage, and recovery behavior.

## Rationale and consequences

A structured projection grants only the actions it describes. Terminal or frame
mirroring would expose Emacs input; durable publications would make live sharing
depend on cold-resume storage. The host therefore projects live authoritative
state, while a content-blind transport supplies reachability.

Dialing out removes Emacs' public-listener responsibilities and its HTTP-server
dependency. Typed input makes phone control useful without handing out arbitrary
Emacs execution. Per-session credentials keep one shared room from granting all
other sessions; switching rooms is browser navigation rather than changing the
session behind a bearer.

Content-blind relay transport still exposes connection/routing metadata, and
browser push routing exposes endpoints. Bearer links authorize their holders,
not named people; projected secrets and locally retained credentials remain
within that deliberate sharing boundary. Untrusted artifact scripts must not
inherit the viewer origin that holds decrypted content and credentials.

## Decision history

- **Browser continuity, 2026-09-24:** compaction removed two still-existing HTML
  artifacts from a live room because the sidebar used only current-segment
  records. Archived publication records now contribute to its artifact catalog,
  and expandable archived segments preserve access to earlier conversations.
  This reuses canonical session storage and projection, without publishing
  arbitrary files or adding a second durable transcript store.

The following changes belong to ADR 0099 unless another ID is named.

- **Initial loopback projection, August 2026:** the host listened on localhost
  through `web-server`, with a read-only viewer, one guest, Origin checks,
  acknowledgment windows, and bounded output. Transport/product spikes exercised
  authentication, wrong Origins, ping/pong, stale guests, forged acknowledgments,
  non-reading guests, slowloris frames, and teardown. They did not verify public
  reachability; external tunnel/browser checks lacked a configured endpoint.
  Those old test-run reports are working evidence, not the current relay contract.
- **Relay and typed input, 2026-08-17:** multi-device use required an external
  tunnel for the loopback design. Listener defenses and a `web-server` recipe
  collision with `simple-httpd` existed only because Emacs was serving. An
  outbound content-blind relay removed that burden. Structured input also showed
  that writable transport need not imply Emacs-level control, so write-token
  bearers gained prompt and interrupt actions. Tracing interaction answers found
  a second model-input channel without the prompt byte bound; the same per-string
  bound now covers all guest text, enforced by the host.
- **Host authentication, 2026-08-29:** an open endpoint protected ciphertext
  confidentiality but let strangers consume rooms, connections, and outbound
  Push resources. Host-token authentication addressed that operator cost. Making
  it mandatory briefly imposed credential distribution even for localhost-only
  relays, so tokenless local/test use remains an explicit option.
- **Scoped input and attachments, 2026-08-29:** replying while viewing a directive
  previously sent ordinary chat elsewhere. The filter became an explicit discuss
  scope. The photo-only attachment limit was unnecessary because Read already
  interprets typed files; PDF and text attachments reuse it. The decoded limit
  fell from 1.5 MiB to 1.25 MiB because base64 plus prompt text could exceed the
  relay's 2 MiB frame limit. Images can be downscaled; other oversized files are
  refused rather than truncated.
- **Explicit invocation and per-session rooms, 2026-08-29:** phone use needed
  planning, review, and compaction beyond free text. Typed frames and host-curated
  admission made those invocations explicit while keeping free text inert.
  Replacing a process singleton with per-session rooms removed its scaling limit;
  a proposed in-room session switcher was rejected because it broadened bearer
  authority unnecessarily.
- **PWA persistence and Push, 2026-08-30/31:** a suspended Home Screen viewer lost
  its in-memory key. Persisting an opted-in bearer enabled reconnect, but did not
  wake the app. Empty Web Push added wake-up without exposing session content to
  push services. Persistent VAPID identity preserves subscriptions across relay
  restarts; per-room service-worker scopes prevent one room's opt-out or key
  replacement from affecting another. The host replays routing metadata after
  transport reconnect so a network blip does not strand subscriptions.
- **Artifacts, 2026-09-01:** guests needed to inspect model-created HTML and
  Markdown. Relay-side file hosting was rejected because it added content trust,
  storage, quotas, and independently leaking URLs. Selected-only ApplyPatch
  records establish what actually applied, while the host serves bounded bytes
  on demand. Existing portable manifests carry artifacts through resume and
  forks; Rewind preserves the current free-form artifact folder.
- **Owner authority, 2026-09-02:** daily phone control still required returning to
  Emacs to change mode or create separate work. A third bearer tier grants those
  two typed authorities, without broadening browser access to durable session
  operations. New rooms receive fresh credentials; requester replies preserve
  its tier. Other owner guests may approve requests and receive owner-room offers.
- **Tool presentation, 2026-09-08:** a Skill call displayed properly in Emacs but
  appeared as raw ToolCall/dependency reminders in the browser because projection
  dropped renderer metadata. Bounded semantic presentation corrected this, with
  host/viewer protocol 3 deployed together and the relay rebuilt to embed the
  corresponding viewer. It added no compatibility adapter.
- **Nonblocking connection, 2026-09-08:** room recreation spent 135.29 of 135.35
  seconds in `websocket-open` while IPv6 stalled before IPv4 succeeded; projection
  took 1 ms and QR encoding 4 ms. Asynchronous dialing plus a collaboration-only
  adapter postponing websocket's handshake until connection reduced the same
  unreachable-address probe to about 1 ms. A TLS fixture delaying negotiation
  by half a second returned from dialing in under 1 ms, accepted a trusted
  certificate, and rejected an untrusted issuer. This bounds UI blocking, not
  DNS reachability or IPv4 fallback time.
- **Room lifetime, ADR 0114:** two active sessions expired under the old one-hour
  host timer. An idle timeout could not distinguish passive readers from idle
  tabs, and the relay's maximum-age sweep only forced automatic recreation.
  Explicit host-share lifetime replaced both absolute-age policies; ADR 0114
  retains the reconnect, keepalive, and revocation rationale.
- **Room storage boundaries:** Dismiss originally removed a saved room link,
  and reload replayed old approvals. Notices and remembered rooms now have
  separate lifetimes. Whole-link storage also duplicated rooms and let tab saves
  overwrite each other; merging by room ID preserves the strongest received
  bearer, while each tab caps display to its own tier. Without that cap a full
  tab could use an owner tab's stored offer to open as owner. Per-tab current-share
  storage prevents reload from choosing an older notification-enabled room.
  The original browser manual supplied no separate dates for these corrections.

- **Combined skill selection, 2026-09-20:** the single armed chip and automatic
  menu dismissal prevented composing several skills on one message. The menu
  now stays open, selected skills travel as an explicit name array, and the
  existing planner applies them together. Admission is checked for every name
  on receipt and delivery; pasted message tokens remain inert.
