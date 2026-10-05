# Edit shared content through the session host

Status: accepted

Supersedes ADR 0099's restriction on direct guest authorship for packaged
whiteboards and documents. Its arbitrary HTML artifact viewer, host authority,
and content-blind relay remain in place. ADR 0114 governs access lifetime.

The requirement changed from showing model-produced artifacts to people and
the model editing the same sketch or document simultaneously. Republishing
whole HTML pages cannot preserve concurrent typing, participant undo, or
precise model edits against older reads. Two inspected template variants
supplied design references, but neither supplied mevedel's collaboration or
durability contract.

Use Yjs shared state with a Tiptap document editor. A packaged Node helper
reuses the browser schema to validate candidate state, apply target-checked
agent transactions, and generate exports. Packaged resvg WASM and a font
render matching board PNGs with no guest browser. The helper has a private
stdin/stdout protocol and owns no session files or public listener.

Emacs owns a serialized operation queue per session. It rechecks authority
before committing candidate state through existing session storage. Project
sessions use their target-side lease and immutable publication transaction;
the browser receives a saved acknowledgment only after commit. A tool's
irreversible commit and result delivery defer cancellation settlement so a
committed edit cannot be reported as rolled back. Existing artifact
materialization carries editable state and embedded assets through session
lifecycle operations.

Full and owner bearers may create, import, rename, edit, comment, ask, and point through
typed room actions. View bearers may read and export. Model tools use the
ordinary permission pipeline, including Plan and read-only restrictions.
Neither adapter receives arbitrary paths or execution authority. The packaged
editor runs in an opaque iframe with an item-scoped MessageChannel; the
trusted viewer retains credentials and enforces the action allowlist. Forms
support local editor dialogs; CSP forbids network form submission and fetches.

Geometry is one atomic element property; independent text/style properties
remain separate. Whiteboard content is Excalidraw elements
([ADR 0121](0121-store-whiteboards-as-excalidraw-elements.md)). Same-property concurrent writes follow Yjs's deterministic
ordering, and deletion wins over an in-flight property edit to that record.
The first drag implementation only redrew after release. Move and resize now
render temporary geometry through the same scene and connector renderer, then
commit that geometry on release. Cancellation drops the preview without a CRDT
write; pointer motion does not create a stream of saved revisions.
Move and resize previews also use the existing bounded presence channel. They
contain object IDs and bounding boxes, never CRDT updates or image bytes. The
receiver marks the movement as unsaved, renders it over its committed scene,
and removes it on commit, clear, disconnect, or expiry. After release the
preview carries the pending operation ID so a late packet cannot replace its
acknowledged commit. Content authority, validation, and saved acknowledgement
remain on the host path.

The previous design showed peers only committed geometry. After pipe and queue
fixes, the user still measured roughly two seconds for a rectangle move while
named pointers remained quick. This separated presence latency from the save
round trip. Reusing presence for disposable geometry previews removes that
wait from dragging without broadcasting unvalidated content. In the local
two-browser room test, the preview arrived in roughly 70 ms before release.

New empty documents share an initial text object: real browser testing found
that two separately created empty text objects could normalize into one while
losing a writer's undo history. Agent edits compare each target's content
hash and inverses their exact targets; either rejects the entire transaction
on overlap.

Presence is bounded, disposable traffic. Explicit questions capture committed
content and enter the ordinary pending-input queue; synchronization starts no
model turns. Native imports validate completely and begin a fresh lineage.
There is one current schema, no executable import and no compatibility layer.

Embedded images use the same byte and dimension validator in both item kinds.
The document schema includes Tiptap's image node with embedded sources only;
the host enforces the same limits as browser insertion. Upload, file drop, and
clipboard images share one insertion path per editor, while relative document
positions keep asynchronous file reads anchored through concurrent edits.
Native, HTML, and Markdown document exports retain image data without a separate
asset service or remote fetch. Existing session activity drives visible working
indicators in the room and editor chats; no second request lifecycle is tracked.

Document image transforms preserve `src` as the source and keep normalized crop,
quarter-turn rotation, flips, and a rendered PNG in one `imageEdit` property.
Board images use Excalidraw's `crop`, `scale` and `angle` instead
([ADR 0121](0121-store-whiteboards-as-excalidraw-elements.md)).
This atomic attribute prevents concurrent edits from mixing transformation
settings with another edit's rendered pixels. Browser canvas rendering serves
both editors; ordinary renderers and HTML/Markdown exports use those pixels,
so document consumers do not need custom crop CSS. Native exports retain the
original for reset. The tradeoff is storing both source and rendered bytes;
both count against the existing bounded image and item budgets. Crop previews
are local until Apply, and a changed target refuses the pending transform.

The cost is a local Node runtime and packaged browser/helper bundles. End
users do not install npm dependencies. Missing Node previously left editing
actions clickable until they failed. Availability now uses a read-only request
through the existing serialized helper protocol: actual startup, document schema,
and PNG rendering must succeed before the browser enables editing actions. The
saved catalog remains visible independently. Recheck starts a fresh helper
between jobs to pick up runtime configuration and resource repairs, without
cancelling queued edits or creating durable state. Chat and static artifacts
remain independent of this optional dependency.

Large content and operation histories have explicit bounds. Revert history
retains at most 32 transactions, targeting 4 MiB, and expires older snapshots
before they crowd out an otherwise valid save. The latest transaction remains
revertible, even above that history target, within the 16 MiB item bound.
The Contributions view states its earliest retained revision. Receipt IDs are
not expired with snapshots: replay protection keeps its existing hard ceiling.
Content or receipt limits still require a smaller item or native export/import.

Browser projections carry only contribution IDs, actor, revision, timestamp,
and affected target IDs with a surviving/deleted flag. Full snapshots remain
host-side for reversion and model reads. The browser already receives the CRDT
or delta, so the bridge also omits duplicate materialized content and the full
latest transaction from its replies.

### Decision history: image history saturated shared editing

A board containing a roughly 0.95 MB screenshot reached 15.6 MB of persisted
state; 14.3 MB was repeated before/after contribution snapshots. An ordinary
image move then failed the item limit. Sending those snapshots in broadcasts
also created transfers larger than the relay's bounded guest queue. A count-only
history bound did not control this growth. Byte-bounded retention and compact
browser projections replace that behavior without raising transport limits or
discarding current content, replay receipts, or browser-local Undo/Redo state.

The follow-up trial exposed slow catch-up during typing: the browser queued
another operation every 300 ms even when the previous save took seconds.
It now accumulates unsent updates into one follow-up until the in-flight
operation is acknowledged. Recovery retains both, and the sent operation's
identity and payload stay stable for retries. The helper pipe also assembles
JSON lines from chunks once, with an incremental byte bound, instead of
repeatedly copying and scanning the entire growing message. On a copy of the
reported board, Emacs reply framing fell from roughly 520 ms to 15 ms; durable
commit remains before broadcast and acknowledgement.

Single moves still exposed Emacs pipe backpressure after that fix: writing a
5 MiB request in one `process-send-string` took about 1.2 seconds even with a
helper that only consumed input. Requests now use 1,024-character writes,
preserving UTF-8 and the same bounded JSON-line protocol. On a copy of the
reported board, a complete host patch fell from roughly 1.6 seconds to 0.28
seconds. State authority, validation, persistence, and acknowledgement order
are unchanged; no helper state cache is introduced.


## Usability findings from the first shared session

The first trial on an iPhone 13 mini left the document almost entirely hidden
behind wrapping toolbars and the keyboard. Editors now open in their own tabs,
with independent room connections and a full-size opaque iframe. The original
room retains its composer. The parent sizes the editor to the visual viewport;
formatting stays on one scrolling row and the assistant panel overlays phone workspaces.
A tab URL identifies the item, while credentials continue through the existing
fragment-to-tab-storage lifecycle and never reach the iframe.

The browser design comparison exposed an unrelated editor palette and a
light-only document surface beside the room's dark theme. Room and editor now
share CSS color and typography tokens. The trusted viewer sends its initial
theme and subsequent choices through the existing item-scoped port; the editor
accepts only light, dark, or system fallback. Theme changes do not reload the
editor or alter content, presence, or exported colors. The whiteboard stores
and exports authored colors as drawn; a dark theme displays the canvas and its
elements through Excalidraw's inversion, `invert(93%) hue-rotate(180deg)`,
which keeps hues, while images are inverted back to their own pixels.
The document uses the selected theme. A real parent/iframe phone test caught
the taller header consuming typing space with the keyboard open; compact
short-viewport spacing retains at least 150px of document viewport in that test.

The trial also showed that a contribution row per 300 ms save was unreadable.
Keep those commits and their exact inverse records; group their presentation
by participant name and five-second idle gaps. Agent transactions remain
separate. Local view decorations distinguish newly received agent changes
for eight seconds, fading over the final two. Repeated publications retain
each target's original local expiry; initial history does not trigger a glow.
Attribution remains in Contributions without modifying CRDT content or
exported formatting.

### Decision history: persistent highlights and SVG exports

Highlights originally lasted until a human edited the same element. A board
authored mostly by the assistant consequently kept a permanent glow that
blurred its labels. Short-lived local decorations replace that display while
retaining contribution records and exact inverses.

Whiteboard exports originally used a fixed cream substrate and positioned SVG
`tspan` lines. The substrate differed from the editor, and Qt's SVG renderer
collapsed those lines and omitted marker arrowheads. PNG and SVG now share
an opaque white background, independent of UI theme. Explicit text baselines
and path arrowheads render in the browser, resvg, and Qt without sacrificing
editable SVG text or blank-line spacing.

The whiteboard originally kept its light drawing substrate in a dark theme,
so authored stroke and image colors would remain faithful. With per-board
canvas colors the light board read as out of place inside the dark room. A
display-only inversion keeps hues, re-inverts images, and changes neither
stored colors, exports, nor the model's board images, so faithfulness no
longer needs a light substrate.


## Pointing and menu refinement

A later browser trial reproduced the Shared menu compressing inside the room
activation row, while that row stretched adjacent pill buttons to the menu's
height. Shared now occupies its own disclosure section, with wrapping creation
controls and a separate item list. This preserves the room composer and avoids
coupling menu height to button proportions.

Laser pointing now follows mouse/pen hover, or a touch drag, rather than requiring
a mouse-button gesture. Screen-sized luminous tips, compact labels, and independently
fading curved trails make it distinguishable from ordinary cursor presence.
Remote cursors transition from their displayed position to received samples.
Laser packets carry a bounded recent input trail, played with a short delay,
so the observer retains the curve between packets. The sender still emits at
most 20 packets per second; this adds samples rather than more packets. Presence remains transient and bounded, and
reduced-motion preferences disable the extra movement and trails.


## Anchored discussion beside the work

Selection trials exposed two weaknesses in the original footer: focus changes
could obscure which text a question referred to, and a queue acknowledgement gave
no way to find the answer while editing. The assistant sidebar now renders the
existing canonical conversation for that item; phones use an overlay so the work
is not compressed into a narrow strip. The composer freezes and displays context
before submission. Browser and host share the capture implementation; the host
checks exact content after pending edits commit, then adds item identity and the
committed revision. Stale captures fail visibly and require explicit refresh.

Document comments are a bounded, host-authored annotation list with Yjs relative
anchors, original quote, attributed human replies, and resolution state. This keeps guest authorship
out of browser-controlled CRDT fields while ordinary concurrent edits move the
anchors. Comments persist with the item but are excluded from content exports.
Posting comments and replies never starts a model turn. A selection toolbar
provides separate comment and assistant entry points. The sidebar has distinct
Comments and Assistant views and independently persisted drafts. Sending a
thread submits its posted human messages and passage through the existing
question queue; the host verifies both captured content and the last reviewed
reply identity. Canonical AI answers render inside that thread, correlated with
the message version sent, rather than being copied into comment storage.
Resolution remains an explicit participant action.
Live anchors describe the current passage; sent snapshots describe the prior
question and never track later edits.
Large snapshots overwhelmed the Emacs and room transcripts while the sidebar
showed only a quote. All three now keep the question visible and put the actual
sent context in a disclosure collapsed by default, identified by item, scope,
and revision. The browser views share one renderer; Emacs reuses its source-backed
input fold. Trusted attribution and an exact authored-question prefix identify
the generated suffix. Host edits and mismatches stay visible. The canonical
prompt and model delivery are unchanged.

Questions use existing external follow-ups and transcript audit attribution.
A stable request identity and content fingerprint find accepted questions in that
queue or transcript after reconnect. Attribution belongs to the data buffer so
asynchronous submissions in different sessions cannot exchange metadata. No
second conversation store or model-request controller is needed. Canonical
provider failure summaries are projected without their private error payloads.

## Dedicated item conversation context

The first sidebar filtered displayed replies while model requests still consumed
room history. The resulting mismatch made document follow-ups depend on unrelated
room work and let item snapshots accumulate in ordinary chat. Each item's trusted
question attribution now selects its model context before reminder delivery:
current reviewed question plus recent same-item turns. Ordinary room requests
and compaction evidence exclude these turns, while the canonical chronology and
operational queue remain shared. Comment questions participate in their item's
conversation; comments remain their presentation subthreads.

Archived canonical segments restore prior item turns after compaction and reload.
Question identities deduplicate preserved tails. Prior history has a complete-turn
character budget with an explicit omission notice; damaged archives needed for
that selection fail requests visibly. The editor shows available live discussion and an archive warning while
content editing remains usable. No placeholder directive is created: its lifecycle
and read-only discussion capability would be wrong for requests that edit an item.

The room routes into item conversations the way it routes into directive
discussions. Separate contexts with one shared display had a trap: a guest
restyling a whiteboard through its comment thread saw the exchange in the room
stream, replied there, and reached the room's model, which had never seen the
whiteboard turns and attached the correction to an unrelated artifact comment.
Records therefore carry their item as they carry a directive, the room filter
lists items beside directives, and a room message with an item selected is a
whole-item question in that item's conversation. Only the routing layer is
shared: each discussion kind keeps its own handler and context policy, so
directive discussions stay read-only and item questions can still edit. A
room message has no reviewed snapshot, so its whole-item question captures the
committed content; editor questions keep the reviewed-snapshot check.

Session artifacts are items in this sense too. Their comments first existed only
as transcript turns in the main conversation, which gave them no people-only
notes, no replies or resolution, and let artifact back-and-forth fill room chat.
An artifact's messages are now item questions about `artifact:NAME` and its
comments live in a per-artifact store beside the shared items. Unlike a
whiteboard or document, whose small structured snapshot travels with each
question, an artifact can be megabytes of HTML: the request names its file and
frames it as untrusted content, and the model reads what it needs.

Read and Grep of `history://root` give scoped conversations deliberate access to
parent decisions, including unsaved turns. `history://saved` covers archived
segments. This extends the existing resource family for directives and all other
session callers; neither an address nor retrieval expands editing authority.
The model chooses retrieval from the task, without an automatic keyword router.

### Decision history

An isolated native-request benchmark exposed unnecessary work in the first
history selector: ordinary room requests classified the transcript twice even
without item attribution, mixed histories searched all user turns for each item
turn, and the character budget was applied only after reading every archive.
With 1,000 turns, request preparation measured 60 ms for ordinary chat and 114 ms
for mixed history; a dense item history across 30 segments took 86 ms and opened
all 30 files. These are local synthetic medians, not network or model latency.

Selection now checks trusted attribution first, matches ordered turn boundaries
in one pass, and applies the complete-turn budget during newest-first archive
reads. Matching fixtures measured 44 ms, 54 ms, and 11 ms respectively; the last
case opened two archives. All 15 native request payload hashes matched the prior
implementation. No persistent index or cache was added. Sparse item histories
can still require all archive reads; retry checks deliberately retain exhaustive
history. Unused older archives no longer block already-truncated context, while
failures in the needed prefix remain visible. The reproducible benchmark and
raw measurements are in `.scratch/shared-conversation-performance/report.md`.

Initially sidebar filtering alone separated item discussions visually. Dedicated
request context replaces that behavior because a focused editor conversation
should not implicitly consume unrelated room and other-item turns. Transcript
storage, shared visibility, and normal editing authority are retained.

A document trial exposed ambiguity when selection always opened comment mode,
posting immediately repurposed the draft as an AI question, and a button named
Send to assistant merely copied text into the composer. Explicit selection
actions, independent drafts, human reply threads, and a send button that submits
replace that workflow. Discussion answers stay at their comment; asking directly
about a passage does not require publishing a comment first.


The first laser refinement interpolated only packet endpoints. It reduced
stepping in timing traces, but a two-browser circle trial exposed visibly
polygonal observer trails: coalescing had discarded the actual curve. Retaining
up to 64 recent input samples per packet replaces that endpoint-only laser
playback. Cursor interpolation remains unchanged. The earlier fixed-delay
experiment had used only sparse endpoints and did not address this data loss.


Originally questions used a whole/selection dropdown in a footer, and a short
per-peer duplicate window. Browser trials showed that the dropdown concealed a
lost selection, the footer lacked replies, and reconnecting lost retry identity.
The frozen context panel, explicit document comments, and queue/transcript retry
lookup replace those choices. Native export remains content-only because session
annotations refer to the original CRDT lineage and conversation.

## Visual feedback after model edits

The first model-assisted board trial required a workaround because nested
ToolCall coordinate lists were encoded as objects, and vector literals were
rejected despite being documented. The interpreter now accepts bounded vector
data, and the editing adapter preserves nested arrays and explicit nulls.

SharedEdit returns the canonical post-edit board PNG through the existing tool
media channel. Board reads and browser questions already used this renderer;
reusing it gives the model immediate visual evidence from the committed revision,
even with all browsers closed. Rendering happens before publication so a render
failure cannot be reported as a failed edit after content was already committed.
A subsequent provider audit found that the old media adapter allowed only
Read/SharedRead names: it dropped images returned by SharedEdit and by the outer
ToolCall identity. Delivery now follows the captured media record and owning
tool-use ID, independent of the tool name. Native continuation tests cover the
actual image blocks in addition to the handler result.

This supplies evidence without adding a second model turn or an automatic
review controller. Documents continue to return structured content.

## Connector borders

A real board exposed bound arrows running through component labels because the
renderer used target centers as visible endpoints. The shared renderer now
places each bound end on the target's nominal silhouette; with Excalidraw
bindings ([ADR 0121](0121-store-whiteboards-as-excalidraw-elements.md)) that
is the outline offset by Excalidraw's binding gap, toward the fixed point. Both endpoints follow target movement and
resizing in the browser and in host snapshots. Hand-drawn wobble remains a
visual decoration rather than changing attachment geometry. Existing boards
benefit without content rewrites; unbound endpoints keep their explicit points.

## Reading shared content as resources

The model reads shared items with Read and Grep at `shared://` addresses. An
item's overview is one `HASH JSON` line per element or block; long strokes,
comments, history, renderings, embedded images and element libraries have
addresses of their own. SharedEdit names each target by the hash it read and
can merge fields with `set` and `unset`. Its result reports the revision and
the stored lines of what changed, not the item.

The resource family reuses Read's paging, output bound and continuation
guidance, Grep's search, and ordinary media delivery, so no size mechanics
are taught to callers. The editing host computes the views from committed
state; the resolver only validates the address and availability, and Read and
Grep fetch the view asynchronously before paging or searching it.

### Decision history: one read tool returning the whole item

`SharedRead` returned an item's full JSON, every retained contribution with
before/after snapshots, all comments and a PNG. Patches carried each target's
exact current JSON as `before`. A user's board of 23 hand-drawn strokes held
183 KB of element JSON; its largest stroke was 21 KB. Every read sent all of
it, every edit returned the whole board again, and changing one stroke's colour
meant the model writing that stroke out twice. SharedRead and SharedEdit had
no result bound, while the generic oversized-result spill would have written
one JSON line that Read cuts at 2,000 characters. Document reads also carried
embedded images as base64 text, up to 12 MiB.

Hash preconditions keep the same concurrent-edit protection, because a hash
over sorted keys changes whenever the content does. They cost the model 12
characters per target. Stroke samples are now also stored at 0.1 units without
near-duplicates, which brought that board to 53 KB.

## Deleting shared content

Items could not be deleted, and session artifacts only from the Emacs
cockpit. Every writable link can now delete both, as can the host. Full links
qualify because they can already edit an item down to nothing; deletion
additionally removes comments and history, which the confirmation states.
Deletion is final: a trash would add a second lifecycle state that listing,
Fork, Save As, Rewind, publication and the model would all have to honor, so
the confirmation offers a downloadable copy for re-import instead. The model
gets no delete action; removing work stays a human decision. Item deletion
runs in the editing queue so it cannot overtake a save in progress, and one
artifact-folder deletion helper commits a project session's tombstone at
once, which also fixed the cockpit's deletions only becoming durable at the
next full save.
