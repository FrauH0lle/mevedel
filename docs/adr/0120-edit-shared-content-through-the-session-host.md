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

Geometry is one atomic shape property; independent text/style properties
remain separate. Same-property concurrent writes follow Yjs's deterministic
ordering, and deletion wins over an in-flight property edit to that record.
The first drag implementation only redrew after release. Move and resize now
render temporary geometry through the same scene and connector renderer, then
commit that geometry on release. Cancellation drops the preview without a CRDT
write; pointer motion does not create a stream of saved revisions.
New empty documents share an initial text object: real browser testing found
that two separately created empty text objects could normalize into one while
losing a writer's undo history. Agent edits and inverses compare their exact
targets and reject the entire transaction on overlap.

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

The cost is a local Node runtime and packaged browser/helper bundles. End
users do not install npm dependencies. Large content and operation histories
have explicit bounds; reaching them calls for a smaller item or a native
export/import, rather than unbounded memory or hidden history truncation.


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
editor or alter content, presence, or exported colors. The whiteboard keeps its
light drawing substrate so authored stroke and image colors remain faithful.
The document uses the selected theme. A real parent/iframe phone test caught
the taller header consuming typing space with the keyboard open; compact
short-viewport spacing retains at least 150px of document viewport in that test.

The trial also showed that a contribution row per 300 ms save was unreadable.
Keep those commits and their exact inverse records; group their presentation
by participant name and five-second idle gaps. Agent transactions remain
separate. Local view decorations distinguish the latest retained agent changes
without modifying CRDT content or exported formatting.


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
character budget with an explicit omission notice; damaged archives fail requests
visibly. The editor shows available live discussion and an archive warning while
content editing remains usable. No placeholder directive is created: its lifecycle
and read-only discussion capability would be wrong for requests that edit an item.

Read and Grep of `history://root` give scoped conversations deliberate access to
parent decisions, including unsaved turns. `history://saved` covers archived
segments. This extends the existing resource family for directives and all other
session callers; neither an address nor retrieval expands editing authority.
The model chooses retrieval from the task, without an automatic keyword router.

### Decision history

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
media channel. SharedRead and browser questions already used this renderer;
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
renderer used target centers as visible endpoints. Bindings still store target
IDs, but the shared renderer now intersects the center-to-center direction with
each target's nominal silhouette. Both endpoints follow target movement and
resizing in the browser and in host snapshots. Hand-drawn wobble remains a
visual decoration rather than changing attachment geometry. Existing boards
benefit without content rewrites; unbound endpoints keep their explicit points.
