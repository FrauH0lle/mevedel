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
New empty documents share an initial text object: real browser testing found
that two separately created empty text objects could normalize into one while
losing a writer's undo history. Agent edits and inverses compare their exact
targets and reject the entire transaction on overlap.

Presence is bounded, disposable traffic. Explicit questions capture committed
content and enter the ordinary pending-input queue; synchronization starts no
model turns. Native imports validate completely and begin a fresh lineage.
There is one current schema, no executable import and no compatibility layer.

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
Remote pointers transition from their displayed position to received samples.
A fixed playback-delay experiment reduced normal stepping but still jumped on
jittered delivery; transitioning from the displayed position absorbs that gap
without extrapolating after a stop. Presence remains transient and bounded, and
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
anchors, original quote, author, and resolution state. This keeps guest authorship
out of browser-controlled CRDT fields while ordinary concurrent edits move the
anchors. Comments persist with the item but are excluded from content exports.
Posting a comment and sending it to the assistant are distinct explicit actions.
Live anchors describe the current passage; sent snapshots describe the prior
question and never track later edits.

Questions use existing external follow-ups and transcript audit attribution.
A stable request identity and content fingerprint find accepted questions in that
queue or transcript after reconnect. Attribution belongs to the data buffer so
asynchronous submissions in different sessions cannot exchange metadata. No
second conversation store or model-request controller is needed. Canonical
provider failure summaries are projected without their private error payloads.

### Decision history

Originally questions used a whole/selection dropdown in a footer, and a short
per-peer duplicate window. Browser trials showed that the dropdown concealed a
lost selection, the footer lacked replies, and reconnecting lost retry identity.
The frozen context panel, explicit document comments, and queue/transcript retry
lookup replace those choices. Native export remains content-only because session
annotations refer to the original CRDT lineage and conversation.
