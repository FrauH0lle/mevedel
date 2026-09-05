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

Full and owner bearers may create, import, rename, edit, ask, and point through
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
formatting stays on one scrolling row and phone question controls collapse.
A tab URL identifies the item, while credentials continue through the existing
fragment-to-tab-storage lifecycle and never reach the iframe.

The trial also showed that a contribution row per 300 ms save was unreadable.
Keep those commits and their exact inverse records; group their presentation
by participant name and five-second idle gaps. Agent transactions remain
separate. Local view decorations distinguish the latest retained agent changes
without modifying CRDT content or exported formatting.
