# Shared whiteboard and document editing

The room's **Shared** menu lists named whiteboards and documents. Full and
owner links can create, rename, import, and edit them concurrently. View
links can observe and download. Opening an item affects that browser only;
other participants get a followable entry. Desktop layouts keep chat beside
the editor; narrow layouts have a Close button to return to chat. The host
uses the same editor through a browser share link.

## Runtime and build

Shared editing requires Node 22.4 or newer on the Emacs host. Set
`mevedel-shared-editing-node-program` if it is not on PATH. Node stays local
when the session's execution target is remote; Emacs owns every durable
read and write on that target. Ordinary chat does not need Node.

The minimum includes the switch disabling Node web storage, introduced in
[Node 22.4](https://nodejs.org/en/blog/release/v22.4.0). The helper uses Yjs,
Tiptap's shared schema, and packaged resvg WASM with Noto Sans for headless
PNG rendering. It exposes only a private stdin/stdout connection to Emacs.
End users receive generated bundles, renderer resources, and third-party
notices; they do not run npm installation.

To rebuild after changing browser/helper source or dependency versions:

```sh
npm ci --prefix shared-editing
npm run build --prefix shared-editing
```

Commit the lockfile and generated helper/viewer bundles together. The build
also assembles license notices; font and renderer licenses accompany their
packaged resources. Rebuild the relay to embed changed viewer assets.

## Editing and pointing

The drawing menu follows the selected `af756034` reference: hand, selection,
rectangle, diamond, ellipse, database, sticky note, arrow, line, freehand,
text, and eraser, followed by the laser tool. Buttons expose tool names and
keyboard shortcuts. Selection supports Shift multi-selection, arrow-key
movement, Delete, resizing, and Enter to edit text. Wheel zoom, zoom buttons,
and Fit affect only the local viewport. Images accept PNG, JPEG, and WebP by
upload or clipboard paste. Arrow endpoints can bind to shapes.

Documents support paragraphs, headings, emphasis, lists, links, code blocks,
and basic tables. Named carets show other writers. Each browser's Undo/Redo
uses its own transactions. Agent contributions have readable attribution and
a Revert action; an overlapping later edit makes the inverse fail instead
of restoring an old whole-item snapshot.

Yjs merges concurrent typing and unrelated shape changes. A shape's geometry
is one atomic property, while text and style remain independently editable.
Same-property writes resolve using Yjs's deterministic ordering. Deletion
wins over an in-flight property edit to the deleted record. This guarantees
convergence, not reconciliation of competing human intentions.

Laser gestures transmit board coordinates with sender attribution. Each
receiver renders through its own viewport. Trails fade within 1.5 seconds;
item changes, disconnects, and cancellation clear presence. Presence is
rate-limited and discarded behind queued content/control traffic. It creates
no revision, undo history, export marks, queued question, or model context.

## Working with the assistant

Shared tools are discoverable through ToolSearch/ToolCall. Implementation and
worker roles can read and edit; discussion, explorer, reviewer, and verifier
roles can read.

`SharedRead` lists items or reads one with stable shape/block IDs, a revision,
structured content, and recent attributed transactions. `selection` narrows a
read; `since` filters recent changes and reports when older history is no
longer available. Whiteboard reads include a PNG from that same canonical
snapshot. Models without supported image delivery get structured content and
an explicit omitted-media note.
Selected connectors retain their current bound geometry; endpoint objects
accompany the selection as context.

`SharedCreate` creates a named item. `SharedEdit` applies patches, renames,
or targeted inverses. A patch carries exact `before` values from a read and
new `after` values; null adds or deletes. Documents target top-level blocks
and may specify an `afterId` insertion anchor. Any stale target rejects the
whole transaction with current target data. Unrelated changes do not
invalidate an otherwise valid patch. Tools use normal permissions, Plan and
read-only ceilings, cancellation, and result/media persistence. They work
with every browser closed and never silently start a share.

The editor's explicit question control captures a committed whole item or
selection, including collaboration-aware text anchors. Pending edits must
save first; refusal preserves the question. Questions enter the existing
pending-input queue with guest attribution and board images as normal
attachments. Duplicate questions use the room's duplicate window. Drawing,
typing, and synchronization never start model turns automatically.

## Durability and recovery

Emacs serializes operations per session, validates candidates in the helper,
checks continuing authority, and commits before acknowledging Saved on host.
Project sessions use the existing execution-target lease and immutable
publication transaction. File sessions use the existing session directory.
Canonical files live under `artifacts/shared-editing/`; embedded images travel
with their item's state. Ordinary read-only artifact addresses gain no new
mutation capability.

Resume, Save As, and Fork preserve accepted content and embedded assets.
Forks are independent. Chat Rewind preserves current shared items. Closing a
browser or ending the share leaves the host original intact. A later share
uses fresh credentials, following [ADR 0114](adr/0114-tie-collaboration-room-lifetime-to-host-share.md).

The viewer retains pending changes in browser storage, scoped to room and
item. Reauthentication in the same logical share merges them. Disconnected,
rejected, or storage-failed edits remain visibly pending and have a Recovery
copy download. A recovery copy is explicitly named and represents that
browser's content, including unsynchronized changes. It is not a host save.
Storage quota errors ask the user to download before closing. Retry save
rereads committed state and retries operation identities without duplicating
content. A host publication failure uses the existing publication recovery
commands before retrying browser saves.
After a reload, locally retained items remain available as **local recovery**
entries even when their share or host item is gone. They open read-only for
recovery download until the host supplies valid current content and authority.

## Downloads and imports

Standard downloads use a committed revision: PNG/SVG for whiteboards,
Markdown/HTML for documents, or an editable native JSON file. Native files
preserve supported content, relationships, and embedded assets. They exclude
credentials, presence, collaboration history, and undo stacks. Markdown and
images are viewable conversions with less editable structure.

Import supports the current native format, Markdown/plain text documents,
and raster images on boards. It validates the complete input before creating
a new item and collaboration lineage, preserving the original. Uploaded HTML,
JavaScript, external image URLs, and unsupported formats are rejected. There
are no legacy readers or migrations.

Bounds are explicit: 16 MiB canonical state/native input, 2,000 shapes,
4,000 points per stroke, 200 targets per agent transaction, and 1 MiB document
JSON with bounded depth and node count. Raster images are limited to 16
megapixels each and 32 megapixels per board; browser image uploads are at
most 4 MiB. At most 32 recent transactions are offered for reversion, and the
receipt ledger has a 65,536-operation ceiling. A native export/import begins
a fresh history when that ceiling is reached. Question snapshots have a
128 KiB bound with a visible request to select a smaller portion. Transfers
use bounded chunks; host and viewer queues also bound aggregate input size.

The packaged editor has an opaque iframe origin and an item-scoped message
port. CSP forbids networking and submitted forms while permitting local
editor dialogs. Room credentials stay in the trusted viewer. The relay stays
content-blind. [ADR 0120](adr/0120-edit-shared-content-through-the-session-host.md)
records the change from model-only authorship to direct shared editing.

## Checks

```sh
npm test --prefix shared-editing
npm run test:browser --prefix shared-editing
npx @emacs-eask/cli clean elc
npx @emacs-eask/cli test ert test/test-mevedel-shared-editing*.el test/test-mevedel-tool-editing.el test/test-mevedel-collaboration-editing.el
MEVEDEL_TEST_SHARED_EDITING=1 timeout 600s ./test/run-remote-acceptance.sh
```

The browser scenario uses a real relay and isolated Emacs, multiple Chromium
contexts, and a deterministic participant invoking actual native tools. It
requires Go, Emacs dependencies installed by Eask, and Playwright Chromium
(`shared-editing/node_modules/.bin/playwright install chromium`). The remote
command provisions the repository's temporary SSH/container fixture.
