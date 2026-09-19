# Shared whiteboard and document editing

The room's **Shared** menu separates creation and import controls from its list
of named whiteboards and documents. Full and
owner links can create, rename, import, and edit them concurrently. View
links can observe and download. Opening an item affects that browser only;
other participants get a followable entry. Each editor opens in its own
browser tab, leaving the room and its composer draft in the original tab.
The editor tab reconnects independently and reopens its item on reload.
**Room** shows the session in that tab. The host uses the same editor through
a browser share link. Browser popup permission is needed to open a new tab.

Documents use a continuous paper surface and a compact, horizontally scrolling
formatting bar. **Assistant** opens a conversation sidebar on desktop and a
full-width overlay on phones. Closing it preserves the private draft, captured
context, and editor position. The editor follows the visible viewport above the
keyboard; Escape closes the panel, and Ctrl/Command+Enter submits its composer.
Downloads, recovery, retry, contribution history, and assistant highlighting
are in the editor's **☰** menu.

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
movement, Delete, and resizing. Click inside an unfilled shape to select it;
lines have a wider invisible hit area. Double-click a shape or press Enter
to type directly in it. Text is shared while typing; blur or Ctrl/Command+Enter
finishes, and Escape cancels if another writer has not changed that text.
The **Style** panel follows the same reference: stroke and background
colours, hachure/cross/solid fill, stroke width, solid/dashed/dotted strokes,
sloppiness (architect, artist, cartoonist), sharp or round rectangle edges,
font size, opacity, layer order, duplicate, and delete. It shows only the
sections that apply to the selection or the active drawing tool, changes the
selected objects, and remembers the choices for new ones. Sloppy outlines are
seeded from the shape id, so every browser and the host's PNG draw the same
wobble.
Wheel zoom, zoom buttons, and Fit affect only the local viewport. Images accept PNG, JPEG, and WebP by
upload or clipboard paste. Arrow endpoints can bind to shapes.

Documents support paragraphs, headings, emphasis, lists, links, code blocks,
and basic tables. Named carets show other writers. Each browser's Undo/Redo
uses its own transactions. Agent contributions have readable attribution and
a Revert action; an overlapping later edit makes the inverse fail instead
of restoring an old whole-item snapshot. The Contributions display groups
consecutive saves from the same participant name until another participant
edits or there is a five-second idle gap. Saves still run every 300 ms; grouping
does not delay durability or merge the underlying revision records. Agent
transactions remain individually revertible. Only the latest 32 transactions
are available, so a displayed burst may cover just the retained part.

A pale highlight identifies shapes or document blocks whose latest retained
contribution came from an agent. Human changes replace that attribution.
**Highlight assistant edits** toggles this display; highlights do not become
content, formatting, or export marks.

Yjs merges concurrent typing and unrelated shape changes. A shape's geometry
is one atomic property, while text and style remain independently editable.
Same-property writes resolve using Yjs's deterministic ordering. Deletion
wins over an in-flight property edit to the deleted record. This guarantees
convergence, not reconciliation of competing human intentions.

Ordinary board cursors move one named arrow per participant. Selecting **Laser
pointer** makes mouse or pen hover visible locally and to collaborators; on a
touchscreen, press and drag to point, then lift to stop. A bright tip, a compact
name tag, and a short curved trail distinguish deliberate pointing from an
ordinary cursor. Older trail segments fade independently within about half a
second; a stationary tip stays visible while the participant points.

Coordinates travel in board space and render through each receiver's viewport.
Remote movement uses a short transition from the displayed position to each
received sample, without predicting past it. A stale gap starts a new pointer
instead of drawing a bridge across the board. Reduced-motion preferences remove
trails and interpolation. Leaving the canvas, switching tools, cancellation,
hiding the page, and disconnect clear the local pointing state; item changes
and peer departure clear remote state. Idle presence expires if its occasional
refresh is lost. Motion coalesces to at most 20 updates per second and retains
the final position. Presence is discarded behind queued content/control traffic;
it creates no revision, undo history, export marks, queued question, or model
context.

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

### Questions and document comments

**Assistant** shows questions about the current item and their canonical replies,
with the session's queued, paused, working, disconnected, or provider-failure
state. Follow-ups use the same session conversation. Other item conversations
remain in the room transcript. View participants can read comments and replies.

For a document selection, choose **Comment on selection** (Ctrl+Alt+M), enter a
question or instruction, and **Post comment**. The draft is private to this
browser's recovery storage until posted. Posted comments are shared with everyone
in the session and persist with the item. Posting never submits a model turn.
Choose **Send to assistant** explicitly to send the comment's passage; **View
conversation** finds its question and answer in the panel. Clicking a passage
scrolls to its live anchor. Participants with edit authority can resolve or reopen
comments. Resolved comments remain readable but no longer highlight the document.
There are at most 200 comments per document.

Comments keep their original quote and collaboration-aware text anchors. The
panel marks a passage changed or removed after later edits. Changed passages
require reviewing current context before sending; removed passages cannot be
sent. The immutable snapshot of an already sent question remains unchanged.
Comments are session annotations: native and document exports contain the content,
not the comments or conversation. A fresh import starts without those annotations.

For a whiteboard, select one or more objects and choose **Ask about selection**.
**Ask about whole item** keeps whole-document and whole-board questions available.
The attached quote and expandable full context show the chosen scope before
submission. Opening controls, typing, and toggling the panel retain that capture.
Pending edits must save first. The host compares the captured content with its
committed content; a concurrent change rejects the request with an explicit
**Refresh context** recovery. It never substitutes a whole item for a missing
selection. Large contexts must be narrowed to fit the 128 KiB snapshot limit.

Accepted questions include item identity, title, committed revision, exact selected
text or shapes, bounded surrounding document blocks or connector endpoints, and
a matching board PNG as a normal attachment. Questions use the ordinary queue
with guest attribution; correlation metadata is persisted as model-invisible
transcript audit data. Provider failures appear in the conversation. If the host edits a queued question,
the panel explicitly marks it **Edited on host** and shows the revised input; it
no longer claims that the original attachment is what the model received.
Disconnected or rejected submissions retain the draft and request identity.
Retrying an accepted question finds its queued or delivered record across browser
reconnects, preventing a second turn. Editing the question or refreshing context
creates a new identity; an intentional follow-up is a new question. Drawing,
typing, posting comments, and synchronization never start model turns automatically.

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

The focused editor checks reproduce interior selection, inline text, cursor
replacement, local/remote laser trails, grouped contributions, export-neutral
assistant highlights, frozen context, stale/offline refusal, explicit comments,
reply rendering, disclosure continuity, and a small keyboard-sized viewport.
The room scenario also checks question-draft recovery across repeated reloads. Set
`MEVEDEL_EDITOR_SCREENSHOTS=1` when running `test/editor.browser.mjs` to save
preview screenshots under `.scratch/shared-collaborative-editing/`.

The room browser scenario checks the separate tab and preserved composer,
as well as caret visibility after a mobile viewport resize. It uses a real
relay and isolated Emacs, multiple Chromium contexts, and a deterministic participant invoking actual native tools. It
requires Go, Emacs dependencies installed by Eask, and Playwright Chromium
(`shared-editing/node_modules/.bin/playwright install chromium`). The remote
command provisions the repository's temporary SSH/container fixture.
