# Shared whiteboard and document editing

The room's **Shared work** section separates creation and import controls from its list
of named whiteboards and documents. Full and
owner links can create, rename, import, and edit them concurrently. View
links can observe and download. Opening an item affects that browser only;
other participants get a followable entry. Each editor opens in its own
browser tab, leaving the room and its composer draft in the original tab.
The editor tab reconnects independently and reopens its item on reload.
**Back to room** shows the session in that tab. The host uses the same editor through
a browser share link. Browser popup permission is needed to open a new tab.
An editor tab shows its loading state immediately. If the item cannot be read,
it keeps the requested item and displays an error with Retry; it does not fall
back silently to the room. Back to room cancels a pending editor opening.

Documents use a continuous paper surface, serif body text, and grouped **Text**,
**Lists**, and **Insert** menus. Room and editor controls share cool and warm
palettes and system/light/dark appearance; changes follow the editor without
replacing its document or question draft. The whiteboard keeps a light drawing
surface so authored colors remain unchanged.

**Discussion** (documents) or **Assistant** (whiteboards) opens a sidebar on
desktop and a full-width overlay on phones. Document discussion starts open on
wide screens. Drag its divider to widen it, up to half the viewport; Left/Right
arrow keys also resize it, Home/End select the limits, and double-click resets
its width. The width is retained with the item's private drafts. Closing the
panel preserves drafts, captured context, and editor position. The editor
follows the visible viewport above the keyboard; Escape closes the panel, and
Ctrl/Command+Enter submits its composer.
Downloads, recovery, retry, contribution history, and assistant highlighting
are in the editor's **☰** menu.

## Runtime and build

Shared editing requires Node 22.4 or newer on the Emacs host. Set
`mevedel-shared-editing-node-program` if it is not on PATH. Node stays local
when the session's execution target is remote; Emacs owns every durable
read and write on that target. Ordinary chat, static artifacts, and browser
viewing do not need Node; browser guests never install it.

Shared editing is optional. On connection the browser loads the saved item
catalog independently, then asks the host to check the configured runtime,
document schema, and packaged PNG renderer. This check creates no saved session
or item. If startup or resource loading fails, creation, import, and opening
items are disabled with an explanation. Existing items stay listed; open editors
and locally stored recovery copies remain accessible.

After installing Node, changing `mevedel-shared-editing-node-program`, or repairing
helper resources, use **Recheck availability** in Shared work. The host
starts a fresh helper between queued operations, so repaired resources take
effect without cancelling an edit or discarding drafts. A successful check
reenables controls and reconnects an open editor to host state. Read-only links
can check availability and view items but still cannot edit them.

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
movement, Delete, and resizing.
Moving and resizing show a preview while the pointer is held, including
selection handles and bound connectors. Geometry previews also travel over the
existing presence channel, so other participants see movement before the save
round trip. Their status reads **Live movement · not saved yet**. Releasing
commits the geometry through the normal validated save path. Previews never
enter content, exports, recovery drafts, or undo history; cancellation restores
the committed scene without creating an edit. Commit, cancellation, disconnect,
and a five-second expiry remove remote previews. A preview packet contains only
up to 100 object IDs and bounding boxes, with the pending operation ID after
release. New shapes, text, style, and keyboard edits use normal committed
updates.
Click inside an unfilled shape to select it; lines have a wider invisible hit
area. Double-click a shape or press Enter
to edit its label in place, with the same alignment, size, and line spacing.
Finishing a drawing returns to selection, so a double-click edits the new shape
instead of creating more shapes. Text is shared while typing; blur or Ctrl/Command+Enter
finishes, and Escape cancels if another writer has not changed that text.
The **Style** panel follows the same reference: stroke and background
colours, hachure/cross/solid fill, stroke width, solid/dashed/dotted strokes,
sloppiness (architect, artist, cartoonist), sharp or round rectangle edges,
font size, and opacity. It shows only the
sections that apply to the selection or the active drawing tool, changes the
selected objects, and remembers the choices for new ones. Sloppy outlines are
seeded from the shape id, so every browser and the host's PNG draw the same
wobble.
**Objects** exposes Select all, Clear selection, Edit text, Duplicate, Delete,
and **Arrange & move** for layer order and precise nudges. **Shortcuts** lists
the canvas keys. Selection handles retain their screen size as the view changes.
Wheel zoom, zoom buttons, Fit, and the percentage button (reset to 100%) affect
only the local viewport. The board fits existing objects once on opening.
Both whiteboards and documents accept PNG, JPEG, and WebP through the board's
image button or **Insert → Image…**, clipboard paste, or file drag-and-drop. Dropped images land at the board pointer or document insertion
position; multiple files insert together and support Undo/Redo. Images are
embedded in the item and retained in native exports; document HTML and Markdown
exports also carry their image data. External image URLs are not fetched.
Selecting an image exposes **Crop image**, **Rotate 90°**, **Flip horizontal**,
**Flip vertical**, and **Reset image** in both editors. Crop opens a shared
dialog with draggable edges, arrow-key adjustments (Shift for ten pixels),
percentage fields, and a live result preview. **Reset crop** restores the
full source while retaining orientation; **Reset image** clears both. Apply
commits one undoable edit; Cancel leaves the image unchanged. A removed or
changed image is reported instead of overwritten when applying a stale dialog.

The original stays in `src`; optional `imageEdit` holds the normalized crop,
quarter-turn rotation, flips, and rendered PNG together as one attributed
shared property. Both source and rendered data count toward existing image
and item limits. Native exports retain both so reset remains possible.
PNG/SVG board exports and HTML/Markdown document exports use the rendered
image, so other viewers show the same crop and orientation without special
CSS or hidden original pixels. Reset and transformations follow local Undo/Redo
and arrive at other participants through the existing editor synchronization.
Arrow endpoints can bind to shapes. Bound arrows meet the facing shape borders
and follow moves and resizes; ellipses, diamonds, rounded corners, and database
rims use their silhouettes. The editor and PNG/SVG exports share this geometry.

Documents support paragraphs, headings, emphasis, lists, links, code blocks,
and tables. Headings 1–6, inline code, code blocks, quotes, list indentation,
clear formatting, dividers and line breaks are exposed in the grouped menus.
Selecting a table reveals row, column and cell menus for insertion, deletion,
header toggles, merging and splitting. Column borders resize by dragging.
Links have edit/remove controls. Images have alt text, title, dimensions,
optional proportions, drag handles and deletion; dimension changes undo and
collaborate like other content edits. Named carets show other writers. Each browser's Undo/Redo
uses its own transactions. Agent contributions have readable attribution and
a Revert action; an overlapping later edit makes the inverse fail instead
of restoring an old whole-item snapshot. The Contributions display groups
consecutive saves from the same participant name until another participant
edits or there is a five-second idle gap. The editor checks for unsent changes
every 300 ms. While a save awaits acknowledgement, further edits accumulate
into one recoverable follow-up, sent as soon as that save succeeds. It keeps
the in-flight operation unchanged for safe retries, so slower hosts do not
build a queue of obsolete intermediate text states. Contribution grouping
does not delay durability or merge already committed revision records. Agent
transactions remain individually revertible. History retains at most 32
transactions and targets a 4 MiB snapshot budget, expiring oldest entries first.
The newest transaction stays revertible even if it alone exceeds that history
budget, subject to the overall item limit. The Contributions display identifies
the earliest retained revision, so a displayed burst may cover only its recent part.

A pale highlight identifies newly received assistant edits for eight seconds,
fading during the final two seconds. Each changed shape or document block has
its own expiry; a human edit clears its highlight immediately. Repeated syncs
do not restart the timer, and opening an item does not highlight its older
contributions. Reduced motion disables the fade, retaining the same expiry.
**Highlight recent assistant edits** toggles this display. Attribution and
Revert remain available in Contributions after the highlight expires;
highlights do not become content, formatting, or export marks.

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
Remote cursors use a short transition from the displayed position to each
received sample, without predicting past it. Laser packets retain up to 64
input samples from the last 550 ms, rather than only each packet's last position.
The observer plays those samples with a short 55 ms delay, preserving curves
between network updates. A stale gap starts a new pointer
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
or targeted inverses. Whiteboard edits also return a PNG of the resulting
canonical revision through the normal tool-media path, so the model can inspect
the visual result without a separate read or a connected browser. A patch carries exact `before` values from a read and
new `after` values; null adds or deletes. Documents target top-level blocks
and may specify an `afterId` insertion anchor. Any stale target rejects the
whole transaction with current target data. Unrelated changes do not
invalidate an otherwise valid patch. Tools use normal permissions, Plan and
read-only ceilings, cancellation, and result/media persistence. They work
with every browser closed and never silently start a share.

### Questions and document comments

The document's **Discussion** panel has **Comments** and **Assistant** views.
Whiteboards keep their **Assistant** panel. The assistant shows questions about
that item and their canonical replies, with the session's queued, paused,
working, disconnected, or provider-failure state.
While the session is working, a spinner accompanies the status and the panel's
toolbar button. The room chat shows **Assistant working…** in its status strip.
These indicators follow the host's session activity, including tool execution,
and clear on completion or disconnection. Reduced-motion preferences disable
the spinning animation. A paused follow-up queue does not conceal a running turn.
All submitted questions, comments, replies, and assistant answers are shared with the room; drafts stay
in the current browser's recovery storage. View participants can read them.

Selecting document text shows **Add comment** and **Ask about selection** beside
the passage. **Add comment** (Ctrl+Alt+M) opens a separate draft; **Post comment**
publishes an anchored discussion. **Post reply** adds an attributed human reply.
Neither action submits a model turn. Questions and comment/reply drafts remain
independent when switching views, receiving updates, or reopening the editor.
An unrecognized saved discussion-draft format is reported and reset without
blocking recovery of document content or posted discussions.

**Ask about selection** opens the assistant composer directly with the passage
attached. **Use whole document** explicitly changes its scope. The attached quote
stays fixed when focus or the document selection moves elsewhere.

**Send to assistant** inside a comment actually submits the posted human thread
and its passage, without changing the assistant composer or including unposted
reply text. Queue and delivery status appear alongside the discussion. Canonical
AI replies render in that thread beneath the human message that prompted them;
ordinary questions remain in the Assistant view. Another AI request requires
another explicit send. The host checks the reviewed thread version as well as
the passage; concurrent replies require reviewing the updated discussion before
resubmission. Failed delivery retains its request identity for retry.

Clicking a document highlight opens its discussion; **Show passage** scrolls back
to the live anchor. Participants with edit authority can resolve or reopen a
thread. Resolution hides its highlight and removes it from the open list;
**Show resolved** reveals its retained discussion. AI responses never resolve
threads automatically. Reopen a resolved thread before posting or asking again.
There are at most 200 comments per document and 200 human replies per comment,
with at most 10,000 characters per message and the normal snapshot size limit
on submissions to the assistant.

Comments keep their original quote and collaboration-aware text anchors. The
panel marks a passage changed or removed after later edits. Changed passages
require reviewing current context before sending; removed passages cannot be
sent. The immutable snapshot of an already sent question remains unchanged.
In the editor sidebar, room transcript, and Emacs view, sent questions keep their
authored text visible and collapse the snapshot under **Shared context** by
default. Expand it to inspect the full sent context and attachment links; the
summary names the item, scope, and revision. This affects display only, not what
the model receives. Prompts edited on the host remain fully visible.
Comments are session annotations: native and document exports contain the content,
not the comments or conversation. A fresh import starts without those annotations.

For a whiteboard, select one or more objects and choose **Ask about selection**.
The discussion composer offers **Selected objects** / **Whole whiteboard** or
**Selected passage** / **Whole document**. Switching to the whole item retains
the previously attached selection, so switching back does not require selecting
it again. A private document highlight keeps the attached passage visible while
focus is in the discussion or comment draft; closing discussion removes that
highlight. The attached quote shows the chosen scope before submission. Opening controls, typing, and toggling the panel retain that capture.
Pending edits must save first. The host compares the captured content with its
committed content; a concurrent change rejects the request with an explicit
**Refresh context** recovery. It never substitutes a whole item for a missing
selection. Large contexts must be narrowed to fit the 128 KiB snapshot limit.

Each document and whiteboard has its own model conversation. A question receives
its reviewed content snapshot and recent turns about the same item, including
questions sent from its comment threads. Other items and ordinary room chat are
excluded. Ordinary room requests and room compaction summaries likewise exclude
item discussion turns. The room transcript still shows their shared chronology;
this is context selection, not a privacy boundary or a separate execution agent.
Questions keep the session's permissions, tools, queue, and checkpoints, so they
can edit content. Directive discussions retain their separate read-only contract.

When broader context matters, the assistant can search `history://root` with
Grep and read the matching line range with Read. This includes unsaved room
turns; `history://saved` covers archived segments. The prompt advertises those
resources, and the model chooses whether to retrieve them from the request's
meaning. No keyword trigger or automatic full-room attachment runs. For example,
“Make this consistent with our earlier architecture decision” can prompt a
search followed by reading the relevant exchange. An unavailable decision must
be retrieved or clarified, not assumed to have been included.

Canonical transcript segments remain the only conversation store. On subsequent
requests, same-item turns are restored from archived segments and deduplicated by
question identity against the live tail. The request includes up to 128,000
characters of prior complete turns, newest first in selection and chronological
in presentation; omitted older history is explicitly disclosed and remains
retrievable. The budget is applied while reading history: once the next unique
item turn exceeds it, older segments are not opened. The current question and
its archived duplicates do not consume that budget. Sparse item history can
still require scanning all segments before reaching the budget. Retry identity
checks remain exhaustive so old accepted questions cannot be submitted twice.
The current question is retained separately. Item requests do not
compact the unrelated room to make space. Current snapshots, tools, and provider
context limits still apply; a large single turn may need a narrower question.
The editor restores recent archived discussion on opening, reconnecting, or
transcript replacement/removal, and reports truncated or unavailable history.
Failure to read a needed archive prevents a model request from silently losing
context, but does not prevent editing the item. Archives older than an already
established truncation point are not consulted. Ordinary requests without
trusted item attribution skip item-related transcript classification.

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
Whiteboard PNG and SVG downloads use an opaque white background independent
of the room theme, without the editor grid or temporary highlights. SVG labels
use individually positioned text lines, preserving blank lines, and arrowheads
use explicit paths so Qt-based viewers retain the same text layout and arrows.

Import supports the current native format, Markdown/plain text documents,
and raster images on boards. It validates the complete input before creating
a new item and collaboration lineage, preserving the original. Uploaded HTML,
JavaScript, external image URLs, and unsupported formats are rejected. There
are no legacy readers or migrations.

Bounds are explicit: 16 MiB canonical state/native input, 2,000 shapes,
4,000 points per stroke, 200 targets per agent transaction, and 1 MiB document
JSON excluding embedded image data, with bounded depth and node count. Documents
allow up to 12 MiB of embedded image data within the canonical state limit.
Raster images are limited to 16 megapixels each and 32 megapixels per item; browser image uploads are at
most 4 MiB. Recent revert snapshots are bounded by both count and bytes as
described above; they expire before older snapshots block further saves. The
receipt ledger has a 65,536-operation ceiling. A native export/import begins
a fresh history when that ceiling is reached. Question snapshots have a
128 KiB bound with a visible request to select a smaller portion. Transfers
use bounded chunks; host and viewer queues also bound aggregate input size.
Browser responses carry CRDT state or its update and compact contribution
metadata. Full before/after snapshots stay on the host for Revert; image bytes
are not repeated through the contribution list on every broadcast.

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
The focused room design and navigation checks also run with
`MEVEDEL_BROWSER=firefox` after installing Playwright Firefox with
`shared-editing/node_modules/.bin/playwright install firefox`.

The room browser scenario checks the separate tab and preserved composer,
as well as caret visibility after a mobile viewport resize. It uses a real
relay and isolated Emacs, multiple Chromium contexts, and a deterministic participant invoking actual native tools. It
requires Go, Emacs dependencies installed by Eask, and Playwright Chromium
(`shared-editing/node_modules/.bin/playwright install chromium`). The remote
command provisions the repository's temporary SSH/container fixture.
