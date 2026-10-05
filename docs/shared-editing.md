# Shared whiteboard and document editing

The room's **Shared work** section separates creation and import controls from its list
of named whiteboards and documents. Full and
owner links can create, rename, import, edit and delete them concurrently. View
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
desktop and a full-width overlay on phones; both have **Comments** and
**Assistant** views. Document discussion starts open on
wide screens. Drag its divider to widen it, up to half the viewport; Left/Right
arrow keys also resize it, Home/End select the limits, and double-click resets
its width. The width is retained with the item's private drafts. Closing the
panel preserves drafts, captured context, and editor position. The editor
follows the visible viewport above the keyboard; Escape closes the panel, and
Ctrl/Command+Enter submits its composer.
Downloads, recovery, retry, contribution history, and assistant highlighting
are in the editor's **☰** menu.

A whiteboard's **Canvas background** in that menu offers the theme's board
colour, Excalidraw's white, grey, blue, yellow and rose canvases, and a custom
opaque colour; the model sets it with `SharedEdit`'s `background` action. The
colour belongs to the board: every participant sees it,
it is an attributed, revertible contribution and follows local Undo/Redo, and
PNG, SVG and `.excalidraw` exports and the model's board images carry it as
Excalidraw's `viewBackgroundColor`. Importing an Excalidraw file keeps its
canvas colour. Without a choice the board follows the room theme and exports
stay white. Outlined arrowheads are filled with the canvas colour.

A dark theme shows the canvas, its grid and its elements as Excalidraw does,
through `invert(93%) hue-rotate(180deg)`, which keeps hues; images are
inverted back to their own pixels. The swatches preview the colours as the
board will show them. Stored colours, exports, and the model's board images
stay as authored.

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
Tiptap's shared schema, roughjs and perfect-freehand, and packaged resvg WASM
with Excalifont, Nunito, Comic Shanns and Noto Sans for headless PNG rendering. It exposes only a private stdin/stdout connection to Emacs.
End users receive generated bundles, renderer resources, and third-party
notices; they do not run npm installation.

To rebuild after changing browser/helper source or dependency versions:

```sh
npm ci --prefix shared-editing
npm run build --prefix shared-editing
```

The board fonts are generated from Excalidraw's font subsets with
`python3 shared-editing/fonts.py EXCALIDRAW_CHECKOUT` (fontTools and brotli).
It writes the TTFs the host renders with, the WOFF2 files embedded in the
editor bundle, and `font-metrics.json`, the advance widths every client uses
to measure and wrap text. Rebuild the bundles afterwards.

Commit the lockfile and generated helper/viewer bundles together. The build
also assembles license notices; font and renderer licenses accompany their
packaged resources. Rebuild the relay to embed changed viewer assets.

## Editing and pointing

A whiteboard holds Excalidraw elements and draws them as Excalidraw does
([ADR 0121](adr/0121-store-whiteboards-as-excalidraw-elements.md)): roughjs
strokes and fills from each element's seed, perfect-freehand drawing, and
Excalidraw's fonts and text layout. Every browser, the host's PNGs and the
exports draw the same wobble.

The drawing menu follows Excalidraw: hand (H), selection (V), rectangle (R),
diamond (D), ellipse (O), sticky note (N), arrow (A), line (L), freehand
drawing (P or X), **Shape from drawing** (Shift+X), text (T) and eraser (E),
followed by the laser (K), comment (M) and image (9) tools. Number keys 1–8
and 0 select tools as in Excalidraw. Buttons expose tool names and keyboard
shortcuts. Selection supports Shift multi-selection, arrow-key
movement, Delete, and resizing. Dragging across empty canvas with the
selection tool, including by touch, draws a selection box: releasing selects
the objects it fully contains, Alt selects every object it touches, and Shift
adds them to the current selection. Connectors are tested by their drawn path.
A press without movement clears the selection. The box disappears on release,
but its area stays attached to the selection, even when it contains no
objects, until the selection changes or an object moves: **Ask about
selection** and **Add comment** use it, and the area shows again while a
question or comment draft carries it. Box selection never edits content.

Selection follows Excalidraw's handles. A lone element shows an outline with
eight resize handles and a rotation handle above it; a group or several
elements share one frame with the same handles, and each group is outlined
once. Corner handles keep the opposite corner in place and side handles the
opposite side, in the element's rotated frame; Shift keeps the proportions,
which images and text keep by default. Dragging a text corner scales its font;
dragging its side wraps it to that width. Several elements scale from their
common frame. The rotation handle turns the selection about its centre,
snapping to 15° with Shift. A lone arrow or line shows its points instead:
dragging a point moves it, dragging a segment's midpoint adds a bend there,
and dropping an arrow end on a shape binds it. Double-clicking a group enters
it, so its members select and edit one by one; Escape or clicking outside
leaves it.
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
Click inside a shape or a closed line to select it, filled or not; lines have
a wider invisible hit area. Double-click a shape or press Enter to edit its label in place, with
the label's font, size, alignment and line height; double-clicking empty canvas
starts free text there. A label is an Excalidraw text element bound to its
container: it wraps to the container, centres in it, moves with it, and grows
the container when it no longer fits. Arrows carry labels the same way, with a
gap cut into the line behind them. Clicking a label selects its container.
Finishing a drawing returns to selection, so a double-click edits the new shape
instead of creating more shapes. Text is shared while typing; blur or Ctrl/Command+Enter
finishes, and Escape cancels if another writer has not changed that text.
Finishing with no text removes the label or text element.

**Shape from drawing** turns a stroke drawn by hand into a clean rectangle,
diamond, ellipse, arrow or line when it matches one, using Excalidraw's
recognizer; other strokes stay freehand drawings. A finished stroke waits
0.7 seconds for another, and strokes drawn within that pause are recognized
together, so an arrow's head may be drawn after its shaft. An unrecognized
group keeps each stroke as its own drawing. A recognized arrow binds its start
to the shape the first stroke began on and its end to the shape under its tip,
wherever the pen lifted. The result takes the current
style and is an ordinary edit, so Undo restores the board as it was before the
strokes. Strokes smaller than 25 screen pixels are not converted.

The **Style** panel follows Excalidraw: stroke and background colours,
hachure/cross-hatch/solid fill, stroke width, solid/dashed/dotted strokes,
sloppiness (architect, artist, cartoonist), sharp or round edges, the arrow
type (sharp, curved or elbow), start and end arrowheads (none, arrow,
triangle, circle, bar), font (hand-drawn Excalifont, normal Nunito, code Comic
Shanns), font size, text alignment, vertical alignment for a label inside a
shape, and opacity. It shows only the sections that apply to the selection or
the active drawing tool, changes the selected objects and their labels, and
remembers the choices for new ones. On screens at least 1,100 pixels wide it
opens by itself when something is selected and closes when the selection is
cleared; closing it keeps it closed until then.
A curved arrow runs through its points; switching a straight arrow to curved
adds a bend to drag. An elbow arrow leaves and enters its bound shapes at the
facing side and runs in horizontal and vertical segments between them,
rerouted as the shapes move. The route does not avoid other shapes.
Freehand drawing records a pen's pressure for every sample the browser
reports; mouse and touch strokes simulate pressure from their speed. Elements imported with other Excalidraw values, such as
the cardinality arrowheads, keep and draw them.
**Objects** exposes Select all, Clear selection, Edit text, Duplicate
(Ctrl/Command+D), Group (Ctrl/Command+G), Ungroup (Ctrl/Command+Shift+G), Lock,
Unlock all, Delete, and **Arrange & move** for layer order and precise nudges.
Clicking any member of a group selects the whole group. A locked element
cannot be selected, moved or erased until **Unlock all**. Duplicates and
copies keep the bindings and labels among themselves and drop the others.
**Shortcuts** lists the canvas keys. Selection handles retain their screen
size as the view changes.
Scrolling pans the board, and Shift turns a mouse wheel sideways;
Ctrl/Command+scroll and a trackpad pinch zoom at the pointer, in proportion to
the scroll. Panning, zooming, the zoom buttons, Fit, and the percentage button
(reset to 100%) affect only the local viewport. The board fits existing objects once on opening.
Both whiteboards and documents accept PNG, JPEG, and WebP through the board's
image button or **Insert → Image…**, clipboard paste, or file drag-and-drop. Dropped images land at the board pointer or document insertion
position; multiple files insert together and support Undo/Redo. Images are
embedded in the item and retained in native exports; document HTML and Markdown
exports also carry their image data. External image URLs are not fetched.
A board stores each image once as an Excalidraw file named by its content
hash; image elements refer to it, so repeated images and contribution history
never repeat its bytes.
Selecting an image exposes **Crop image**, **Rotate 90°**, **Flip horizontal**,
**Flip vertical**, and **Reset image** in both editors. Crop opens a shared
dialog with draggable edges, arrow-key adjustments (Shift for ten pixels),
percentage fields, and a live result preview. **Reset crop** restores the
full source while retaining orientation; **Reset image** clears both. Apply
commits one undoable edit; Cancel leaves the image unchanged. A removed or
changed image is reported instead of overwritten when applying a stale dialog.

On a board these are Excalidraw's `crop`, `scale` (flips) and `angle`
(rotation, about the image's centre): the original pixels stay in the file and
the renderer crops them. Cropping keeps the image's display scale. In a
document, the original stays in `src`; optional `imageEdit` holds the
normalized crop, quarter-turn rotation, flips, and rendered PNG together as one
attributed shared property, and both count toward image and item limits.
Board PNG and SVG downloads and document HTML/Markdown exports show only the
visible pixels: an SVG download replaces a cropped image with its cropped
rendering. The `.excalidraw` and native document files keep the original so
reset remains possible. Transformations follow local Undo/Redo and arrive at
other participants through the existing editor synchronization.
Arrows bind to shapes as in Excalidraw. A bound end in orbit mode sits on the
target's outline, offset by 5 + half its stroke width, toward the binding's
fixed point; inside mode places it at the fixed point. Bound ends follow moves
and resizes of their targets. Moving an arrow away from its targets unbinds
the ends whose targets stay behind. The editor and PNG/SVG exports share this
geometry; a binding to a deleted element is drawn unbound.

## Element library

**Library** offers the libraries kept on the session host: **My library**,
installed libraries, and mevedel's **Built-in** library. Clicking an item
inserts copies with fresh identities at the centre of the view, selected for
moving; groups, labels and bindings within the item are kept.

Each library is an `.excalidrawlib` file in `mevedel-shared-library-directory`,
by default `~/.mevedel/whiteboard-libraries/`, named after the file. **Add
selection** stores the selected objects and their labels in **My library**;
**Import file…** adds the items of an `.excalidrawlib` file there;
**Download** saves it. Items are removed with ×. Copying any Excalidraw
library file into the directory installs it as well. The libraries belong to
the host's Emacs, not to a session or board: every writable participant in
every room, and the assistant, sees the same libraries. Inserted items are
ordinary elements, so a board carries them without its libraries, as an
Excalidraw scene does. View links have no library.

**Browse public libraries** lists the collection at
[libraries.excalidraw.com](https://libraries.excalidraw.com) with a search
field, sorted by downloads, recent updates, age or name. Opening one previews
its items; **Install library** saves it in the directory, where it stays
available in every whiteboard until its **Remove**. The editor has no network
access, so Emacs fetches the index, download counts and library files from
`mevedel-shared-library-catalog-url`, and only library files listed there.
The built-in **Database** item replaces the former cylinder shape: a group of
an ellipse and lines, as Excalidraw has no cylinder element.

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
or targeted inverses. Board patches use Excalidraw elements with Excalidraw's
field names; absent fields take Excalidraw's defaults, labels are text
elements with a `containerId`, and connections are arrows with
`startBinding`/`endBinding`. The derived Excalidraw fields `version`,
`versionNonce`, `updated`, `isDeleted` and `boundElements` are refused. An
image element must reference an existing file; the assistant cannot add image
bytes. `SharedRead` with `library` lists the host's library items as
`LIBRARY/ITEM-ID` references with a numbered PNG sheet of their appearance,
optionally only for the libraries named in `selection`. `SharedEdit`'s
`insert` places such an item with its top-left corner at `x`, `y` as new
elements and returns their IDs, so the assistant can label or connect them. Whiteboard edits also return a PNG of the resulting
canonical revision through the normal tool-media path, so the model can inspect
the visual result without a separate read or a connected browser. A patch carries exact `before` values from a read and
new `after` values; null adds or deletes. Documents target top-level blocks
and may specify an `afterId` insertion anchor. Any stale target rejects the
whole transaction with current target data. Unrelated changes do not
invalidate an otherwise valid patch. Tools use normal permissions, Plan and
read-only ceilings, cancellation, and result/media persistence. They work
with every browser closed and never silently start a share.

### Questions and comments

The document's **Discussion** panel and the whiteboard's **Assistant** panel have
**Comments** and **Assistant** views. The assistant shows questions about
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
the passage. **Add comment** (Ctrl+Alt+M) opens a separate draft; **Post**
publishes an anchored discussion and **Post reply** adds an attributed human
reply. Each has a **Send to assistant** choice, checked by default: posting then
also sends the thread to the assistant, which answers in it. Unchecked, the
message stays a note between people and submits no model turn. The same choice
applies to artifact comments in the room. Questions and comment/reply drafts remain
independent when switching views, receiving updates, or reopening the editor.
An unrecognized saved discussion-draft format is reported and reset without
blocking recovery of document content or posted discussions.

**Ask about selection** opens the assistant composer directly with the passage
attached. **Use whole document** explicitly changes its scope. The attached quote
stays fixed when focus or the document selection moves elsewhere.

A request submits the posted human thread and its passage, without changing the
assistant composer or including unposted reply text. The thread's latest human
message is the visible request in the room and to the model; the whole thread
travels in the shared context. **Send thread to assistant** sends the thread as
it stands without a new message, and confirms a reviewed context or retries an
unconfirmed request. Queue and delivery status appear alongside the discussion. Canonical
AI replies render in that thread beneath the human message that prompted them;
ordinary questions remain in the Assistant view. Another AI request requires
another message sent to the assistant or an explicit thread send. The host checks the reviewed thread version as well as
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
Comments are session annotations: native, document and board exports contain the
content, not the comments or conversation. A fresh import starts without those
annotations.

Whiteboard comments anchor to objects, to a board area, or to both. Select
objects or box-select an area and choose **Add comment** (Ctrl/Command+Alt+M),
or use the **Comment** tool (M): it outlines the object under the pointer,
comments on a clicked object, and comments on a dragged area together with the
objects it contains. The tool stays active for further comments; Escape returns
to selection. Every open comment shows a numbered pin at the top-right corner
of its anchor, following moves live. Hovering a pin outlines its anchor and
shows the author, text, reply count, and whether its objects changed; clicking
it opens the thread. A ring turns around a pin while the assistant works on its
thread, the same way as for artifact comments. Resolved comments have no pin,
and pins renumber in order of the open comments. **Show objects** selects the thread's surviving objects
and area and frames them. Pins, hover cards and threads are visible to view
participants, who cannot post.

A board comment records its object IDs, optional area, quote, and a fingerprint
of those objects and their labels. The thread is
**changed** once an object moves, restyles, changes its label or is deleted, and
**removed** when none of its objects remain and it has no area. An area
outlives its objects. A request to the assistant attaches the anchor's surviving
objects and its area, subject to the same review of changed context; the host
accepts only objects from the comment's own anchor and its exact area.
The same 200-comment and 200-reply limits apply to whiteboards.

For a whiteboard, select one or more objects and choose **Ask about selection**.
After a box selection, the question is about the **Selected area**: it carries
the box's board region with the objects it contains, including an area with no
objects, so a request such as “put a legend here” names a place. Unselected
objects the area touches accompany it as context with their labels; images
appear by file reference, and their pixels only in the attached PNG, which
shows the area with a small margin and is scaled up to four times so small
areas stay legible. Selected objects bring their labels and the shapes their
arrows connect as context.
The discussion composer offers **Selected objects** (or **Selected area**) /
**Whole whiteboard** or **Selected passage** / **Whole document**. Switching to the whole item retains
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
item discussion turns. The room transcript still shows their shared chronology,
and lists each item as a discussion of its own: selecting it in the room shows
its turns and sends room messages into its conversation as whole-item questions
about the committed content (see [scoped prompts](collaboration.md#scoped-prompts-and-attachments)).
This is context selection, not a privacy boundary or a separate execution agent.
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
text or elements, the selected board area, bounded surrounding document blocks,
labels, connector endpoints or touched neighbours, and a matching board PNG as a normal
attachment. Questions use the ordinary queue
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

## Deleting

**Delete whiteboard/document…** in the editor's **☰** menu, or `d` on its row
in the Emacs artifacts cockpit, deletes an item for everyone: its content,
embedded images, comments and contribution history. There is no undo; the
browser's confirmation offers **Download a copy**, which can be imported
again. The deletion runs in the session's editing queue, so it lands after
any save in progress, and a project session commits it at once, so Resume,
Save As and Fork do not bring the item back. An editor showing the item in
another browser closes and names who deleted it; a later save to it fails
with "This item no longer exists". A browser keeps its local draft of a
deleted item only while it holds edits that never reached the host, listed
as a local recovery. The transcript keeps the item's discussion. The model
cannot delete items.

## Downloads and imports

Standard downloads use a committed revision: PNG/SVG or an Excalidraw file for
whiteboards, and Markdown/HTML or an editable native JSON file for documents.
The **Excalidraw file** (`.excalidraw`) is a complete Excalidraw scene: every
element field, fractional z-order indices, derived reverse references, label
and connector positions as displayed, and the image files in use. It opens in
excalidraw.com and other Excalidraw editors. Native and Excalidraw files
preserve supported content, relationships, and embedded assets. They exclude
credentials, presence, collaboration history, and undo stacks. Markdown and
images are viewable conversions with less editable structure.
Whiteboard PNG and SVG downloads use an opaque white background independent
of the room theme, without the editor grid or temporary highlights. SVG
downloads embed the fonts their text uses. SVG labels use individually
positioned text lines, preserving blank lines, and arrowheads use explicit
paths so Qt-based viewers retain the same text layout and arrows.

Import supports `.excalidraw` scenes (also as `.json`), the native document
format, Markdown/plain text documents, and raster images, which open as a new
whiteboard holding that image. It validates the complete input before creating
a new item and collaboration lineage, preserving the original. An Excalidraw
scene goes through Excalidraw's restore rules: legacy fields migrate, deleted
and invisible elements and unknown element types are dropped, and unusable
values take Excalidraw's defaults. Images in formats other than PNG, JPEG and
WebP are drawn as placeholders, and the room reports them after import.
Library files are imported in the editor's **Library**. Uploaded HTML,
JavaScript, external image URLs, and unsupported formats are rejected. There
are no legacy readers or migrations; whiteboards saved in mevedel's former
shape format no longer open.

Bounds are explicit: 16 MiB canonical state/native input, 4,000 elements and
200 image files per board, 4,000 points per stroke, 8 MiB per library, 200 targets per agent transaction, and 1 MiB document
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
editor dialogs and the board fonts embedded as data (`font-src data:`). Room credentials stay in the trusted viewer. The relay stays
content-blind. [ADR 0120](adr/0120-edit-shared-content-through-the-session-host.md)
records the change from model-only authorship to direct shared editing.

## Checks

```sh
npm test --prefix shared-editing
npm run test:browser --prefix shared-editing
npx @emacs-eask/cli clean elc
npx @emacs-eask/cli test ert test/test-mevedel-shared-editing*.el test/test-mevedel-shared-library.el test/test-mevedel-tool-editing.el test/test-mevedel-collaboration-editing.el
MEVEDEL_TEST_SHARED_EDITING=1 timeout 600s ./test/run-remote-acceptance.sh
```

The focused editor checks reproduce interior selection, inline text, cursor
replacement, local/remote laser trails, grouped contributions, export-neutral
assistant highlights, frozen context, stale/offline refusal, explicit comments,
reply rendering, disclosure continuity, box selection, area questions, board
comment pins and threads, and a small keyboard-sized viewport.
`test/board-tools.browser.mjs` covers shape recognition, the library (with a
stand-in for the host's library requests), groups, locking, duplicates,
arrowheads and fonts; `test/test-mevedel-shared-library.el` covers the library
file and public collection through a local HTTP server.
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
