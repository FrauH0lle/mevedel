# Shared whiteboard and document editing

Whiteboards and documents live in the workspace
[artifact store](view.md#artifact-store), so every session and room of the
project sees the same ones. The room's **Shared work** section separates
creation and import controls from its list of the whiteboards and documents
attached to the room's session; creating, importing or editing one attaches
it. The open item and local recoveries stay listed; **Project artifacts**
lists the rest of the project. Full and
owner links can create, rename, import, edit and delete them concurrently. View
links can observe and download. Opening an item affects that browser only;
other participants get a followable entry. Below a window width of 1800px each
editor opens in its own browser tab, leaving the room and its composer draft
in the original tab; from 1800px it docks beside the chat instead (below).
The editor tab reconnects independently and reopens its item on reload.
The editor tab's header shows the item's title, editable in place by full and
owner links, over its save state, such as **Saved on host** or a save failure;
the editor reports both to the tab over its item port and performs the rename.
A failed save also marks the editor menu, where **Retry save** then appears.
The back button before the title shows the session in that tab. The host uses the same editor through
a browser share link. Browser popup permission is needed to open a new tab.
An editor tab shows its loading state immediately. If the item cannot be read,
it keeps the requested item and displays an error with Retry; it does not fall
back silently to the room. The back button cancels a pending editor opening.

### Docked beside the chat

In a window at least 1800px wide, opening a whiteboard or document from
Shared work, a card or a link, or creating or importing one, opens it in the
room's own tab: the chat keeps an 860px column on the left and the editor
takes the rest, under the room's header. The editor's head is compact: the
back button, the item's title and save state, presence, and **Open full ↗**,
which closes the docked editor and opens the item in its own tab. Its
discussions sidebar starts open, on the right, as in the editor tab.

The chat follows the docked item: opening it selects the item's tab in the
filter strip, listed even before the item's first turn, so the composer writes
to the item's [general discussion](collaboration.md#discussions). The sidebar's
row for the whole item reads **in chat ←** rather than opening a second
composer for the same discussion; choosing it selects the item's tab again and
focuses the chat's composer. That discussion's news is read in the chat, so
the row raises none. Part discussions behave as in the editor tab. The chat's
dock, including pending requests, stays in place beside the editor. **In this
room** moves behind the header's room button, which opens it as a drawer over
the editor's right side; it holds no news of its own, since requests appear in
the chat. The page keeps the room's title, and presence reports the docked
item as the page being viewed.

Narrowing the window below 1800px turns the docked editor full screen in the
same tab, with its state and drafts; the sidebar's whole-item row becomes the
ordinary discussion again. Widening the window does not dock it again; the
next opened item docks. Closing the editor returns to the room and keeps the
item's tab selected. HTML artifacts open in their panel as before.

From Emacs, `o` on an item's row in the artifacts cockpit, or clicking a
`[title](shared://ID)` link in the transcript, opens that item's editor tab in
the default browser with the session's full-control link. The editor exists
only in a room, so for a session that is not shared this first asks, with
`/collab`'s disclosure, to start sharing it; the room then stays up until
`/collab stop`. The cockpit opened outside a session uses the room of the
item's own conversation, its dedicated session, and the lobby's **Artifacts**
tab does the same: a whiteboard or document opens in its editor there.

`n` in the artifacts cockpit or the session menu, or
`M-x mevedel-artifacts-new-shared-item`, creates one: it asks for Document
or Whiteboard and a title, then opens the new item the same way. The sharing
question comes before anything is created, so declining or cancelling a prompt
leaves no item and no room; a creation that fails starts no room either. From
a session the item is attached to it; elsewhere it opens in its own
conversation. A read-only session refuses before asking, and one that loses
control while the creation is queued creates nothing. Once created, an item is
kept: a failure to attach or open it is reported with its `shared://ID`
address. Creating records no version.

Every creator -- the browser, the model's `SharedCreate` and Emacs -- applies
the same rules: the kind is a whiteboard or a document, and the title,
trimmed, has 1 to 200 characters.

Documents use a continuous paper surface, serif body text, and grouped **Text**,
**Lists**, and **Insert** menus. Room and editor controls share cool and warm
palettes and system/light/dark appearance; changes follow the editor without
replacing its document or discussion drafts. The whiteboard keeps a light drawing
surface so authored colors remain unchanged.

**Discussions** opens the [discussions sidebar](#discussions-in-the-editor), a
sidebar on desktop and a full-width overlay on phones. It starts open on
wide screens for documents. Drag its divider to widen it, up to half the viewport; Left/Right
arrow keys also resize it, Home/End select the limits, and double-click resets
its width. The width is retained with the item's private drafts. Closing the
panel preserves drafts, captured context, and editor position. The editor
follows the visible viewport above the keyboard; Escape closes the panel, and
Ctrl/Command+Enter sends what its box holds.
Downloads, recovery, retry, contribution history, and assistant highlighting
are in the editor's **☰** menu, with a document's word count. A document's
toolbar row holds its formatting, then undo, redo, **Discussions** and **☰**;
on a phone the formatting scrolls sideways while those actions stay, the
Discussions button shows its icon and count, and the page runs edge to edge.
The formatting menus open below their buttons, clear of the scrolling row.
Pressing an image selects it for its tools, and closing the crop dialog
returns to the document or board.

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

The whiteboard's controls float over its canvas in islands, as Excalidraw's
do: the editor menu (☰) at the top left, the drawing tools centred at the top
with the current tool's hint below them, **Library** and **Discussions** at the
top right, zoom with undo and redo at the bottom left, and the keyboard
shortcuts (**?**) at the bottom right. Where the board is narrower than 1,100
pixels, as beside the sidebar, the tools move to its foot and the hint goes;
on a phone-sized board the tools scroll sideways along the foot and **?**
goes too.

The drawing menu follows Excalidraw: hand (H), selection (V), rectangle (R),
diamond (D), ellipse (O), sticky note (N), arrow (A), line (L), freehand
drawing (P or X), **Shape from drawing** (Shift+X), text (T) and eraser (E),
followed by the laser (K), Discuss (M) and image (9) tools. Number keys 1–8
and 0 select tools as in Excalidraw. Buttons expose tool names and keyboard
shortcuts. Selection supports Shift multi-selection, arrow-key
movement, Delete, and resizing. Dragging across empty canvas with the
selection tool, including by touch, draws a selection box: releasing selects
the objects it fully contains, Alt selects every object it touches, and Shift
adds them to the current selection. Connectors are tested by their drawn path.
A press without movement clears the selection. The box disappears on release,
but its area stays attached to the selection, even when it contains no
objects, until the selection changes or an object moves: **Discuss** uses it,
and the area shows again while a new discussion about it is on screen. Box
selection never edits content.

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
Finishing a shape returns to selection with the shape selected, so a
double-click edits it instead of creating more shapes. Freehand drawing keeps
its tool, as strokes come in runs. The lock button (Q) before the tools keeps any
drawing tool active after each shape; it belongs to the browser, not the board. Text is shared while typing; blur or Ctrl/Command+Enter
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

The selection panel follows Excalidraw's: it opens at the board's left with a
selection, or an area left by a box selection, and on a board at least 1,100
pixels wide also with a drawing tool, for the next object. Its header names
the selection and offers **Discuss**; × closes it until the selection or tool
changes. On a phone-sized board it is a sheet over the board's foot, in place
of the tools and zoom. It holds the style, the selection's **Layers** (send to
back, bring to front) and **Actions** (duplicate, group, ungroup, lock and
delete), and for a lone image the image tools. The style follows Excalidraw: stroke and background colours,
hachure/cross-hatch/solid fill, stroke width, solid/dashed/dotted strokes,
sloppiness (architect, artist, cartoonist), sharp or round edges, the arrow
type (sharp, curved or elbow), start and end arrowheads (none, arrow,
triangle, circle, bar), font (hand-drawn Excalifont, normal Nunito, code Comic
Shanns), font size, text alignment, vertical alignment for a label inside a
shape, and opacity. It shows only the sections that apply to the selection or
the active drawing tool, changes the selected objects and their labels, and
remembers the choices for new ones.
A curved arrow runs through its points; switching a straight arrow to curved
adds a bend to drag. An elbow arrow leaves and enters its bound shapes at the
facing side and runs in horizontal and vertical segments between them,
rerouted as the shapes move. When a shape stands in the way, the route detours
around it with as few bends as it can, leaving and entering its shapes by
whichever sides suit the detour; a shape behind either end, such as a lane
holding the node, is not in the way. Text, lines, drawings and frames never
block a route.
Freehand drawing records a pen's pressure for every sample the browser
reports; mouse and touch strokes simulate pressure from their speed. Stored
line, arrow and stroke points keep 0.1 board units, and a freehand stroke drops
samples within one unit of the last one it keeps, retaining its first and last.
Pointer input repeats and jitters, and every sample costs storage, sync and
model context. Elements stored before this rule keep their points until next
written. Elements imported with other Excalidraw values, such as
the cardinality arrowheads, keep and draw them.
The editor menu adds **Select all** and **Unlock all** for the whole board.
Duplicate (Ctrl/Command+D), Group (Ctrl/Command+G), Ungroup
(Ctrl/Command+Shift+G), Delete, Escape to clear the selection, Enter or a
double-click to edit text, and the arrow keys to nudge by 1 or, with Shift,
10 pixels work from the keyboard as well.
Clicking any member of a group selects the whole group. A locked element
cannot be selected, moved or erased until **Unlock all**. Duplicates and
copies keep the bindings and labels among themselves and drop the others.
**?** lists the canvas keys. Selection handles retain their screen
size as the view changes.
Scrolling pans the board, and Shift turns a mouse wheel sideways;
Ctrl/Command+scroll and a trackpad pinch zoom at the pointer, in proportion to
the scroll. Panning, zooming, the zoom buttons, Fit, and the percentage button
(reset to 100%) affect only the local viewport. The board fits existing objects once on opening. Fit and opening keep them clear of the controls floating over the canvas's top and foot, and zoom in no further than 100%, as Excalidraw's fit does.
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

**Library** opens the sidebar's Library tab, beside Discussions; each toggle
closes its own tab. It offers the libraries kept on the session host: **My library**,
installed libraries, and mevedel's **Built-in** library. Each library is a
collapsible section; My library starts open, the others closed, and the
editor remembers which sections a participant opened while it stays loaded.
The search field matches item names and library names across every library,
opening the sections with matches. Clicking an item
inserts copies with fresh identities at the centre of the view, selected for
moving; groups, labels and bindings within the item are kept. The sidebar
stays open beside the board; on a phone, where it covers the board, it closes
to show the inserted item. The libraries load when the tab first shows.

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
An invalid or unsupported installed file reports its own error in the browser
and at `shared://library`; other libraries remain available, with the same
item numbering in the model's listing and preview sheet.

**Browse public libraries** lists the collection at
[libraries.excalidraw.com](https://libraries.excalidraw.com) with a search
field, sorted by downloads, recent updates, age or name. Opening one previews
its items; **Install library** saves it in the directory, where it stays
available in every whiteboard until the **Remove** button inside its opened
section, which names the library, removes it. Installed libraries are removed
whole: they mirror a published file, so useful items are kept by inserting
them and adding them to My library. The editor has no network
access, so Emacs fetches the index, download counts and library files from
`mevedel-shared-library-catalog-url`, and only library files listed there.
The built-in **Database** item replaces the former cylinder shape: a group of
an ellipse and lines, as Excalidraw has no cylinder element.

Ordinary board cursors move one named arrow per participant. Selecting **Laser
pointer** makes mouse or pen hover visible locally and to collaborators; on a
touchscreen, press and drag to point, then lift to stop. A bright tip, a compact
name tag, and a short curved trail distinguish deliberate pointing from an
ordinary cursor. Older trail segments fade independently within about half a
second; a stationary tip stays visible while the participant points. Holding
the mouse button, pen, or touch while pointing draws laser ink instead: the
line stays for two seconds and fades over the next, so a participant can
sketch a circle or arrow on top of the board without editing it.

Coordinates travel in board space and render through each receiver's viewport.
Remote cursors use a short transition from the displayed position to each
received sample, without predicting past it. Laser packets retain up to 64
input samples from the last 550 ms, rather than only each packet's last position.
The observer plays those samples with a short 55 ms delay, preserving curves
between network updates. Each sample is `[x, y, age, ink]`; held samples carry
an increasing ink index, which lets observers accumulate a line longer than one
packet's window. Each press skips an index, so separate strokes do not join. A stale gap starts a new pointer
instead of drawing a bridge across the board. Reduced-motion preferences remove
trails and interpolation. Leaving the canvas, switching tools, cancellation,
hiding the page, and disconnect clear the local pointing state; item changes
and peer departure clear remote state. Idle presence expires if its occasional
refresh is lost. Motion coalesces to at most 20 updates per second and retains
the final position. Presence is discarded behind queued content/control traffic;
it creates no revision, undo history, export marks, queued question, or model
context.

## Working with the assistant

The model reads shared items with Read and Grep at `shared://` addresses and
changes them with `SharedCreate` and `SharedEdit`. Every role has Read; the
editing tools are discoverable through ToolSearch/ToolCall to implementation
and worker roles. Their summaries name whiteboards and documents, and
ToolSearch's `shared` key separates them from artifacts, so a requested
whiteboard finds `SharedCreate`. The prompt's resource roster lists
`shared://` once the project has an item.

| Address | Read returns |
| --- | --- |
| `shared://` | the project's items with kind and title, marking those attached to the session, and `shared://library` |
| `shared://ID` | a header with kind, title, revision and the related addresses, then one line per element in drawing order or per top-level block in document order |
| `shared://ID/elements/ELEMENT` | one element or block in full, structurally formatted with lossless long-string chunks |
| `shared://ID/view.png` | the board rendered, long edge at most 2048 px |
| `shared://ID/images/KEY` | an embedded image: a board file by its `fileId`, a document image by the key its `src` shows |
| `shared://ID/discussions` | the item's discussions, the general one first, with their status, target, live quote and messages |
| `shared://ID/history` | retained contributions, newest first, with their changed ids |
| `shared://library`, `shared://library/NAME` | element library items as `LIBRARY/ITEM-ID` references |
| `shared://library[/NAME]/sheet.png` | a numbered sheet of the first 60 of those items |

An overview line is `HASH JSON`: a 12-character content hash of the element or
block, then its JSON. The hash is computed over sorted keys, so writers' key
order does not matter. Lines stay within Read's 2,000-character cap: past
1,800 characters a stroke's `points` and `pressures` become a count with the
element's address; other oversized entries become explicitly abbreviated
summaries pointing there. An abbreviated entry is not replacement JSON.
The full-element view formats nested structure and stroke points across lines.
Long string values or keys use the output-only `string-chunks(...)` notation:
decode its ordered JSON string lines and join them without separators before
passing the value to SharedEdit. Its header explains this notation; it never
changes stored content, image addresses or the hash. Long discussion and
contribution lines use the same lossless chunks. Document images appear as
`shared://ID/images/KEY`, never as image data. Text pages through Read's ordinary `offset` and `limit` and its
50 KiB output bound, so an item of any size can be read in parts. Grep searches
complete values while returning addresses and physical line numbers of ordinary
Read output. An item's overview, or bare `shared://`, also searches the existing
full-element resources behind abbreviated entries. Matching joins string-chunk
display boundaries internally, then maps each hit and its context back to the
actual Read lines, including all lines touched by a cross-chunk match. Images go through Read's ordinary media
delivery, so models without image support get its explicit refusal. Glob and
ApplyPatch do not accept `shared://`. Views are computed by the workspace's
editing host from committed state, without a browser.

Creating or editing an item attaches it to the model's session. When a turn
that created or changed items settles, each of them gets a version in the
store; an edit that changed nothing does not count.

`SharedCreate` creates a named item and returns its address. `SharedEdit`
applies patches, inserts library items, adds images, renames, sets a board's
background, or reverts a contribution from `shared://ID/history`. A patch change is
`{id, hash, set, unset, after, afterId}`:

- `set` merges fields into the current element or block and `unset` removes
  fields; the result is validated whole.
- `after` replaces the element or block; `null` deletes it.
- A new element or block gives `after` without a hash.
- Document blocks may give an `afterId` insertion anchor, which may name a
  block inserted earlier in the same patch; insertions sharing an anchor keep
  patch order, each followed by those anchored to it. Their image `src`
  values may keep the `shared://ID/images/KEY` form, which the host restores
  to the image data.

The host compares every given hash, or its absence, with the current target.
Any mismatch rejects the whole patch and returns each stale target's current
hash and line. Unrelated changes do not invalidate an otherwise valid patch.
Board elements use Excalidraw's field names; absent fields take Excalidraw's
defaults, labels are text elements with a `containerId`, and connections are
arrows with `startBinding`/`endBinding`. The derived Excalidraw fields
`version`, `versionNonce`, `updated`, `isDeleted` and `boundElements` are
refused. An image element in a patch must reference an existing file; the
`image` action adds a new one. `insert` places a library item with its
top-left corner at `x`, `y` as new elements.

### Images in and out

The `image` action takes one `source`: a file path or a resource address the
model can read (`work://`, `artifact://`, or a `shared://ID/images/KEY` or
`shared://ID/view.png` of any item). It is authorized as a read, like Read, and
not as an edit (see [permissions](permissions.md)). The tool reads the bytes and
sends them to the host, which decides the format from the bytes, not the name:
PNG, JPEG and WebP are stored as they are, and an SVG is rasterized to a PNG at
twice its intrinsic size, the long edge at most 2,048 px, because boards and
documents hold only those three raster formats; the item still shows it at its
intrinsic size. The source file may be up to 4,700,000 bytes; the
16-megapixel, 8,192-px-per-side and 32-megapixel-per-item limits apply, and a
failure names the size or limit and what to change.

A board image gets a content-addressed file id, the SHA-1 of its stored bytes,
as a browser upload does, so identical bytes are stored once, and an element at
`x`, `y` (give both or neither; the default is below the existing content). A
document image becomes a block
after `afterId` (default: the end) with the optional `alt`. The display width
is the image's own up to 640 px unless `width` is given; the height keeps the
aspect ratio. The addition is one attributed contribution, the file is
retained with it, and Revert removes the image and a Revert of that Revert
restores it. The result lists the new element or block line and, for a board,
the resulting PNG.

`SharedSaveImage` goes the other way: its `source` is a `shared://ID/images/KEY`
or `shared://ID/view.png` address and its `file_path` a new file, which keeps the
image's own format and gets its folders created. It refuses an existing file
instead of overwriting it, which keeps Rewind exact, and is an edit tool, so
Plan refuses it and the destination needs write authority. Copying ordinary
files is Bash's job.

An edit result names the item, its revision, address and contribution, the
deleted ids, and the stored lines of the elements it changed, including
derived changes such as a label's container growing. Past 24 KiB of lines,
the rest are reported as ids and hashes. It does not repeat the board. Whiteboard
edits also return a PNG of the resulting canonical revision through the normal
tool-media path, so the model can inspect the visual result without a separate
read or a connected browser. Tools use normal permissions, Plan and read-only
ceilings, cancellation, and result/media persistence. They work with every
browser closed and never silently start a share.

### Discussions in the editor

Whiteboards and documents follow the room's [discussion](collaboration.md#discussions)
model and its shared discussions sidebar. **Discussions** opens the sidebar's
Discussions tab on the list: the whole item first, then the open discussions
with their pin number, then the resolved ones folded into one row. The tab and
the **Discussions** button count the open discussions, and the button shows a
dot while news in another discussion is unread. A status line under the tabs
reports the session's queued, paused, working, disconnected, or
provider-failure state, and stays away while there is nothing to report.
While the session is working, a spinner accompanies the status and the panel's
toolbar button. The room chat shows **Assistant working…** in its status strip.
These indicators follow the host's session activity, including tool execution,
and clear on completion or disconnection. Reduced-motion preferences disable
the spinning animation. A paused follow-up queue does not conceal a running turn.
Posted messages and assistant answers are shared with the room; drafts stay
in the current browser's recovery storage. View participants can read them.

**Discuss** (Ctrl/Command+Alt+M) opens a new discussion about the current
selection: selecting document text shows it beside the passage, a board shows
it in the selection panel, and the list's foot offers it, as **Discuss
selected passage**, **Discuss 2 selected objects** or **Discuss selected
area**. The new discussion names the **Selected passage**, **Selected
objects** or **Selected area** and quotes it; the passage or area stays
highlighted while it is on screen, and **Refresh** attaches the content as it
is now. **Start discussion** posts it and keeps it open. **Ask assistant**,
checked by default, also sends the message to the assistant; the choice is
kept for the next message. A discussion's box replies to it, **Show passage**
or **Show on board** shows its target, and its ⋯ menu holds **Ask assistant
again**, which sends the discussion as it stands, and **Resolve** or
**Reopen**. When a request needs attention, the discussion offers **Review
updated context**, **Send to assistant** for reviewed context, or **Retry** for
an unconfirmed request. The discussion about the whole item writes to it and
takes file attachments. A message to the whole item asks about the item as it
is when sent; it carries no reviewed capture, since none is shown. Each
discussion's draft, the new discussion and the sidebar width survive
receiving updates, closing the sidebar and reopening the editor; going back to
the list drops a new discussion. The box starts at two lines and grows with
its text, and no size depends on focus, so pressing a button never moves it.
An unrecognized saved discussion-draft format is reported and reset without
blocking recovery of document content or posted discussions.

### Sending a selection without a message

A selection can be sent with no message, on a board or in a document: what is
written in the selected content is then the request, for example a handwritten
“draw it” beside a formula, or “Rephrase this paragraph” typed above the
paragraph. A new discussion says so in its placeholder when **Ask assistant** is
checked. A message to the whole board or document still needs text, because
it has no instruction of its own, and so does a discussion that does not ask
the assistant.

The browser sends the empty message and the host words it, so the room sees,
and the model receives, the same line in place of a message; the discussion
shows **Sent the selection as the request**. The line says the
sender sent only the selection, tells the model to carry out the instructions
written in it and, when there are none, to ask what the sender wants. This
overrides the “user-provided data” label of the snapshot for the selection
itself; the unselected content in the snapshot stays data, so another
participant's text becomes an instruction only when someone selects and sends
it. Afterwards the instruction text is removed from a document, where it would
pollute the content, and left on a board unless it says otherwise. A written
message always replaces the host's line. Once the turn completes, the
discussion resolves itself.

### What the assistant receives

A request carries the discussion's latest message, which is the visible
request in the room and to the model, with the discussion so far in the shared
context; unposted reply text never travels. Queue and delivery status appear
in the discussion. Another AI request requires another message sent to the
assistant or an explicit **Ask assistant again**. The host checks that the discussion has
not moved on since the sender reviewed it, as well as its target; a concurrent
reply requires sending again. Failed delivery retains its request identity for
retry.

Clicking a document highlight opens its discussion; **Show passage** scrolls
back to the live passage. Resolving hides its highlight.

A document discussion keeps its original quote and a collaboration-aware text
anchor. The discussion marks a passage changed or removed after later edits. A
changed passage requires **Review updated context** before sending; a removed
passage cannot be sent. The immutable snapshot of an already sent request
remains unchanged. In the editor sidebar, room transcript, and Emacs view, sent
requests keep their authored text visible and collapse the snapshot under
**Shared context** by default. Expand it to inspect the full sent context and
attachment links; the summary names the item, scope, and revision. This
affects display only, not what the model receives. Prompts edited on the host
remain fully visible. Discussions are item annotations: native, document and
board exports contain the content, not the discussions or conversation. A
fresh import starts without them.

A whiteboard discussion is about objects, a board area, or both. Select
objects or box-select an area and choose **Discuss**, or use the **Discuss**
tool (M): it outlines the object under the pointer, discusses a clicked
object, and discusses a dragged area together with the objects it contains.
The tool stays active for further discussions; Escape returns to selection.
Every open discussion shows a numbered pin at the top-right corner of its
target, following moves live. Hovering a pin outlines its target and shows its
latest message, message count, and whether its objects changed; clicking it
opens the discussion in the sidebar. A ring turns around a pin while the
assistant works on its discussion, the same way as for artifact discussions,
and a dot marks news not yet opened. Resolved discussions have no pin, and pins
renumber in order of the open discussions. **Show on board** selects the
discussion's surviving objects and area and frames them. Pins, peeks and
discussions are visible to view participants, who cannot post.

A board discussion records its object IDs, optional area, quote, and a
fingerprint of those objects and their labels. The quote names labelled
objects and counts unlabelled ones by type, such as `freedraw ×24`. In a
discussion, a request sent to the assistant shows only its sender and shared
context, since its text is the latest message. The discussion is
**changed** once an object moves, restyles, changes its label or is deleted, and
**removed** when none of its objects remain and it has no area. An area
outlives its objects. A request to the assistant attaches the target's surviving
objects and its area, subject to the same review of changed context; the host
accepts only objects from the discussion's own target and its exact area.

A box selection is discussed as the **Selected area**: it carries
the box's board region with the objects it contains, including an area with no
objects, so a request such as “put a legend here” names a place. Unselected
objects the area touches accompany it as context with their labels; images
appear by file reference, and their pixels only in the attached PNG, which
shows the area with a small margin and is scaled up to four times so small
areas stay legible. Selected objects bring their labels and the shapes their
arrows connect as context.
A private document highlight keeps a new discussion's passage visible while it
is on screen; closing the sidebar removes that highlight and reopening it
shows the passage again. The quote shows what will be sent before submission.
Opening controls, typing, and toggling the sidebar retain that capture.
Pending edits must save first. The browser sends a content hash of the capture
it showed, and the host compares it with a capture of its committed content; a
concurrent change rejects the request with an explicit **Refresh context**
recovery. It never substitutes a whole item for a missing selection. No
selection is too large to discuss. The model receives the reviewed content
as the hashed lines it reads at `shared://ID`, introduced by the item's address
and revision. A document selection gives its text and the blocks it spans; a
board selection or area gives its elements, then its labels, connected and
nearby elements. Past a 128 KiB budget the remaining lines are counted and the
model is told to read on at `shared://ID`. The discussion so far follows the
content, and the board PNG accompanies it as before.

Each document and whiteboard has its own model conversation. A request receives
its reviewed content snapshot and recent turns about the same item, including
requests from every one of its discussions. Other items and ordinary room chat are
excluded. Ordinary room requests and room compaction summaries likewise exclude
item discussion turns. The room transcript still shows their shared chronology,
and lists each existing item as a discussion of its own: selecting it in the room shows
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
typing, posting without **Ask assistant**, and synchronization never start model turns automatically.

## Durability and recovery

An item is `<workspace>/.mevedel/artifacts/.state/ID/state.json`, with `meta.el`
naming its kind, title and revision, and `versions/`. Catalogs read these small
metadata records without parsing the item's CRDT state or history.
Emacs serializes operations per
workspace in one queue, which every room and session of this Emacs shares,
validates candidates in the helper, checks continuing authority, and commits
before acknowledging Saved on host. Consecutive queued edits of one item
(updates, patches, inserts, renames and background changes) commit together:
each is computed from the state the previous one left, and up to 16 are
written by one commit, then acknowledged and announced in order. When an edit
continuing such a batch fails, the batch commits on its own. A batch commits
only while none of its edits was cancelled or lost its authority and this
Emacs still holds the item's lease as it did when the batch began; otherwise
every edit in it is reported unsaved, since each was computed from the one
before. Batched edits count toward the queue's limits. Under several
writers the commit, one fenced target program, otherwise made edits wait for
each other's commits. Embedded images travel with their item's
state. Ordinary read-only artifact addresses gain no new mutation capability.
Requests and helper replies arriving during a remote file operation retain
their continuations until the transport is idle, so TRAMP's temporary timer
lists cannot strand the workspace queue.

Across Emacs instances, an item lease decides who may commit
(`mevedel-artifact-lease.el`). It lives outside the store, under
`.mevedel/leases/artifacts/ID/`, so the store stays safe to commit. It uses
the session lease's generation records, target clock and expiry
(`mevedel-session-lease-seconds`): the first edit in an Emacs acquires it,
a timer renews it, and every state write proves ownership and expiry in the
same target program. Lease transitions, state and metadata writes, and item
deletion share a target-side `flock` on the stable
`.mevedel/leases/artifacts/` directory, so a takeover cannot interleave with a
commit. The execution target must provide `flock`.
These file operations are serialized, but are not a crash-atomic multi-file
transaction. A failed program reports a save failure and can leave an incomplete
new item or state and catalog metadata at different revisions until retried.
After `mevedel-artifact-lease-idle-seconds` without edits, or when Emacs
exits, the lease is released. Another Emacs that tries to edit a held item
gets a read-only refusal and asks the holder to hand it over; the holder
releases it to that Emacs at its next renewal once its queue for the item is
idle and nobody has edited the item there for 10 seconds, so a person still
drawing keeps it. A request that is not repeated within one lease period
lapses, so a requester who walked away does not take the item later. A
lease whose holder stopped renewing, such as a suspended laptop's, is taken
over by the next edit, from Emacs or a browser, and Emacs says so. Each write
replaces the whole item under the lease, so a late write from the old holder
fails instead of overwriting.
While the lease is held and was renewed within the last minute, an edit does
not read the target clock again. While this Emacs still holds the lease it
last committed under, and renewed it within one lease period, an edit or read
starts from that commit instead of reading the item back: the lease fences every state write, so one fenced target program
per edit remains. Metadata has other writers, so the commit verifies it and,
when it changed meanwhile, rereads it and retries once. Reading needs no lease. An open editor that
receives a change skipping a revision, as after a hand-over between two Emacs
instances, rereads the whole item instead of applying only that change. If
that read fails, the editor says so and reads again with the next change.

Versions follow the store's caps. A version keeps the item's content, title
and discussions but not its receipts or contribution history. One is recorded
through the item's editing queue, after the saves before it, when a model
turn that changed the item settles, and on **Save version** (the
cockpit's `s`, or the store list in a room or the lobby). Restoring a version
is one ordinary, attributed edit through the queue: lineage, discussions and
history stay, concurrent editors receive it as an update, and the restore can
itself be reverted. The most blocks of a document that are the same in the
version and still in its order keep their identity, so discussions anchored in
them stay current, even around a moved block; the others are copied whole
from the version. Whiteboard restores update retained objects in place, so a
pending browser edit to an unchanged object still merges. Removed objects
retain the normal deletion-wins behavior.

Sessions only attach to items, so Resume, Save As, Fork and Rewind neither
copy nor roll them back. Closing a browser or ending the share leaves the host
original intact. A later share uses fresh credentials, following
[ADR 0114](adr/0114-tie-collaboration-room-lifetime-to-host-share.md).

An editor sends a local edit as soon as no save of its own is awaiting the
host's reply. Edits made while one is in flight join a single follow-up
operation, sent when the reply arrives. Consecutive sends start at least 50 ms
apart, so a slider or color-picker drag against a fast host does not commit,
and spend an operation receipt on, every frame.

The viewer retains pending changes in browser storage, scoped to room and
item. Reauthentication in the same logical share merges them. Disconnected,
rejected, or storage-failed edits remain visibly pending and have a Recovery
copy download. A recovery copy is explicitly named and represents that
browser's content, including unsynchronized changes. It is not a host save.
Storage quota errors ask the user to download before closing. Retry save
rereads committed state and retries operation identities without duplicating
content. A host storage failure leaves the item unchanged until a retried save
succeeds.
After a reload, locally retained items remain available as **local recovery**
entries even when their share or host item is gone. They open read-only for
recovery download until the host supplies valid current content and authority.

## Deleting

**Delete whiteboard/document…** in the editor's **☰** menu, or `d` on its row
in the Emacs artifacts cockpit, deletes an item for everyone: its content,
embedded images, discussions and contribution history. There is no undo; the
browser's confirmation offers **Download a copy**, which can be imported
again. The deletion runs in the workspace's editing queue, so it lands after
any save in progress, and removes the item's whole artifact: versions, its
lease and its dedicated session. An editor showing the item in
another browser closes and names who deleted it; a later save to it fails
with "This item no longer exists". A browser keeps its local draft of a
deleted item only while it holds edits that never reached the host, listed
as a local recovery. The transcript keeps the item's discussion: its turns stay
in the room under **All**, their chip marked **deleted**, but the item loses its
tab, so nothing can be sent into a conversation about an item that no longer
exists. A browser showing that tab returns to **All**. A deleted HTML artifact's
discussion goes the same way. The room lists an item's tab only once the host
has listed its items, so a deleted item's tab does not reappear on reload. The
model cannot delete items.

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
most 4 MiB, and a file the assistant adds at 4,700,000 bytes. Recent revert snapshots are bounded by both count and bytes as
described above; they expire before older snapshots block further saves. The
receipt ledger has a 65,536-operation ceiling. A native export/import begins
a fresh history when that ceiling is reached. A question's model context is
held to 128 KiB, with the remainder named by address rather than refused.
Transfers
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
npx @emacs-eask/cli test ert test/test-mevedel-tool-permission-read-access.el test/test-mevedel-pipeline-read-access.el
MEVEDEL_TEST_SHARED_EDITING=1 timeout 600s ./test/run-remote-acceptance.sh
```

The focused editor checks reproduce interior selection, inline text, cursor
replacement, local/remote laser trails, grouped contributions, export-neutral
assistant highlights, frozen context, stale/offline refusal, discussions with
and without the assistant, selections sent alone, reply rendering, disclosure
continuity, box selection, area discussions, board discussion pins, and a small
keyboard-sized viewport. The editor fixture stands in for Emacs's discussion
actions and records what the assistant would be asked.
`test/board-tools.browser.mjs` covers shape recognition, the library (with a
stand-in for the host's library requests), groups, locking, duplicates,
arrowheads and fonts; `test/test-mevedel-shared-library.el` covers the library
file and public collection through a local HTTP server.
The room scenario also checks new-discussion draft recovery across repeated reloads. Set
`MEVEDEL_EDITOR_SCREENSHOTS=1` when running `test/editor.browser.mjs` to save
preview screenshots under `.scratch/shared-collaborative-editing/`.
The focused room design and navigation checks also run with
`MEVEDEL_BROWSER=firefox` after installing Playwright Firefox with
`shared-editing/node_modules/.bin/playwright install firefox`.

The room browser scenario checks the separate tab and preserved drafts,
as well as caret visibility after a mobile viewport resize. It uses a real
relay and isolated Emacs, multiple Chromium contexts, and a deterministic participant invoking actual native tools. It
requires Go, Emacs dependencies installed by Eask, and Playwright Chromium
(`shared-editing/node_modules/.bin/playwright install chromium`). The remote
command provisions the repository's temporary SSH/container fixture.
