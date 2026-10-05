# Store whiteboards as Excalidraw elements

Status: accepted

Amends ADR 0120's whiteboard schema, image transforms and connector rendering.
Its host authority, Yjs synchronization, attributed transactions, editor
isolation and document editor are unchanged.

The whiteboard was a clean-room imitation of Excalidraw, inherited from the
reference template: Excalidraw's tools, hand-drawn sloppiness, hachure fills
and sticky notes, with its own shape record (`box`, `stroke`, `rough`,
`from`/`to`). The user wanted Excalidraw's interchange: open and save
`.excalidraw` files, use `.excalidrawlib` libraries including the public
collection, and turn hand-drawn strokes into clean shapes. A private record
made each of these a lossy translation.

Board content is now Excalidraw elements: Excalidraw's types, field names and
values in the Yjs `elements` map, and its image files in a `files` map.
Rendering follows Excalidraw's renderer, using roughjs and perfect-freehand
with the element's seed, Excalidraw's text metrics and its fonts. The editor
UI remains mevedel's, because its comments, area questions, presence,
movement previews, assistant highlights and Revert sit on that UI, and the
board was not a pain point; only the data layer and its drawing changed.

## Decisions

- **Excalidraw's own reconciliation fields are derived, not stored.**
  `version`, `versionNonce`, `updated`, `isDeleted` and `boundElements` exist
  for Excalidraw's collaboration; Yjs already merges concurrent edits. Export
  generates them; a patch carrying them is refused. Absent optional fields
  take Excalidraw's defaults, `seed` derives from the id and an element without
  an `index` draws on top, so the model writes only what it means.
- **References are stored in one direction.** A label stores `containerId`,
  an arrow its `startBinding`/`endBinding`. Excalidraw also stores the reverse
  `boundElements`; under concurrent edits two independent lists can disagree,
  so they are derived. A reference to a removed element is drawn unbound and
  dropped on export instead of failing validation, because rejecting it would
  reject an unrelated writer's update after a concurrent delete.
- **Displayed geometry is derived.** Bound arrow ends (orbit and inside
  modes) and label wrapping and placement are computed from current shapes in
  the browser, host PNGs and exports alike, so stored arrow points and label
  positions can never go stale. Text metrics come from advance-width tables of
  the bundled fonts, not the browser, so every client and the host wrap a
  label at the same places.
- **Images live in content-addressed files.** An image element names a
  `fileId`; bytes are stored once in `files`. Contributions then record image
  element changes without image bytes, which removes the history growth that
  saturated ADR 0120's boards. The host prunes files that no element and no
  retained contribution references, so Revert can still restore a deleted image.
  The model cannot author image bytes; it can only reference existing files.
- **Crops are Excalidraw fields.** Boards store `crop`, `scale` (flips) and
  `angle`, not ADR 0120's rendered `imageEdit` PNG; documents keep `imageEdit`.
  The `.excalidraw` file keeps the original so a crop stays editable. An SVG
  download re-renders a cropped image to its visible pixels, preserving ADR
  0120's rule that a download does not reveal what a crop hides.
- **The native whiteboard file is `.excalidraw`.** `mevedel-editable-1`
  remains for documents. Import applies Excalidraw's restore rules: legacy
  fields migrate, invisible and unknown elements are dropped, unusable values
  fall back to defaults. Unsupported image types become placeholders with a
  visible note.
- **The cylinder became a library item.** Excalidraw has no cylinder type.
  Keeping one as a private type would break files opened elsewhere, so the
  database shape is a built-in group of an ellipse and lines.
- **Element libraries belong to the host.** A directory of `.excalidrawlib`
  files, `mevedel-shared-library-directory`, holds the personal library and
  installed ones; mevedel ships a built-in library. They are offered to
  writable editors in every room and to the model. Excalidraw keeps its
  library in the browser, apart from scenes; a host directory gives the same
  separation while surviving browsers and sessions. Emacs fetches the public
  catalog from libraries.excalidraw.com because the editor's sandbox has no
  network access; it fetches only that catalog's library files. Items stay
  opaque JSON in Emacs; inserting one validates it like any other edit.
- **The model inserts library items by reference.** Reading
  `shared://library` lists items as `LIBRARY/ITEM-ID`, its `sheet.png` shows
  them numbered, and `SharedEdit` `insert`
  places one with fresh identities in the helper. Copying an item's elements
  into a patch would make the model handle ids, groups and bindings that the
  editor's insertion already remaps.
- **Shape recognition is an editor tool.** Excalidraw's moment-based
  recognizer turns a stroke drawn with the autoshape tool into a rectangle,
  diamond, ellipse, arrow or line, or keeps it as freedraw. The result is an
  ordinary element saved through the normal path, so Undo and collaboration
  need nothing new.

## Considered options

- **Translate only at the file boundary.** Keeping the private record and
  converting on import/export was the smallest change, but every Excalidraw
  feature the record lacked was lost on import, and libraries would have needed
  the same translation.
- **Embed Excalidraw's React editor.** It brings its whole editor, but not
  the layers mevedel depends on: it has no Yjs or host-authoritative mode, its
  export needs a browser so the host still needs its own renderer, it loads
  assets from a CDN that the editor's CSP forbids, and comments, area
  questions and previews would need re-integration on its API. The element
  model chosen here is also the first step if that ever changes.
- **Port Excalidraw's interaction in full.** Most of its specification
  describes tools, handles and snapping. Cherry-picking interaction (groups
  and entering them, locking, arrowheads, fonts, transform and point handles,
  arrow types) where it helped was cheaper than cloning it.

## Consequences

Existing whiteboards no longer open; there is no migration. The editor bundle
grows by the fonts and roughjs; the editor CSP gains `font-src data:`, so the
relay must be rebuilt and redeployed. Constant-width freedraw approximates
Excalidraw's laser-pointer stroke with perfect-freehand, and sticky notes omit
the lifted corner and date footer. Elbow arrows take a fixed orthogonal route
between the facing sides of their shapes instead of Excalidraw's A* router,
so they do not avoid other shapes.

## Decision history

The first version kept one personal library file, `mevedel-shared-library-file`,
and "Add all to my library" merged public libraries into it. In use, the user
wanted a few public libraries installed on the host and always available, as
separate collections. The library directory replaces the single file;
installing writes the library as its own file, which can also be removed
whole. Excalidraw's own libraries never travelled with scenes either, so
nothing was lost by keeping them off the board.
