# Excalidraw-compliant whiteboard: handoff

Branch `excalidraw-model`. Implemented as recorded in
`docs/adr/0121-store-whiteboards-as-excalidraw-elements.md`; current behavior
is in `docs/shared-editing.md` (Editing and pointing, Element library,
Downloads and imports). Remaining parity items are in `docs/backlog.md`
(Whiteboard).

Follow-up (2026-10-02): host library directory with install/uninstall and
catalog sorting, model access to libraries (SharedRead library, SharedEdit
insert), transform and point handles, entering groups, curved and elbow
arrows, pen pressure from coalesced events, Style opening with a selection,
and the box-selection area shown only while drawn or attached.

Before use:
- Rebuild and redeploy the relay: the editor CSP gained `font-src data:`.
- Existing whiteboards no longer open (no migration, by design).

Checks run on 2026-10-01: `npm test` (41), `npm run test:browser` (59, incl.
the real-room scenario), full ERT (`eask run script test`, 0 unexpected),
`go test ./...`, byte compilation without warnings. Not run: remote acceptance
(`MEVEDEL_TEST_SHARED_EDITING=1 ./test/run-remote-acceptance.sh`).

References: `.scratch/excali-mode/docs/excalidraw-spec.md` (local clone of
https://github.com/yibie/excali-mode) and the sparse Excalidraw checkout in
`.scratch/excalidraw-src/excalidraw` (fonts come from there via
`shared-editing/fonts.py`).
