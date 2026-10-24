---
name: "Check graphical rendering from a worktree without the live Emacs"
description: "Use `eask run script graphical-check` (since mevedel@982d9811) to render worktree view code in an isolated emacs -Q"
type: feedback
---

# Check graphical rendering from a worktree without the live Emacs

Since `mevedel@982d9811` (2026-10-24) the maintained command is
`npx @emacs-eask/cli run script graphical-check -- --grammar-dir
~/.emacs.d/.local/etc/tree-sitter`; see `docs/development.md` (graphical
check section). It isolates HOME, XDG and the native cache, checks for a
display and both Markdown grammars, and saves PNG/PDF output under
`.scratch/graphical-check/`. Prefer it over the hand-built recipe below.
Extend `test/graphical-check.el` for new fixtures instead of writing ad-hoc
drivers. It covers static rendering only, not hover or frame timing.

The historical recipe below is superseded where the command applies.
One correction from 2026-10-24: copying `font-lock-face` was a workaround.
Font Lock skips buffers whose names start with a space, so rename the fixture
buffer instead.

Batch ERT cannot show overlines, strike-throughs, `:extend` or face blends, and
loading worktree code into the user's running Emacs would replace modules that
other sessions are using. On 2026-10-19 (turn separators work) this worked:

- Run a separate `emacs -Q` (pgtk) with `HOME` and every `XDG_*_HOME` pointed at a
  fresh temporary directory; `test/helpers.el` refuses to load otherwise ("Tests
  require HOME and XDG roots under temporary-file-directory").
- Add `.eask/31.1/elpa/*`, the worktree and its `test/` to `load-path`, require
  `helpers`, build the transcript with `mevedel-view-test--with-buffers` /
  `mevedel-view-test--insert-data`, then `mevedel-view--full-rerender`.
- The fixture view buffer is not fontified, so copy `font-lock-face` to `face`
  before capturing; otherwise every face is missing from the image.
- Capture with `(x-export-frames frame 'png)` (needs a Cairo build), then
  `kill-emacs`. Wrap the load in `condition-case` that prints the error and
  exits, or an error leaves the window open and the command hangs.

The same `x-export-frames` call works in the live Emacs on a temporary frame for
read-only prototypes built from a copy of a view buffer's text.

**Why:** a CSS mock and batch tests both missed real rendering facts here: tool
rows inherit `default`'s background, and strike-through on an extended newline
reaches the window edge.
**How to apply:** use this for view presentation changes before reporting a
visual result. Keep probes and scripts under `.scratch/`. Bound live Eval
output while doing it; dumping text properties of a view row returned 20 MB.
