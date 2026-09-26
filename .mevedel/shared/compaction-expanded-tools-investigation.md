# Compaction and expanded tool rows (2026-09-27)

Source: investigation of the two `docs/backlog.md` inbox items in this task.

- Starting root compaction changes only the progress row; the focused regression in `test/test-mevedel-compact-target.el` keeps two expanded Read results and a multiline draft intact at start.
- Successful root compaction rotates source coordinates. A reproduced retained-tail Read row collapsed on the ensuing full redraw; an archived Read row correctly disappeared. The implemented fix rekeys recorded tool disclosure state by surviving tool ID before that redraw (`mevedel-compact-target.el`, `mevedel-view-disclosure.el`).
- Streamed response tables could be formatted by the idle pass before the response ended, then return to raw on the next chunk. The live table now remains raw, and terminal reconciliation allows the existing idle formatter to format it once (`mevedel-view-render.el`, `mevedel-view-table.el`).
- Verification: focused view tests 422/422; focused compaction tests 56/56 reported by the investigation agent; final isolated full ERT suite 8,639 cases, zero unexpected, 22 skipped; fresh Eask compilation 208 files without warnings.
