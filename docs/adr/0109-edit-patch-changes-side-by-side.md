# Edit patch changes side by side

Status: accepted

Patch review has one editor for a staged change: an ediff session comparing the
captured baseline against the planned result. Every operation kind uses it —
Update and Move compare baseline to the hunk-applied result, Add compares an
empty buffer to the proposed content, Delete compares the baseline to an empty
buffer. Committing an Update or Move re-derives its hunks by diffing the edited
result against `:baseline-content`. The `diff-mode` hunk editor is gone.

The editor it replaces asked the user to write diff syntax. Every line of a hunk
buffer had to begin with a space, `+`, or `-`, because that is all
`mevedel-tool-patch-parse-update-lines` accepts. Fixing a character inside an
existing `+` line was fine; adding a line was a trap. An added line of indented
code starts with whitespace, so the leading space was consumed as the context
marker and the rest became "this unchanged line must already exist in the
file" — caught at commit by `mevedel-tool-patch--match-start` and reported as a
hunk that does not match, which names neither the missing `+` nor the line. An
editor whose most common operation has a silent failure mode reported as
something else is not worth keeping alongside one that has no markers at all.

Side by side also turns out to be the better editor for a Delete, which is the
case that first looked degenerate. ediff's copy-difference commands make
"delete this file but keep these parts" a keystroke per kept region. The
alternative under the old editor was retyping the file into an empty buffer,
because a Delete carries no `:content` to prefill.

Two facts moved the cost estimate enough to decide the shape. Deriving hunks
from an edited whole file initially looked like it had to consume hunk
selection, since the planned result is built by `mevedel-tool-patch-apply-hunks`,
which skips deselected hunks — so a deselected change is absent from the result
and absent from the regenerated diff. Reading `--match-start` showed its
`after` argument is only an ambiguity tiebreaker and not a positional floor, so
deselected hunks can be carried across an edit unchanged and merged back into
the regenerated set: `:baseline-content` never moves, both sets position against
it, and the merged list sorts into file order. Selection stays a reversible
toggle instead of ediff being a one-way door. Second, matching happens against
the exact text the diff was computed from, not against a drifted file, so
machine-generated `-U3` context is sufficient and there is no argument for
trading review legibility for wider context.

The rejected alternatives:

- **A second key beside the existing editor.** Two editors for one job, and the
  one with the marker trap stays reachable. The trap was the reason to build
  the other one.
- **ediff per hunk**, comparing a hunk's old lines to its new lines. It
  preserves hunk granularity for free, but shows nothing around the change,
  which is the whole reason to want a side-by-side view for a large patch.
- **Committing the edit as one whole-file hunk** — every baseline line removed
  and every edited line added, which is how converting a Delete into keeping
  the file used to be represented. No derivation code at all, at the price of a
  review that displays the entire file as a single hunk with no per-hunk
  selection left.
- **ediff's file-patching job**, which the deleted implementation used
  (`35e57b4`). It operates on real files, needs stub files and
  `diff-find-file-name`, grew session-group machinery for the multi-file case,
  and finished by renaming files over each other. Buffers against the captured
  baseline need none of it.

Consequences:

- Hunk identity is not stable across an edit. The revision report names the
  operation ("whole file revised") rather than hunk indices, which after
  regeneration no longer correspond to anything the model wrote.
- A deselected hunk survives an edit and stays reselectable, but reselecting it
  can fail if the edit touched its region. That surfaces as the ordinary
  "Patch hunk does not match" error and a rollback, which does not say the user
  edited around it.
- Hunk feedback needs no confirmation before an edit, which the design did not
  expect. Feedback deselects the hunk it is attached to, and a deselected hunk
  is exactly what an edit carries across untouched — so the set of feedback an
  edit could destroy is always empty. Only an Add or a Delete still confirms,
  because adopting one selects the operation and a selected operation carries
  no feedback. If hunk feedback ever stops deselecting its hunk, this
  confirmation has to come back at file scope.
- The review stays interactive while a session is open, so adoption
  re-derives the planned result and refuses when it no longer matches the one
  the session started from. Without that check, deselecting a hunk in the
  review while editing would reinstate it: the edited result still contains
  it, and every derived hunk is adopted as selected.
- A hunk cannot record whether a file ends with a newline, so both sides of a
  derivation are reduced to logical lines and given one. A trailing-newline
  difference therefore derives nothing rather than a hunk whose replacement
  re-joins to the text it claims to change.
- An ediff session is refused when both sides hold the same lines. ediff
  builds a zero-difference session and only fails on the first command typed
  into it
  (`ediff-diff.el`, `ediff-NO-DIFFERENCES`). The check also covers a pure
  rename and a file with nothing selected, and compares logical lines because
  applying hunks renormalizes line endings.
- Matching context for a regenerated hunk is machine-generated. Hand-tuning a
  hunk's context is no longer possible, and no longer meaningful: the context is
  derived from the baseline the hunk is matched against.
- `mevedel-tool-patch.el` owns the content-to-hunks derivation, so it is
  testable from two strings with no UI, and available to the deferred
  cumulative turn-diff view.
- One editor at a time, per session. The deleted implementation's concurrency
  machinery is not re-created; a second request is refused.

Reversing this means re-teaching users diff syntax, so the marker trap is the
thing to re-examine first if it is ever revisited. ADR 0092 keeps ApplyPatch as
the single file-mutation tool; this decision keeps its review surface to a
single editor for the same reason.
