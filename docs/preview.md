# ApplyPatch review

Entry point: `mevedel-patch-review-start` (`mevedel-patch-review.el`, over the patch engine in `mevedel-tool-patch.el`). The model submits one Codex-style
`ApplyPatch` payload containing any number of Add, Update, Delete, and Move
operations. The pipeline authorizes and snapshots every affected path before
the proposal reaches this UI. An allowed-root patch in `ask` mode goes directly
to review; protected or outside-root paths first show the resource-authority
prompt.

In `ask` mode, the view interaction zone shows one aggregate review:

```
ApplyPatch · 5 files · 6/9 changes · +69 −26 · 1 comment

▼ ◐ M mevedel-tool-fs.el · 2/3 · +7 −3
    ✓ @@ mevedel-tool-fs--edit-handler · +5 −2
      620   │           (or (mevedel-session-plan-mode session)
      622 - │               (and mevedel--current-request
      622 + │               (and (boundp 'mevedel--current-request)
    ✗ @@ legacy helper · +2 −1
▶ ✓ A mevedel-tool-patch.el · +412
▶ ✗ R old.el → new.el · +6 −0

Keys: TAB fold · SPC select · RET visit · e edit · f feedback · ...
[ Apply 6 changes in 4 files ]   [ Reject all ]
```

Fold state (`▼`/`▶`) and selection state (`✓` all, `◐` partial, `✗` none)
are independent glyph columns. Hunk rows are labeled with the hunk's `@@`
context, falling back to a diff-style section heuristic (the closest
unindented baseline line at or above the hunk), then `~N`, the baseline
match line. Diff lines carry a gutter — old-file line numbers for context
and `-` lines, new-file numbers for `+` lines, computed per review by
`mevedel-tool-patch-annotate-line-numbers` against the captured baseline —
plus whole-line background tints (`mevedel-patch-review-added/-removed`,
which inherit `diff-added`/`diff-removed`, or magit's diff faces once
`magit-diff` is loaded). Deselected changes render dimmed without tint.
Every tally (header, file rows, primary button) counts selected changes
only and updates live.

Every file starts folded. Keys, active anywhere in the review body:

- `TAB` folds a file, or a single hunk on a hunk row
- `SPC` toggles selection; on an Update file row it toggles every hunk
- `RET` visits the affected file, at the hunk's baseline location
- `e` edits the staged change before it is applied: an Update or Move hunk
  opens in a temporary `diff-mode` buffer, an Add file's proposed content
  opens in a buffer with the target's major mode (`C-c C-c` commits the
  revision after re-validation, `C-c C-k` discards it). A Delete and a
  pure rename have no content to edit; their refusal points at `SPC`,
  which deselects the operation and so keeps the file (or its path)
- `f` attaches multiline feedback (`✎`): on a hunk, on a file row the whole
  file, anywhere else the whole patch
- `n` / `p` move between file and hunk rows
- `C-c C-c` submits, `C-c C-k` rejects everything

Hunk feedback and feedback on indivisible Add/Delete/Move operations
deselect their target; file-level feedback on Update files and whole-patch
feedback leave selection untouched, so comments can ride along with an
apply. An active feedback editor stays rendered while its file or hunk is
folded. Add, Delete, and Move operations remain indivisible selection
units. The primary action states its effect — `Apply N changes in M files`,
plus `· send K comments` when feedback is staged, `Request revision · K
comments` when only feedback is staged, or `Reject patch`. Re-selecting a
change with feedback asks before clearing that feedback. Selected changes
are validated again against the captured baseline at submission; a stale
path writes nothing and leaves the review open with a conflict message plus
a recovery hint (deselect the stale file, or reject so the model re-reads).
An incomplete rollback instead shows sanitized authored paths and tells the
user to inspect them before retrying.

A user-revised change is marked `· edited` in the review, and the tool
result the model receives marks each one `User edited during review
(authoritative)` with an explicit do-not-revert directive, plus a
`(N revised by the user during review)` suffix on the applied-patch
header. Submitting a revised patch also queues the one-shot
`user-revised-patch` system reminder for the next turn, so the model does
not treat the divergence from its own proposal as an anomaly to fix.
Once submission starts, the review becomes an inert progress row on both the
Emacs and collaboration surfaces until diagnostics finish.  Review teardown
may still abort it, and whichever terminal result wins settles the tool once.

No file changes before final submission. Application is one rollback-backed
transaction across the selected changes, creates parent directories for added
files, and refreshes visited unmodified buffers after success. If restoration
itself fails, the tool reports a distinct incomplete-rollback error containing
the original failure and every path it could not restore. `edits`,
`full-auto`, and a direct allow rule covering every affected path skip the
interactive review but use the same validation and transaction.

## Parse leniencies

The parser accepts everything Codex's grammar-constrained `.lark` emits and
mirrors Codex's hand-written parser leniencies on top: structural markers
match after trimming surrounding whitespace (update-body markers tolerate
trailing whitespace only, so an indented header inside an update stays a
context line), one `*** Environment ID:` line after `*** Begin Patch` is
skipped, and a bare empty line inside an update body is an empty context
line whose space marker the model dropped. Deliberate divergences: a pure
rename (Update + Move with no hunks) is allowed where Codex rejects it, and
a hunk without any `+`/`-` line is rejected as a no-op.

## Matching fallbacks

Update hunks match through the full Codex ladder in decreasing strictness:
exact (after line-ending normalization), ignoring trailing whitespace,
ignoring surrounding whitespace, then additionally folding typographic
Unicode punctuation to ASCII (dashes, curly quotes, exotic spaces — the
same table as Codex). An `*** End of File` hunk is first anchored to the
end of the file, then retried unanchored. Two deliberate divergences from
Codex: every pass still requires a unique match (ambiguity is rejected,
never first-match-wins), and application preserves the file's context lines
verbatim — a fuzzy match only decides where the hunk lands and what gets
deleted, so ASCII-fied or re-indented context in the patch never rewrites
untouched lines. Hunks arrive in file order, so an otherwise ambiguous
match is retried among the candidates at or after the previous hunk's
position before being rejected; deselected hunks still advance that
cursor at apply time, keeping the preview's disambiguation window and
the application's identical. Updated and moved files retain their detected
coding system, and files with CRLF line endings are written back with CRLF.
Fuzzy matches are surfaced: the winning pass is recorded
on the hunk, the review row gains a warning-face `· fuzzy` suffix with the
pass in its help-echo, and applied fuzzy hunks add a `Fuzzy: FILE hunk N
matched while ...` line to the model-visible result and the transcript
notes.

## Result contract

The handler returns one ordinary tool result envelope with `:kind patch`
render-data, including applied/total change counts, the number of sent
comments, and rejected/feedback note lines. The persisted transcript keeps
that one ApplyPatch event, headed `ApplyPatch: 4 files · 6/9 changes ·
1 comment sent (+69 -26)` — the view's `Name: arg (+N -M)` summary grammar,
which colors the count group — and renders its selected changes as per-file diff
blocks with aggregate counts followed by the notes, so a revision-only
settlement stays expandable. The body is self-fontified with the review's
faces (no `:body-mode`), keeping status letters, counts, and diff lines
colored in the transcript; diff lines are stamped `mevedel-view-no-linkify`
so the view's Markdown pass never buttonizes paths or rewrites links inside
verbatim patch content, while the per-file header lines stay clickable. Its
model-visible text reports every applied, rejected, modified, and
feedback-bearing change. Rejecting everything without feedback returns an
error; feedback is a successful revision request so the model can continue.
