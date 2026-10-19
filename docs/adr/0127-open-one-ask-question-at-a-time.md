# Open One Ask Question At A Time

Status: accepted

## Current decision

An Ask questionnaire shows every question, but only one is open: its full
text, options, and sample panel. Every other question is a single line,
answered ones ending in `→ answer`. The first question starts open. Answering
closes it and opens the next unanswered question, or moves to Submit when none
is left. `RET` or a click on a closed question, or `TAB`/`S-TAB`, opens one.
Moving point never opens or closes anything, so cursor motion cannot change
the form's layout.

The form also keeps one height while it is pending. It reserves the screen rows
its questions need with whichever question is open, or none, and fills the
difference with blank lines above Submit. The questions stay anchored at the
top of the form and Submit and the key hints at its bottom; opening or
answering a question changes neither the form's height nor anything outside it.

The browser applies the same one-open-question and fixed-height rules. It
keeps presentation state local and sends all answers atomically through the
existing host questionnaire descriptor. Previous/Next controls and clickable
summaries navigate questions; Tab and Shift-Tab keep the browser's ordinary
focus traversal so every option, sample action, and text input stays keyboard
accessible. Both navigation controls retain their slots when disabled. The
browser measures the tallest open panel once per width, not while typing or
moving focus, and leaves unchanged host replays intact.

## Rationale

With all questions expanded, a form with several sample-bearing questions
pushed later questions and Submit off screen, and the reader had to find the
question they were answering among the others' options. Showing one open
question gives that focus while the one-line rest keeps the overview: which
questions exist, which are answered and how.

Claude Code shows one question per tab with a separate review page. In an
Emacs buffer a tab bar truncates in narrow windows, hides the other questions'
text, and needs a review page that the collapsed answers here already are. A
guest viewing a shared session also receives the whole form, which tabs would
turn into per-question state to synchronise.

Opening only on explicit request follows the form's rule that motion never
repaints: `n`/`p` across closed questions stay as cheap as any other cursor
movement, and nothing moves under the reader.

The fixed height answers live testing: with only the open question at full
height, opening a question with an eleven-row sample after one with two options
changed the form by fifteen rows. With the composer anchored on its screen row
([ADR 0126](0126-keep-the-composer-still.md)), every such change moved the
whole form and the transcript above it.

## Consequences

- A closed question and its answer are flattened and cut to one line, the
  answer keeping at least half of it; the full text is in the help echo and
  appears when the question opens. The cut fits the narrowest window showing
  the view, and the form lays itself out again when that width changes. It
  used to wait for the next repaint, so the first keystroke after a resize
  changed the form's height: after a split from 150 to 70 terminal columns,
  the form went from 32 to 31 rows on the next `n`. Below about 25 columns
  each part keeps a 10-column minimum and the line wraps.
- Digits and `c` act only in the open question.
- A short open question leaves blank space above Submit, up to the height of
  the tallest open question.
- Reserved rows are computed by following `word-wrap` over the rendered
  questions at the width a wrapped row holds, once per width. Without a right
  fringe, as on a terminal, the last column carries the continuation glyph, so
  that width is one less than the window's. Counting the full width let a row
  that exactly filled a 100-column terminal count as one row while it wrapped
  to two: with two such option descriptions in one question, opening it made
  the form 2 rows taller than any other question, measured as 19 against 17
  rows. Across realistic descriptions at widths 50-160, about 4% of wrapped
  lines change their row count between the two widths.
