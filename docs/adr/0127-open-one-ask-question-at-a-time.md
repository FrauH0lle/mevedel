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

## Consequences

- A closed question and its answer are flattened and cut to one line, the
  answer keeping at least half of it; the full text is in the help echo and
  appears when the question opens. The cut is computed at repaint, so after a
  resize it may be too long or too short until the next repaint. Below about 25
  columns each part keeps a 10-column minimum and the line wraps.
- Digits and `c` act only in the open question.
