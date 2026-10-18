# Keep the composer still

Status: accepted

## Current decision

The view keeps the composer on its screen row. Chrome above it (the
background-work panel, tasks, interaction prompts) changes height while the user
types; a window that shows the composer keeps the composer's row across every
zone mutation and re-render, so that growth scrolls the transcript instead, as in
a bottom-anchored terminal. While everything from the window start to the end of
the draft still fits, the start is kept and free space below is used first; a
window scrolled away from the composer, or starting inside a tall draft, keeps
its own reading position. Both zone mutations and full re-renders capture the
number of characters from each window start to the buffer end. After restoring
logical positions and composer anchors, a smaller tail permits clamping a start
past the last page backward to that page. This fills empty space left below the
draft when an interaction disappears, even if the composer was previously
offscreen or near the top and the start was in surviving transcript. Unchanged
or growing tails retain deliberate blank space; earlier history views do not
move toward the bottom, and a window already starting at `point-min` stays
there. View buffers set `scroll-conservatively` to 101 and `scroll-margin` to 0
so a draft growing past the bottom scrolls one line.

The line next to the composer never appears or disappears: one permanent rail
carries the mode badge, an actionable pending count with `C-c C-j`, and the
turn's activity, replacing the separate progress row and the badge line.

Arriving permission prompts and questions do not move point. They render, the
rail shows them as pending, and `C-c C-j` visits the next one from the invoking
window, which returns to its draft position on settlement only if it is still
visiting that prompt.

Interaction cards stack in arrival order, the newest farthest from the
composer; only the pending-input card, which the composer itself queues, stays
pinned next to it. No other producer, patch review included, carries a type
priority; arrival alone orders them. A card arriving while another is pending
therefore moves only the transcript above it, never a card the user may be
reading or answering. `C-c C-j` starts at the card nearest the composer, the
one that has waited longest, and repeated presses cycle outward. In a window
not yet full, where free space is used first, an arrival pushes the cards
below it down like any other growth above the composer.

Closing an agent inspection's side window rewraps the parent's text without
any zone change. A parent window gets the composer's screen row from before
the inspection opened back only if, just before the close, it still shows the
composer with point in it. A window the user moved into history is left alone,
and a same-window inspection always returns to the parent at that row. Reflow
caused by the close itself does not make a window eligible.

Background-work rows are in stable start order, newest farthest from the
composer; state changes, including blocking, update rows in place. While point
or the mouse is on the panel, it keeps exactly the rows it showed: finished rows
stay as inert dimmed lines and new work is only counted, until both leave.
A finished row also lingers for ten seconds after the pointer is away, and
leaving the panel restarts that period, so removals happen when nobody is
aiming at the panel.

Panel heights depend only on which rows exist, not on their text. Every row,
finished line, preview line and rule fits one screen row of the narrowest
window showing the view, and re-renders when that width changes. An open
execution preview is always three lines. Expanded controls pack whole buttons
into explicit fitted rows, using short labels only when a button cannot fit
alone. The live layout sets the completion budget: an execution reserves its
Stop slot as inert space, and a held finished agent replaces its actions with
a padded finished notice. Model details are fitted in both states. Thus a
finished expansion keeps its displayed height, not just its logical line count.

## Rationale and consequences

Measured in a terminal Emacs (100x30) with the composer on the last row: the
previous top anchoring recentred the composer by thirteen rows when the first
status row appeared, then moved it a row on every later change. Composer
anchoring with `scroll-conservatively` kept it on its row through additions,
removals and a typed newline. Claude Code and Codex both anchor their prompt at
the bottom; a Claude Code and a Codex consultation reached the same conclusion.

Status stays above the composer. Status after the composer would need an
editable input-end boundary in place of roughly forty composer `point-max` uses
and, with the composer anchored low, reserved space or its rows fall off the
window. Display-only after-strings were rejected because rows must be real text
that point can reach and activate.

With the composer anchored, only text above a change moves, so new rows go to the
far edge and existing rows keep their place. Reordering (previously "blocked
first") and removals under the pointer still moved targets; the hold on hover
addresses both without a timed lock, whose expiry could move a target while the
user acts. The hold uses a short timer only while rows are held, to notice that
the pointer left.

Automatic focus let a keystroke typed into the draft land on a newly arrived
approval's RET action. An idle delay (as in Codex) only narrows that window, so
focus is explicit.

## Decision history

- **Narrow expanded controls (2026-10).** A 47-column display wrapped live
  execution controls onto two rows but finished controls onto one; expanded
  agents had the same gap, and their model metadata was unbounded. Holding
  row identities alone did not protect mouse targets. Explicit control-row
  layout now keeps every action and reserves the live height on completion;
  truncation applies to metadata, never to the set of actions. Packing whole
  buttons can use an extra row in a narrow window instead of allowing a
  button's label to wrap. A compiled hot status-render fixture with expanded
  agent and execution controls remained around 0.40 ms before and after the
  change, at both normal and narrow widths; differences were within run noise.

- **One-row panel lines and constant-height expansions (2026-10).** Holding
  the row set did not hold the height. Measured in a 100-column graphical
  window with the composer fixed at row 40:
  - An open inspector grew from 11 to 15 rows as output arrived, moving its own
    row from row 32 to 28.
  - A two-row agent row became a one-row finished line, so the panel went from
    8 to 7 rows.
  - An expanded agent that finished lost its four detail rows under a pointer
    on `[Interrupt]`.
  - Separator rules sized for the widest window took three rows in a 48-column
    split, then shrank at the next unrelated status redraw (6 to 4 rows).

  Fitting every line to the narrowest window and fixing the preview and agent
  expansion heights keeps all four constant (12, 7, 10 and 4 rows). The extra
  layout work costs about 21 µs per panel render.

- **Arrival order for interaction cards (2026-10).** Cards were ordered by
  kind: patch preview, Plan, Ask and requests, permissions, pending input,
  then notices. With the composer anchored, every card ranked below an
  existing one was inserted between it and the composer and pushed it up.
  Measured in a graphical 42-row view, a 10-line permission card arriving
  while the user was in an Ask moved the form and the cursor up 9 rows; a
  sub-agent permission, a control-transfer request, or the permission
  expiry notice can arrive this way at any moment. No kind needed a fixed
  place: each card is answered on its own, and only pending input belongs
  to the composer. Arrival order applies the principle the background panel
  already follows, new rows at the far edge, at no rendering cost.
  `C-c C-j` used to start at the top card; it now starts at the one nearest
  the composer, which in arrival order has waited longest and needs the
  least scrolling from the draft.

- **Rail refresh outside redisplay (2026-10).** The rail was refitted from one
  handler on both `window-size-change-functions` and `window-scroll-functions`.
  The scroll hook runs inside the window's redisplay, which restores point to its
  earlier buffer position afterwards, so a rail text change there shifted the
  draft cursor. A GUI reproduction crossed the elapsed label from `9s` to `10s`
  before the once-a-second tick and typed RET then `x` at the window bottom.
  The redisplay scroll rendered `10s`, point fell back one character, and `x`
  joined the previous line. Only the size hook now refits the rail; it runs
  before windows redisplay. Scrolling still reschedules animation. A compiled
  probe put the scroll hook at 0.21 µs per call, down from 5.8 µs for the
  unchanged-rail check alone.

- **Last-page clamp after shrink (2026-10).** Preserving a screen row alone
  preserved empty space too: submitting a question from a scrolled viewport
  left the composer at row 5 in a 41-row window. When the composer had been
  offscreen, deleting the interaction collapsed the saved start marker to the
  rail and left only the rail and composer at the top. An initial unconditional
  backward clamp fixed these views but broke six reader-position regressions:
  pinned prompts, history readers, permission starts, and the parent viewport
  after inspecting an agent transcript all legitimately retained blank space.
  Restricting the clamp to starts lost by fragment removal or truncation fixed
  those regressions but missed another real case: a split window starting in
  surviving transcript just above the Ask card, with the composer offscreen,
  retained only lines 79--90 in a 39-row window after submission. The current
  net-tail shrink gate covers both cases without coupling viewport policy to
  fragment identity. Zone reconciliation and full re-renders capture tail
  lengths before mutation and clamp after logical and composer restoration.
  A truncation regression established that clamping before logical restoration
  lets the latter undo the correction. The six original reader assertions
  remain unchanged.

  One backward `vertical-motion` computes the last page only when a restored
  tail shrank; starts at `point-min` skip it. Equal-length reflow alone does not
  trigger this character-count gate. Cached `window-end` cannot safely gate
  this calculation because mutations and `set-window-start` leave it stale.
  Visual spinner frames only change display properties and do not traverse
  this path. A separate graphical probe with wrapped text and a 1.5-height face
  kept the buffer end visible and the composer at row 33 in a 34-row window
  after submission.

- **Grace period for finished rows (2026-10).** Finished rows first left the
  panel at once, since the transcript notice takes over. The user asked for a
  short grace period, as Claude Code keeps finished agents for 30 s, restarted
  whenever the pointer leaves the panel. Ten seconds was chosen: the notice is
  the durable record, so the panel only needs to show what just ended at a glance.

The original terminal benchmark measured anchoring at about 41 µs per displayed
window per mutation (compiled, 20-row window), dominated by `vertical-motion`;
the rail's no-op render about 7 µs and its once-per-second elapsed update about
41 µs. Those measurements predate the last-page clamp. Visual spinner frames
are unchanged.

A compiled graphical probe of the net-tail rule (20-row window, medians of
seven batches of 10,000 calls) measured unchanged-tail capture/check at 0.32 µs
and a forced shrink calculation at 115 µs. Anchoring alone and anchoring plus
the unchanged-tail check both measured about 382 µs; their difference was below
the probe's noise. These graphical timings are not comparable to the earlier
terminal baseline. Ordinary no-op and growing-tail updates do not pay for the
last-page display-row calculation.
