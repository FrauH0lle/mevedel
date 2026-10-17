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
its own reading position. View buffers set `scroll-conservatively` to 101 and
`scroll-margin` to 0 so a draft growing past the bottom scrolls one line.

The line next to the composer never appears or disappears: one permanent rail
carries the mode badge, an actionable pending count with `C-c C-j`, and the
turn's activity, replacing the separate progress row and the badge line.

Arriving permission prompts and questions do not move point. They render, the
rail shows them as pending, and `C-c C-j` visits the next one from the invoking
window, which returns to its draft position on settlement only if it is still
visiting that prompt.

Background-work rows are in stable start order, newest farthest from the
composer; state changes, including blocking, update rows in place. While point
or the mouse is on the panel, it keeps exactly the rows it showed: finished rows
stay as inert dimmed lines and new work is only counted, until both leave.
A finished row also lingers for ten seconds after the pointer is away, and
leaving the panel restarts that period, so removals happen when nobody is
aiming at the panel.

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

- **Grace period for finished rows (2026-10).** Finished rows first left the
  panel at once, since the transcript notice takes over. The user asked for a
  short grace period, as Claude Code keeps finished agents for 30 s, restarted
  whenever the pointer leaves the panel. Ten seconds was chosen: the notice is
  the durable record, so the panel only needs to show what just ended at a glance.

Anchoring costs about 41 µs per displayed window per mutation (compiled, 20-row
window), dominated by `vertical-motion`; the rail's no-op render about 7 µs and
its once-per-second elapsed update about 41 µs; spinner ticks are unchanged.
Graphical variable-height content was not measured separately.
