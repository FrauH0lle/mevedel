# Keep the composer still

Status: accepted

## Current decision

The view keeps the user's active surface at its screen position. Zone reconciliation
and its enclosing full-render transaction
chooses one viewport anchor per window: point inside a surviving visited
interaction card takes priority; otherwise a previously visible composer keeps
its position regardless of point. Logical positions are restored before applying
that anchor. A visited card retains its screen position unless its shrink creates
new blank space below the buffer; repairing only that excess takes priority over
the visited row. Existing blank space is retained.

Chrome above the composer (background work, tasks and interaction prompts)
changes height while the user types. Composer anchoring scrolls the transcript
instead, as in a bottom-anchored terminal. While everything from the window start
through the draft fits, growth uses free space below before scrolling. Growth
that overflows the window uses the earlier of the saved composer viewport and
the last full page, consuming remaining free space even in a single large burst.
This comparison includes partial vertical scroll. A window
scrolled away from the composer, or starting inside a tall draft, keeps its own
reading position.

Zone reconciliation and full re-renders also capture each logical window start
and its character tail. After logical restoration, they apply this policy:

| Window context before mutation | Viewport policy | Discriminating case |
| --- | --- | --- |
| Point visits a surviving interaction card | Retain the visited row, then fill only newly created blank pixels | Shrinking a visited card below point must not leave a newly empty viewport |
| Composer visible, saved logical start survives | Retain the composer position and pre-existing blank space, except when growth overflows the window | Shorter status text must not erase intentional blank space; oversized incoming growth fills the last page |
| Composer visible, saved logical start removed or truncated | Fill the last page, bypassing the character-tail guard | Ask submission can expose longer history lines and increase the tail despite losing rows |
| Neither a composer nor visited-card anchor | Clamp backward only when the character tail shrank | An offscreen composer returns to a full last page after an interaction disappears |

Earlier history views do not move toward the bottom, and shrink repair leaves a
window already starting at `point-min` there. Strict logical-anchor resolution distinguishes
a surviving row from one removed with its fragment or truncated within it.

A genuinely unchanged zone does not capture or restore a viewport. It first
compares the rendered text including properties, verifies complete live-region
ownership, and checks for stale namespace text outside the region. Property-only
changes and stale text still take the ordinary reconciliation path.

Anchors measure body-relative pixels through Emacs's native visibility query,
not a row loop. Native backward display traversal restores the window start and
partial vertical scroll, accounting for remapped faces, wrapping and images.
Measurements are valid before redisplay, when the cached window end is stale.
Terminal geometry uses the same API in character-cell units. Batch policy tests
model only the visibility seam; native graphical tests establish pixel geometry.

Draft commands capture blank pixels before their first mutation and repair only
newly created blank space after the command. Insertion-before-deletion commands
must capture before insertion too: a temporary extra line is not the original
viewport. Ordinary `self-insert-command` skips capture when overwrite and auto-fill
are disabled: it can only grow the draft. A short plain deletion also skips
capture when the entire logical line fits every displaying window. Newlines,
wrapping, remapping, display/face properties and non-ASCII text take the exact
path. Graphical windows also require a monospaced default font; proportional
fonts use exact geometry. Other deletions, including multiline
delete-selection before typing, and all other insertion-first commands capture.
Commandless mutations repair immediately. Prompt, history and saved-draft
replacement and folded prompt submission are single mutations: neither a temporary
empty draft nor a user echo above the uncleared draft is a viewport state to
restore. Backward traversal runs only when blank pixels actually increased. View
buffers set `scroll-conservatively` to 101 and `scroll-margin` to 0 so a draft
growing past the bottom scrolls one line.

Height-only window growth repairs newly exposed blank space using the height
delta, provided the same buffer and width were displayed previously. This
inference covers popup closure with an unchanged start, not arbitrary simultaneous
scrolling, reflow or buffer replacement. The buffer-local window-size-change hook
requires no continuous geometry recording. Text scaling uses one guarded advice
around `text-scale-mode`: capture actual blank pixels before face remapping and
fill only the new excess afterward. Other buffers take the direct path. Both
repairs preserve deliberate pre-existing blank space.

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

- **Compare before capturing (2026-10-19).** An unchanged zone still spent
  0.60--0.99 milliseconds capturing composer geometry, even though its mutation
  stage already checked text equality. Rendering and checking before capture
  reduces a compiled unchanged reconcile to 4.8--5.0 microseconds in matched
  GUI/TTY plain and wrapped-history fixtures (three batches of 300 calls each).
  Text, character tick, point, buffer end, window start and partial scroll remain
  unchanged across all 900 calls per layout. The fixture includes the
  `fontified` property added by redisplay: genuinely changed properties are not
  ignored to obtain a no-op. This is a synthetic unchanged-zone measurement,
  not a claim that every production redraw is unchanged.

- **Overflowing bursts consume free space (2026-10-19).** A short transcript
  followed by more than a window of incoming output retained the composer's old
  high position and left blank space below the draft. The regression reproduces
  this through the render preserver with both single-line and multiline drafts;
  small growth remains a keep-start control. When growth no longer fits from the
  current start, restoration now chooses the earlier of the composer anchor and
  last-page viewport. Only that overflow branch computes the extra last page;
  shrinkage still preserves deliberate blanks. Native pixel traversal retains
  the final draft line's height and compares partial vertical scroll as well as
  buffer positions.

- **Draft commands are one mutation (2026-10-18).** Per-change repair moved a
  five-line replacement before its insertion half, consuming five deliberate
  blank rows. Deferring repair until `post-command-hook` fixes that ordering,
  but capturing at the first deletion is insufficient: `transpose-lines`
  inserts first. A native replay captured EOF at pixel 525 after that temporary
  insertion, then repaired the original pixel 504 position to 525 after deletion,
  consuming a row although the command changed no height. Capture now starts at
  the first mutation, and the repair compares actual blank pixels, including the
  final line's height. The first short-line gate was removed: besides missing
  insertion-first commands, its `window-max-chars-per-line` call alone cost
  639--647 microseconds in quiet graphical plain/wrapped-history probes. Its
  replacement uses cheap body width and conservative line-local checks only
  for deletions; insertion-first transactions still capture at their first edit.

- **Lost starts still fill the last page (2026-10-18).** Exempting every visible
  composer from the shrink clamp regressed the original Ask submission: a
  recentered submission left roughly eighteen blank rows. Comparing numeric
  start movement was insufficient, and the character-tail guard could reject a
  real row loss: an 83-character tail became 98 characters after anchoring
  exposed longer history lines. Strict anchor resolution now distinguishes a
  surviving row from a removed or truncated one; a lost start bypasses that
  character guard. A surviving composer anchor still preserves deliberate
  blanks. A visited card that shrinks below point repairs only newly created
  blank pixels rather than retaining an otherwise empty viewport.

- **Native pixel anchors and event-local geometry repair (2026-10-18).** Counting
  rows loses image height, remapped glyph height and partial scrolling. Native
  geometry replaces the production row loop rather than adding a second path.
  Popup closure in a graphical probe enlarged the body by 160 pixels and created
  exactly 160 new blank pixels while retaining the start and content height;
  subtracting the height delta recovered the previous 98 blank pixels. Text
  scaling from +2 to 0 created 216 blank pixels without running a size-change
  hook; a row-count approximation predicted 240, so scaling captures actual
  geometry at its own event. A folded 20-line submission previously consumed
  three deliberate blank rows because deletion captured the end after the folded
  echo was inserted. Submission now repairs net shrink from before that insertion.

- **One viewport anchor per window (2026-10-18).** Logical window-start
  restoration previously overrode the composer's row inside growing cards,
  while composer anchoring displaced visited controls when unrelated cards
  arrived or disappeared. A graphical replay kept a visited Ask control at row
  3 through arrivals above and below it and removal after choosing the visited
  row first. Only visiting windows pay for the additional bounded row capture.

- **Preserve intentional blank space, repair draft shrink (2026-10-18).** A
  recentered transcript with a visible composer jumped 24 rows in a 41-row
  terminal window when a streaming status summary became shorter in characters
  but not in display rows. The character-count gate alone cannot distinguish
  this from removed content. A captured composer row already carries the needed
  viewport intent, so full re-renders initially exempted those windows from the
  fallback rather than adding an end-of-buffer geometry scan to every render.
  The lost-start refinement above supersedes that blanket exemption. Separately,
  deleting five draft lines left five new blank rows; clearing a tall draft could
  leave almost the entire window empty. The first repair captured and restored
  the old end row, bounded by one window; pixel geometry and first-mutation
  capture now supersede that representation. Replacing a draft runs this repair once,
  after insertion, so navigating to a shorter history entry does not scroll for
  the temporary empty draft. These paths preserve pre-existing blank space
  instead of consuming it.

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
  retained only lines 79--90 in a 39-row window after submission. The subsequent
  net-tail shrink gate covered both cases without coupling viewport policy to
  fragment identity. Zone reconciliation and full re-renders capture tail
  lengths before mutation and clamp after logical and composer restoration.
  A truncation regression established that clamping before logical restoration
  lets the latter undo the correction. The six original reader assertions
  remain unchanged.

  At that stage, one backward `vertical-motion` computed the last page when a restored
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

The revised first-mutation transaction was measured compiled in Emacs 31.1,
using real keyboard commands and post-command hooks (60 observations per command
and layout). Plain / wrapped transcript-history medians, in microseconds:

| Command | TTY, 41 rows | GUI, 30 rows | GUI draft hooks disabled |
| --- | ---: | ---: | ---: |
| Self-insert | 45 / 46 | 55 / 57 | 51 / 50 |
| Short-line Backspace | 56 / 57 | 63 / 65 | 46 / 46 |
| C-k joining lines | 981 / 1780 | 1058 / 1810 | 302 / 420 |
| Yank | 699 / 1014 | 819 / 1096 | 58 / 59 |
| Transpose lines | 696 / 1020 | 752 / 1046 | 73 / 73 |

The GUI body geometry matches its control (636 pixels, 90 columns). Typing stays
near baseline; the cheap deletion gate replaces the initial 440--767 microsecond
TTY Backspace capture. Backspace in a *wrapped draft*, where deleting one character
can remove a row, still takes the exact path: about 1.58 milliseconds in both
environments (GUI hooks-off control 150 microseconds). These command timings
include dispatch and garbage collection, not the following explicit redisplay.
Every candidate preserves the expected draft text and blank-space budget;
hooks-off controls fail the shrink cases. Correct wrapped and multiline edits
remain more expensive than edits without viewport repair.
