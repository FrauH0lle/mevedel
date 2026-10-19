# Keep the composer still

Status: accepted

## Current decision

The view keeps the user's active surface at its screen position. Zone reconciliation
and its enclosing full-render transaction
choose one viewport anchor per window: point inside a surviving visited
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
Header, tab and mode-line heights are measured freshly with a zero text-height limit:
the visibility query uses fresh chrome internally, then restores cached heights,
so subtracting the public cached heights could introduce a pixel error before
redisplay. The chrome measurement subtracts an otherwise identical text-only
baseline because pixel scrolling can make that bounded text height negative.
One shared geometry seam derives both the body's top and usable height, excluding
the horizontal scrollbar and bottom divider too. These extra measurements do not
traverse buffer text and run only on paths already measuring geometry.
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

Command scope is explicit: a view's pre-command hook arms the transaction, and
post-command clears it before repairing the originating buffer, even if the
command switched buffers. `this-command` alone is not evidence that a command
is executing; it retains its last value between commands. A timer or process
mutation outside the armed scope repairs immediately rather than leaving a stale
capture for the user's next unrelated command.

Table splices preserve point, mark and window starts by cell identity and offset.
Their viewport transaction must not replace that exact mapping with a source-run
character offset or mistake the changed offset for a lost start. They therefore
reuse the same viewport policy while retaining the splice's position mapping.

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
turn's activity, replacing the separate progress row and the badge line. The
blank separator above the rail depends on how the text before it ends, so it is
checked again whenever a transcript writer finishes and redrawn only when it no
longer fits.

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

Browser request cards likewise keep their arrival slot on updates. Its
scrollable stack reads oldest first from the top, exposing the longest-waiting
decision initially. Replacing a changed card in place, rather than removing it
and appending a new one, preserves that order; an unchanged frame leaves the
card, its draft and its scroll untouched.

Closing an agent inspection's side window rewraps the parent's text without
any zone change. A parent window gets the composer's screen row from before
the inspection opened back only if, just before the close, it still shows the
composer with point in it. A window the user moved into history is left alone,
and a same-window inspection always returns to the parent at that row. Reflow
caused by the close itself does not make a window eligible.

Background-work rows are in stable start order, newest farthest from the
composer. Agent trees form units, with members in path order. Equal unit starts
put agents before executions, then sort lexicographically by agent root path or
execution owner path and ID, ascending; live, held and retained rows use the
same identity tiebreaks. State changes, including blocking, update rows in place.
While point or the mouse is on the panel, it keeps exactly the rows it showed:
finished rows stay as inert lines and new work is only counted, until both
leave. Completion glyphs and state labels use success/error faces for known
outcomes, warning for explicit stops, and a subdued face while unknown; names
and metadata remain dim. Explicit Show fewer is a user-requested layout change,
so it applies the normal limit under the pointer while preserving selected
inspectors and retained finished rows. Subsequent passive updates freeze that
reduced set. This intent survives deferred and coalesced status rendering.
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

- **Explicit overflow reduction and outcome colors (2026-10-19).** Applying
  passive row freezing after Show fewer left every expanded row visible while
  point remained on the control, even though its label changed to Show all.
  Explicit reduction now bypasses only that row-selection freeze, not the
  pointer tracking that protects completed rows and their grace periods.
  Previously dimmed completion labels also hid known success and failure;
  bounded terminal facts and a snapshot of each finished agent's outcome now
  color the label without making held rows interactive or changing their size.

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
  A later native table-shrink check exposed a two-pixel drift: the visibility
  query used an 18-pixel tab line while the cached accessor still returned 20.
  Fresh bounded chrome measurements now keep both coordinate calculations in
  the same frame before redisplay; exact header/tab change tests reject the
  previous cached subtraction. The same correction applies to usable body
  height: a larger tab line changed a cached 640-pixel body to 624 pixels, so
  mixing fresh EOF coordinates with cached body height overstated the blank
  area by 16 pixels until redisplay.

- **One viewport anchor per window (2026-10-18).** Logical window-start
  restoration previously overrode the composer's row inside growing cards,
  while composer anchoring displaced visited controls when unrelated cards
  arrived or disappeared. A graphical replay kept a visited Ask control at row
  3 through arrivals above and below it and removal after choosing the visited
  row first. Only visiting windows pay for the additional bounded row capture.
- **Equal-start ordering (2026-10).** Sorting only by start time let completion
  reorder tied rows as they moved between the live, held and retained lists.
  A held finished agent could shift a retained execution's action despite the
  unchanged panel height. Kind and stable unit identity now break start-time
  ties, independently of collection order and lifecycle state.

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

- **Rail separator after transcript writes (2026-10).** The separator was
  chosen only when the rail itself redrew. A batched history refresh published
  placeholders ending in a single newline, so the rail took an extra blank
  line; replacing the last placeholder did not redraw the rail, and the next
  unrelated rail update removed the line again. A 43-row window measured
  266, then 267, then 266 buffer lines, moving the transcript a row each time.
  The writer queue now rechecks the separator when it drains; an unchanged
  separator costs 0.38 µs (compiled).

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

| Command | TTY, 41 rows | TTY draft hooks disabled | GUI, 30 rows | GUI draft hooks disabled |
| --- | ---: | ---: | ---: | ---: |
| Self-insert | 42.2 / 43.4 | 41.1 / 41.0 | 47.9 / 48.3 | 45.8 / 46.0 |
| Short-line Backspace | 51.5 / 53.2 | 37.9 / 37.7 | 58.2 / 58.5 | 42.6 / 43.2 |
| C-k joining lines | 1282 / 2315 | 285 / 500 | 1469 / 2778 | 299 / 416 |
| Yank | 1010 / 1589 | 46 / 44 | 1251 / 2101 | 54 / 55 |
| Transpose lines | 1012 / 1589 | 66 / 65 | 1164 / 2025 | 69 / 68 |

The GUI body geometry matches its control (636 pixels, 90 columns). Typing stays
near baseline; the cheap deletion gate replaces the initial 440--767 microsecond
TTY Backspace capture. Backspace in a *wrapped draft*, where deleting one character
can remove a row, still takes the exact path: about 2.03 milliseconds in both
environments (TTY/GUI hooks-off controls 266/153 microseconds). These command timings
include dispatch and garbage collection, not the following explicit redisplay.
Every candidate preserves the expected draft text and blank-space budget;
hooks-off controls fail the shrink cases. Correct wrapped and multiline edits
remain more expensive than edits without viewport repair.

The final GUI typing difference is 2.1 / 2.3 microseconds. A separate interleaved
probe measured roughly 2--3 microseconds and no geometry calls during plain
self-insertion. An earlier 42-row fixture measured a 27--31 microsecond difference;
its cause remains unestablished, and the changed fixture does not explain it.
Several exact-repair costs also increased between successive builds, so those
cross-run differences are not attributed to one geometry change.

The same final compiled build measured zone reconciliation separately (medians
of three 300-call batches, microseconds per call):

| Path | TTY plain / wrapped history | GUI plain / wrapped history |
| --- | ---: | ---: |
| Genuinely unchanged zone | 4.81 / 4.83 | 4.87 / 4.86 |
| Changed equal-height zone | 346 / 754 | 424 / 911 |

Both paths preserve draft, point and viewport through redisplay; only the changed
path advances the character tick. These fixtures use a 634-pixel GUI body and
41-row terminal body. They measure the reconcile seam, not a complete session
redraw or provider response.
