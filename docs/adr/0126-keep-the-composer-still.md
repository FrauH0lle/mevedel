# Keep transcript readers and controls still

Status: accepted

## Current decision

The transcript and composer are separate buffers. Each transcript window
follows streamed output if it showed the buffer end before the write; the
composer's focus or caret does not decide. Scrolling away stops following in
that window. Scrolling back or using Back to bottom resumes it without a
sticky follow flag. An output write leaves the viewport alone if the new end
still fits. If not, it brings the last page into view without selecting the
transcript or disturbing the dock's draft.

Zone reconciliation and its enclosing render transaction restore logical
positions first and then apply the viewport rule for each window:

| Context before mutation | Result |
| --- | --- |
| Visible, surviving status visit | Keep the row on its screen row even when its details shrink. |
| Visible, surviving interaction visit | Keep the row, then fill only newly exposed blank space below the buffer. Existing blank space stays. |
| Output write with the buffer end visible and no surviving visit | Keep the new end visible. Keep the caret if it remains visible on the new page. |
| Logical start removed or truncated | Fill the last page if the restored start is later than it, regardless of the new character tail. |
| History reader with a surviving start | Keep the logical reading viewport through changes below it. |
| Other restored starts | Clamp backward only when the character tail shrank. |

An ordinary status or card update does not opt in to output following. A row
visit takes priority over following even during a stream update. Strict
anchor resolution distinguishes a surviving row from a removed or truncated
one. Fill never moves a start forward and keeps a window already starting at
`point-min` there. Table splices preserve positions by cell identity and
offset; the viewport transaction does not override that mapping or treat the
old offsets as lost starts.

Unchanged and absent zones skip viewport capture entirely. Unchanged means
identical text including properties, complete live-region ownership, and no
stale namespace text outside it. Absent means no fragments, live region or
namespace text. Property-only changes and stale text still reconcile.

## Geometry

End visibility uses Emacs's native query, not a cached `window-end`. Anchors
use body-relative pixels and native backward traversal, so wrapping, face
remapping, images and partial vertical scrolling do not require a Lisp row
loop. Bounded native measurements give fresh header, tab and mode-line
heights before redisplay. Subtract the text-only baseline because vscroll can
make even a zero-height measurement negative. Body height also excludes the
horizontal scrollbar and bottom divider. Terminals use the same geometry in
character cells.

A numeric vscroll restore also requests native preservation. That request
lasts one redisplay; writers renew it even for a numeric no-op because a
forced start inside a wrapped line can still cause redisplay to reset it and
move point. Redisplay without a writer can do the same. The view snapshots
start and vscroll before redisplay and restores dropped vscroll in the scroll
hook only when the start did not change. Commands and writers run before the
snapshot, so intentional scrolling is kept.

## Stable interaction and background rows

Arriving questions and permission cards do not take focus. The pending count
announces them; `C-c C-j` visits the next one explicitly. A visit started from
the composer returns there when its card settles only if the user is still
visiting that prompt. Automatic focus let a keystroke intended for the draft
land on an approval action; an idle delay would only narrow that race.

Interaction cards keep their arrival slots, newest farthest from the composer.
The locally queued pending-input card stays nearest the composer. `C-c C-j`
starts with the longest-waiting card and repeated presses cycle outward.
Status and interaction panels remain real text so point can reach and activate
their controls.

Browser request cards likewise keep their arrival slot on updates. Its
scrollable stack reads oldest first from the top, exposing the longest-waiting
decision initially. Replacing a changed card in place, rather than removing it
and appending a new one, preserves that order; an unchanged frame leaves the
card, its draft and its scroll untouched.

Background-work rows are in stable start order, newest farthest from the
composer. Agent trees form units, with members in path order. Equal unit starts
put agents before executions, then sort lexicographically by agent root path or
execution owner path and ID, ascending; live, held and retained rows use the
same identity tiebreaks. State changes, including blocking, update rows in place.
While focused point or a bounded mouse visit protects the panel, it keeps
exactly the rows it showed: finished rows stay as inert lines and new work is
only counted. Mouse-only protection expires after 60 seconds, without renewal
by polling, status updates or motion within the panel. An observed exit, panel
disappearance or focus loss resets the visit. Both protections need a visible
frame with a focused top-level frame; terminals may also protect on unknown
focus, since their window system may not report it. Explicitly unfocused frames
never hold rows. Completion glyphs and state labels use success/error faces for known
outcomes, warning for explicit stops, and a subdued face while unknown; names
and metadata remain dim. Explicit Show fewer is a user-requested layout change,
so it applies the normal limit under the pointer while preserving selected
inspectors and retained finished rows. Subsequent passive updates freeze that
reduced set. This intent survives deferred and coalesced status rendering.
A finished row also lingers for ten seconds after protection ends, including
mouse-lease expiry or focus loss. Ending protection restarts that period;
explicitly expanded execution inspectors remain independently retained.

Panel heights depend only on which rows exist, not on their text. Every row,
finished line, preview line and rule fits one screen row of the narrowest
window showing the view, and re-renders when that width changes. An open
execution preview is always three lines. Expanded controls pack whole buttons
into explicit fitted rows, using short labels only when a button cannot fit
alone. The live layout sets the completion budget: an execution reserves its
Stop slot as inert space, and a held finished agent replaces its actions with
a padded finished notice. Model details are fitted in both states. Thus a
finished expansion keeps its displayed height, not just its logical line count.

## Decision history

- **Separate draft buffer (2026-11-20).** With the inline draft, inserting
  a status row could recenter the draft by thirteen terminal rows. Composer
  anchoring kept that caret on its screen row, but draft deletion, folded
  submission, popup height changes and temporary scrolling each needed
  their own viewport repair. The native dock puts the draft and transcript
  in different buffers: draft edits no longer move transcript text, and
  scrolling the transcript cannot displace the draft caret. End visibility
  replaces draft caret anchoring; draft blank-space and resize repair,
  scroll-to-draft restoration, draft guards and position-based editing maps
  are no longer needed. Reader and visited-row anchors remain necessary
  because renderers still replace transcript text in place.
- **Status visits retain their screen row (2026-11-01).** Filling blanks
  under a shrinking status row repeatedly moved the row down. Status now
  retains the row instead; interaction shrink still fills newly empty space.
- **Absent zones skip viewport work (2026-11-07).** A matched graphical probe
  found two anchor transactions per stream update: one for the live turn
  and another for an empty status zone. Skipping the absent zone removed
  the second pass without changing viewport observations. Combining
  distinct live and status mutations into one transaction was rejected
  because their viewport policies differ. The same probe saw higher native
  traversal cost with face-rich text inside the display span, but not with
  the same rendered table above it; there was no case for caching geometry
  across user scrolling or changes in window layout.
