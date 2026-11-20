# ADR 0136: Dock the composer in its own buffer

Status: accepted

## Context

Reading earlier turns should not hide an unfinished prompt. The inline draft
scrolled with the transcript, and its shared buffer coupled typing and streamed
output to the same redisplay and viewport bookkeeping. [ADR 0134](0134-float-the-composer.md)
kept the draft visible by putting a second window on that buffer in a child
frame. That put the input surface in a different frame from its popups: which-key
and Embark could fail to show, Corfu candidates were clipped, the child covered
Vertico candidates, and Help or the session menu could replace the transcript.
Redirecting display to the parent did not make child-frame editing equivalent
to editing in the parent.

## Decision

An editable view owns one composer buffer. Each eligible window displaying
the view has an ordinary split immediately below it showing that buffer. Splits
share the draft but not the window caret or scroll position. The transcript
owns the rendered turns, status and interaction zones; the composer owns editing,
undo, history, completion and the rail. Submission uses the owning view and data
buffer, not a hidden inline draft.

```text
Frame
  Main area: one vertical window combination per pair
    Transcript window  -> rendered view buffer
    Composer window    -> draft buffer (rail, draft, mode line)
  Side windows: popups outside the pair
```

The composer keeps its normal, user-configurable Emacs mode line. The paired
transcript window suppresses its own mode line so there is none between them.
The composer's top rule also replaces the transcript's plain closing rule
where the two would meet; labelled request summaries remain visible.

`mevedel-view-pair.el` owns lookup and layout. `window-combination-limit` gives
the two windows their own parent. The dock is dedicated to its buffer, preserves
its height, and survives `delete-other-windows` in the transcript. The paired
transcript is weakly dedicated: automatic popup display skips it, but explicit
`C-x b` can still replace its buffer. Window parameters route split and delete
through the pair's parent and `delete-other-windows` in the dock through the
transcript. The dock's
`C-x b` switches the transcript's buffer instead of replacing the draft.
Native split and switch-back can install and fit the dock before the first
paint. See [window commands](../view.md#docked-composer) for the user contract.

Commands and display rules can bypass window parameters; restoring a layout
can also lose them. Redisplay hooks notice damage and queue the affected frame
for a coalesced zero-delay timer rather than changing windows during redisplay.
The timer reads the current tree, keeps valid pairs, restores missing rules or
docks, and removes orphan dock windows without discarding drafts. Repeating
repair on an intact layout makes no changes. Repair responds to layout events,
not model output; stable typing needs no repair and there is no periodic timer.

Each transcript window follows output only if it showed the end before the
write, unless a surviving row visit takes priority. Scrolling away preserves
the reading position, and returning to the end resumes following. The draft's
caret and focus are not follow signals. [ADR 0126](0126-keep-the-composer-still.md)
owns the viewport rules and their exceptions.

## Alternatives and limits

- **A second window on the same buffer** would still couple drafting to
  transcript rewrites. As a child frame it also interfered with popup geometry
  and focus. A same-frame split uses ordinary display rules without a separate
  frame for the draft.
- **Atomic windows** redirect split and delete to the atom root, but the native
  deletion commands can error when that root is the frame root. Restoring a
  window configuration also drops `window-atom`. They do not remove the need
  for repair and give less control over ordinary window commands.
- **A native side window as the dock** belongs to a frame-wide slot, not one
  transcript. It can collide with a popup or span multiple transcript windows.
  Conversely, a display rule that places the *transcript* in a native side
  window gets no dock: that slot cannot contain an independent pair. The
  transcript remains usable. A too-short window can also stay unpaired until
  it can fit a dock.

The same pair works in GUI and terminal Emacs, in `/btw`, and inside the
note conversation's child frame. The note frame contains the whole conversation,
not a separate frame overlaying just the draft. Its fallback is an ordinary
right-hand split. Agent transcripts have no dock. Historical session segments
normally hide the dock and preserve the live draft. Arming a fork shows that
dock for the new prompt; cancelling hides it again without leaving the segment
or changing the draft.

## Performance verdict

The user accepted the residual costs after the layout and repaint fixes, not
performance parity. Six warmed, alternating pairs in a presenting private KWin
compared byte-compiled builds on PGTK Emacs 31.1: inline `95ebf09a` and docked
`912b06d2`, both using the portable rail renderer.
Both paid for the same explicit checkpoint before layout. These are median
paired changes, with positive values indicating added cost:

| Scenario | Metric | Docked minus inline |
| --- | --- | ---: |
| Typing | Command-to-redisplay latency | −1.2 ms |
| Wrapped deletion | Command-to-redisplay latency | −2.7 ms |
| Streaming | Render latency | −13.7 ms |
| Streaming | Emacs CPU time per second | −33.3 ms/s |
| Animated idle | Emacs CPU time per second | −13.2 ms/s (37.0 → 23.8) |
| Split | Command-to-redisplay latency | +4.6 ms |
| Switch away (`C-x b`) | Command-to-redisplay latency | +0.7 ms |
| Switch back (`C-x b`) | Command-to-redisplay latency | +0.9 ms |
| Transcript scrolling | Command-to-redisplay latency | +0.7 ms |
| Transcript scrolling | Emacs CPU time per second | +15.7 ms/s |

Quiet-idle CPU did not show a clear change. The stock mode line keeps its
normal behavior; the measured cost does not justify replacing it or making
Doom's mode line the default. These timings do not measure scanout or provider
transport. The [development guide](../development.md#docked-composer-benchmark)
explains how to repeat the comparison or compare two docked revisions.
