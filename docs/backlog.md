# Project backlog

Canonical home for concise future-work requests and unresolved defects.
Detailed plans, investigations, and review reports belong under `.scratch/`.
Read this before planning work in any listed area.

Use the inbox for ideas that have not been investigated yet. Promote an
item to a detailed entry when its scope and current status are understood.
Remove items when they are implemented, obsolete, or no longer valuable.

## Inbox

- Investigate Firefox shared-editor test failures for synthetic image paste
  and touch presence; both reproduce with the previous save behavior.
  Sync and retry checks pass. Evidence: `.scratch/whiteboard-sync-latency/`.
- Consider making mevedel's data buffers hidden
- Add optional cached container/VM/WSL detection to environment context for
  local and SSH targets; TRAMP execution targets are already reported.
- Lobby: let a lobby open a session whose same-host lock is provably stale
  (dead PID or PID reuse) without the Emacs prompt; today a crash leaves such
  sessions refusable only from the keyboard. See `docs/collaboration.md#the-lobby`.

## Editor CPU

### Remaining per-wakeup costs

On the measured pgtk build, Lisp/process wakeups present the whole frame surface, even
when nothing changed: about 2% editor CPU and 1% compositor per wakeup per
second on a 2x-scaled 1536x888 frame. The heartbeat, shimmer cadence, tool
rows, Bash progress and watch timers, stream batching, collection pacing and
unattended render timers were reduced for this (ADR 0119; `telemetry.md`,
`tools.md` and `sessions.md` for the heartbeat, Bash and collection). Still
open:

- Investigate animation presentation cost on non-PGTK displays before adding
  another native backend. They keep the accepted 8-fps breathe/bounce and
  half-speed glyph cadences; native surfaces cover PGTK/Wayland only.
  Burst-and-pause glyph variants were rejected.
- The 2-Hz telemetry heartbeat still incurs presentation work during
  requests; lower that cost while keeping useful lag detection.
- `mevedel-view--status-strip` still evaluates its cache key from several
  live lookups on every redisplay.
- The whole-surface repaint itself is an Emacs pgtk behavior worth reporting
  upstream.
- Streaming still costs 16-23% editor CPU after read pacing (ADR 0119), for
  gptel and Claude Code alike, mostly the per-wakeup pgtk presentation of the
  0.4-second cycle. An upstream pgtk fix (draw only damaged areas) would help
  every case. The measured Claude Code chunk rates come from a scripted peer;
  check a real adapter's rate and burst sizes.
- A coalesced callback that waits for input or output delays its siblings.
- A curl stream paused when Emacs crashes stays stopped, holding its socket,
  until killed; a crash leaves no hook to continue it.
- Native surfaces recheck placement only when their layout inputs change;
  invisibility or overlay changes that leave the character tick alone keep a
  stale position until the next edit, scroll or resize.
- `test/test-mevedel-chat.el` fails `mevedel-abort/test@4` when run alone
  (it passes in the partitioned suite); find the ordering dependency.

### Animation and native presenter follow-ups

- Freezing (input pause, zero fps) during a shimmer sweep holds the faded
  band for the whole pause. Consider snapping a frozen shimmer to its rest
  frame.
- The native surface paints the frame's default background, ignoring
  buffer-local face remapping, `hl-line` and `alpha-background`; such rows
  show a mismatched box.
- The C module caches the Wayland display and globals for the session and
  never resets them when that display closes (daemon, `delete-terminal`).
  Reset on the GDK display's `closed` signal.
- A native surface the module closes itself (unmap, scale change, cairo
  failure) stays excluded from Lisp ticks until the view's next semantic
  update; with only pending-tool rows and no status label nothing triggers
  one.
- A move while the previous position callback is pending can show one or two
  frames at the old position.
- Native placement, geometry translation and the C module have no automated
  coverage beyond mocks; their checks are lab scripts in
  `work://shared/editor-cpu/renderer-lab/`.
- A label whose native placement or timeline keeps failing reopens and closes
  the surfaces of the labels before it on each semantic update (about once a
  second). Remember failing signatures until the label changes.

### Wakeup review follow-ups

- `mevedel-view--prompt-on-screen-p` keys its memo on content, invisibility,
  start, size and the input marker, but not line wrapping, text scale, line
  spacing or font; a header can stay stale until the next edit or scroll.
- Tool-row refreshes recorded while a view is hidden are cleared when a later
  turn settles before it is shown, leaving an older row's progress stale.
- Publication collection arms its idle timer at the current idle time plus
  the wait; one armed during a long absence then needs that much idle time
  again after the user returns.
- The executions list sorts its Elapsed column as text.

## Request lifecycle

### Prevent system sleep during active requests

Hold an OS sleep inhibitor while root or agent requests run, releasing it on
completion, failure, abort, and stale-request replacement. Keep screen blanking
and locking unaffected. Request teardown is centralized in `mevedel-structs.el`
and `mevedel-agent-runtime.el`; no inhibitor is currently acquired. The main
constraint is reliable platform-specific cleanup so a leaked inhibitor cannot
prevent suspend after work ends.

## Whiteboard

### Excalidraw parity left out of the element model

[ADR 0121](adr/0121-store-whiteboards-as-excalidraw-elements.md) stores and
draws Excalidraw elements but cherry-picks interaction. Not yet implemented:
obstacle avoidance and segment dragging for elbow arrows (Excalidraw's A*
router), object and grid snapping, a frame tool and moving children with their
frame, sticky-note font auto-fit with its lifted corner and date footer, the
constant-width laser-pointer stroke, and embedding the scene in PNG/SVG
downloads. Add each when a board needs it; Excalidraw's
behavior is compiled in excali-mode's `docs/excalidraw-spec.md`
(https://github.com/yibie/excali-mode).

## Review

### Automatic turn advisor

Consider an opt-in second-model review of successful root turns, with bounded
feedback delivered to the next request. Reuse the Stop hook, reviewer runtime,
and reminder delivery. Keep findings distinct from Buddy's advisory notes, and
specify evidence scope, deduplication, cooldown, cancellation, and cost before
implementation. Pursue when recurring review failures justify the extra model
call; keep it advisory, without restarting completed turns. Implement this
bounded feature before considering generic prompt or agent hook handlers.
There is no automatic root-turn reviewer/instruction-injection path today.
The original request came from the 2026-08-20 watchdog/advisor discussion.

## Hook extensions

### Task transition events

Consider a task transition event when notifications or tracker synchronization
need to observe every completion. A hook matching `TaskUpdate` misses automatic
completion by `mevedel-tool-task-finalize-owner` in `mevedel-tool-task.el`.
Observe actual state transitions across both paths rather than tool calls alone.
Defer until a concrete integration needs that coverage.

### Deferred extensions

- HTTP, prompt, MCP-tool, and agent handlers: revisit when a named workflow
  cannot be served adequately by command/Elisp handlers. Define permission,
  cancellation, and recursive-hook behavior before adding a handler type.
- Handler-level predicates: keep conditions inside handlers unless repeated
  workflows or measured launch cost justify declarative filtering. Prefer an
  event-specific matcher where sufficient, such as skill-name matching for
  `UserPromptExpansion`, rather than a general predicate language.
- Configuration, cwd/files, tool-batch, and shell-environment events: add
  individually when a caller needs a boundary existing events cannot express;
  define the event's scope and control effects before implementation.
