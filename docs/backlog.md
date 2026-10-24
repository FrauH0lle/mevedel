# Project backlog

Canonical home for concise future-work requests and unresolved defects.
Detailed plans, investigations, and review reports belong under `.scratch/`.
Read this before planning work in any listed area.

Use the inbox for ideas that have not been investigated yet. Promote an
item to a detailed entry when its scope and current status are understood.
Remove items when they are implemented, obsolete, or no longer valuable.

## Inbox

- Consider making mevedel's data buffers hidden
- Add optional cached container/VM/WSL detection to environment context for
  local and SSH targets; TRAMP execution targets are already reported.
- Check if memory and journal processing could be async


## Execution

### Harness-enforced Bash timeouts

Managed Bash commands run until they exit or are stopped. `145c66a` removed
`mevedel-bash-timeout` (120 s default) and the model-facing `timeout_seconds`,
so only a native `timeout` the model writes itself bounds a command. A
forgotten command keeps running and holds the
[system sleep inhibitor](sessions.md#system-sleep). When Bash completion wake-ups
and the [Monitor tool](tools.md#monitor) were added (2026-10), the user decided
against restoring a Bash deadline for now; only Monitor expires (`timeout_ms`).
Revisit if forgotten commands become a problem in practice. Reference point:
Claude Code caps background commands at 10 minutes by default and 2 hours at
most. Expiry would reuse the existing TERM/KILL process-group teardown and
report a timeout outcome.

## View performance

Leads from ETAF's Ebox renderer (https://github.com/emacs-taf/ebox). Its
commit `faee59e` (2026-10) removed its Rust layout module along with its
rebuild-prove-publish pipeline, in favor of one Elisp node tree updated in
place. Measure each lead before implementing it.

### Rewrite only the changed part of a zone

`mevedel-view-zone--reconcile` deletes every owned span and inserts the whole
new text whenever anything differs. One ticking elapsed digit therefore
replaces the whole zone and moves point, markers and window starts, which the
reconcile path then captures and restores. Instead, keep the common character
prefix and suffix and set only the property runs that changed; a property-only
change then leaves the characters in place (Ebox `ebox--rewrite-span`,
`4785ad7`). Measure the redisplay cost of a status tick, and check whether some
of the logical-state capture and restore becomes unnecessary.

### Interrupt large single-turn preparation on input

Scheduled projection treats one turn as indivisible, so a very large turn
delays input for its whole preparation ([Render flow](view.md#render-flow)).
ETAF runs idle preparation under `while-no-input`, puts the pending work back
and reschedules when input arrives (etaf `c60063f`). A projection plan is
built off-buffer and checked against the source tick before publication, so
its preparation could be abandoned the same way. Wrap only pure preparation,
never buffer writes.

### Allocation budgets for hot paths

The [profiler ledger](../benchmark/profiler-optimization-results.md) measures
allocation for each candidate optimization, but no test fails when a hot path
starts allocating more. ETAF's gate (`emacs-taf/benchmarks/performance-gate.el`)
records conses per operation, net of the measurement's own allocation, plus the
GC count, and fails above a per-scenario budget. Cons counts are
deterministic, unlike wall time. Bounding a stream chunk, a composer keystroke
and a zone tick would extend the pattern that
`test/test-mevedel-session-control-fs.el` already uses for one path.

### Measure updates against the minimal edit

ETAF's "diff twin" derives the smallest plain buffer edit that yields the same
text and properties, replays it on a buffer copy with the same redisplay, and
reports the framework's cost against that floor. It also checks that the
reported change ranges reproduce the buffer. Applied to streaming and zone
updates, it would show how far each is from the cheapest possible edit, not
only whether it got faster.

## Whiteboard

### Excalidraw parity left out of the element model

[ADR 0121](adr/0121-store-whiteboards-as-excalidraw-elements.md) stores and
draws Excalidraw elements but cherry-picks interaction. Not yet implemented:
segment dragging for elbow arrows, object and grid snapping, a frame tool and
moving children with their frame, sticky-note font auto-fit with its lifted
corner and date footer, the constant-width laser-pointer stroke, and embedding
the scene in PNG/SVG downloads. Add each when a board needs it; Excalidraw's
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
need to observe every completion. A hook matching `TaskWrite` misses automatic
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
