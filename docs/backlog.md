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

## Request lifecycle

### Bound remaining large transcript redraws

Prepared historical tools now update their containing turn. Three paired stress
replays reduced median completion from 806 to 539 ms; worst typing delay remained
about 118 ms. Scheduled settled-history refreshes now stage full-context
segmentation, grouping, Markdown and tool preparation. A 27-case frozen replay
reduced median maximum terminal input delay from 180.9 to 77.8 ms for a cold
root refresh and from 180.5 to 39.4 ms for a cold agent refresh, but doubled
scheduled settlement time; warm root still reached 101.6 ms. Synchronous cold
renders remain around 265 ms root / 247 ms agent. Bound remaining atomic tool
group/turn insertion, scanner repair and allocation-driven GC; investigate
display-only synchronous callers without losing live text. Consider independently
trusted summary/payload storage for old hidden metadata only if evidence
warrants the cost; the current producer emits bounded direct-call metadata.
Preserve failure classification, source ownership and expansion behavior. The
paired raw-result hashes and caveats are in
`work://shared/transcript-redraw-2026-09-27/report.md`.

Subprocess experiments isolated GC but added startup, snapshot/export costs and
about 196 MiB worker RSS. Revisit immutable offload only where those costs are
amortized. Use original captures: measurements based on `second-bounded.org` are
withdrawn because its serialization changed unrelated history. Protocols and
remaining limits: `.scratch/session-performance/report.md` and
`.scratch/bounded-responsiveness/report.md`.

### Bound remaining publication callbacks and legacy storage

Production telemetry now splits pauses into collection and callback time
and times saves, publications, and control programs. On a session with a
260 KB sidecar and 1.5 MB live segment, a save takes about 150 ms: two
whole-buffer structural passes of about 40 ms each (property normalization
before `GPTEL_BOUNDS`, whose tick memo never hits because the drawer write
changes the tick, and the prompt-index reparse), about 70 ms in five
control programs (recovery read, reservation, discovery sidecar,
generation, head commit), and a whole-segment rewrite. Both passes are pure
functions of mostly appended text; resuming them from a stable boundary
before the lowest changed position would remove most of their cost. Remaining pauses
of 200-400 ms are such saves, sometimes with a collection. Removing them
needs incremental segment and sidecar publication or saves off the
foreground path, preserving ownership, recovery, and cancellation.

Save As, fork, and rewind still copy every committed artifact through the
editor twice: 7.9 s for a 99 MB session. On local targets, link the
parent's immutable artifacts and reuse its manifest hashes.

### Prevent system sleep during active requests

Hold an OS sleep inhibitor while root or agent requests run, releasing it on
completion, failure, abort, and stale-request replacement. Keep screen blanking
and locking unaffected. Request teardown is centralized in `mevedel-structs.el`
and `mevedel-agent-runtime.el`; no inhibitor is currently acquired. The main
constraint is reliable platform-specific cleanup so a leaked inhibitor cannot
prevent suspend after work ends.

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
