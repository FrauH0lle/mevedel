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
- While a table is streaming in, it flickers between rendered and raw
- compacting might collapse all expanded tools when starting
- Add optional cached container/VM/WSL detection to environment context for
  local and SSH targets; TRAMP execution targets are already reported.

## Request lifecycle

### Bound remaining large transcript redraws

Prepared historical tools now update their containing turn. Three paired stress
replays reduced median completion from 806 to 539 ms; worst typing delay remained
about 118 ms. Bound source segmentation, individual native operations and large
turn insertion, and extend batching to remaining synchronous callers without
losing live text. Consider independently trusted summary/payload storage for old
hidden metadata; the current producer already emits bounded direct-call metadata.
Preserve failure classification, source ownership and expansion behavior.

Subprocess experiments isolated GC but added startup, snapshot/export costs and
about 196 MiB worker RSS. Revisit immutable offload only where those costs are
amortized. Use original captures: measurements based on `second-bounded.org` are
withdrawn because its serialization changed unrelated history. Protocols and
remaining limits: `.scratch/session-performance/report.md` and
`.scratch/bounded-responsiveness/report.md`.

### Bound remaining publication callbacks and legacy storage

Interactive tool pipelines now yield between steps, and large control writes
stream encoded fields. A 9 MiB portable completion still has about 245 ms
worst input delay after the checkpoint placement and timer fixes. Follow-up
profiling attributes the longest remaining step to synchronous lease checks and
publication writes, sometimes including GC. Removing redundant copies and scans
reduced completion allocation but left worst input delay unchanged. Capture
production stalls before restructuring those transactions; split only at boundaries
preserving ownership, recovery and cancellation.

Exact-file collection reduces the example's publications from 1.14 GB to 405 MB
and response streaming brings its cold scan from 31.1 to 20.7 s before collection
and to 0.95 s afterward. Individual collection callbacks can still take about
140 ms including GC; a targeted callback replay attributed about 53 ms to GC
and measured 75 ms input delay when typing began inside a running callback
without GC, or 130 ms when the targeted callback included GC.
Investigate smaller validation batches if production background pauses persist. Idle collection now progresses
without requiring new user input; the transport timer restoration bug is fixed.
Existing fixed file-history caches also remain: verify all consumers and recovery
paths before reclaiming historical cache copies. New snapshot writes already
omit them. Deferred idle-agent hydration is implemented; measure real follow-up
workloads before adding another residency mechanism. Protocols and limits:
`.scratch/remaining-performance/report.md` and
`.scratch/completion-collection/report.md` and `.scratch/pause-followup/report.md`.

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
