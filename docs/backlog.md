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
- Collaboration publishes while a reply streams still scan the whole
  transcript's segments (`mevedel-transcript-segments`, about two thirds of a
  retained publish: 8 of 12.5 ms at 1,001 records on the desktop). Resume the
  scan from the last closed control before the first change.
- A guest prompt waits ~120 ms on the deployed host for the synchronous
  session-transfer control programs in prompt admission before it is
  inserted. Same evidence.
- Check if memory and journal processing could be async
- Generic permission preflight (`mevedel-permission--preflight`, the Plan
  `early-decision` in `mevedel-permissions.el`) checks Plan read-only only
  through the ambient request and session, not the captured `request` it
  already holds. A deferred Eval or native-edit check could then lose a
  request-scoped Plan restriction to a newer non-Plan ambient request. Bash
  and Monitor were fixed at their own policy boundary; reproduce this one
  before changing it.

## Request lifecycle

### Time out unanswered permission prompts

A permission prompt currently waits indefinitely, which keeps its turn, and the
[system sleep inhibitor](sessions.md#system-sleep), alive while the user is
away. After a configurable period, deny the request and tell the model that
the user is probably absent. Open questions: the default duration, whether
agent prompts share it, and how it interacts with the automatic reviewer's
fallback to the human card.

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
