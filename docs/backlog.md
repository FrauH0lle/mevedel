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
- Check the codebase for battery friendliness
- Lobby: let a lobby open a session whose same-host lock is provably stale
  (dead PID or PID reuse) without the Emacs prompt; today a crash leaves such
  sessions refusable only from the keyboard. See `docs/collaboration.md#the-lobby`.
- Headless hosts: audit minibuffer prompts reachable during a guest-driven
  turn outside the interaction overlays (`yes-or-no-p`, `completing-read`);
  an Emacs daemon has nobody to answer them.

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
