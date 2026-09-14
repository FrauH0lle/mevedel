# Project backlog

Canonical home for concise future-work requests and unresolved defects.
Detailed plans, investigations, and review reports belong under `.scratch/`. Read this before planning work in any listed
area.

Use the inbox for ideas that have not been investigated yet. Promote an
item to a detailed entry when its scope and current status are understood.
Remove items when they are implemented, obsolete, or no longer valuable.

## Inbox

- Consider making mevedel's data buffers hidden

- Notification error: (dbus-error "org.freedesktop.Notifications.Error.ExcessNotificationGeneration" "Created too many similar notifications in quick succession") [3 times]
- Warning: unknown coding system "utf8" [6 times]

- shared editing
  - use comments for sending selections to llm

## Request lifecycle

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
specify deduplication, cooldown, cancellation, and cost before implementation.
There is no automatic root-turn reviewer/instruction-injection path today.
The original request came from the 2026-08-20 watchdog/advisor discussion.

## Hook extensions

- Evaluate HTTP, prompt, MCP-tool, and agent handlers, with explicit permission
  and cancellation contracts before adding a handler type.
- Add handler-level conditional predicates only when event/matcher selection
  cannot express a concrete workflow.
- Evaluate additional lifecycle events (configuration, cwd/files, tool batches,
  tasks, shell environment) against actual caller needs.
