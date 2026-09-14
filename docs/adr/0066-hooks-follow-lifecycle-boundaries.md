# Hooks Follow Lifecycle Boundaries, Not Model Requests

## Current decision

Expose mevedel event plists at domain boundaries rather than exposing provider
request internals. `SubagentStart` runs once before a retained identity is
published; prompt, tool, compaction, and terminal hooks run for their respective
turn or attempt. `SessionStart` begins root context epochs, including startup,
resume, clear, compact, rewind, restore, and fork. Agent compaction emits no start
hook. Hook context does not recursively trigger earlier lifecycle events.

`Stop` is observational: a returned decision cannot restart the completed turn.
Goal continuation belongs to the Goal controller. Decision handlers run serially
through one engine, with native functions preceding declarative layers and
restrictive permission decisions preserved. Notification-only Emacs hooks
remain separate. See the [hook manual](../hooks.md) for the event table, ordering,
configuration, trust, and failure contracts.

## Rationale and alternatives

A provider request is too low-level a lifecycle boundary: tool continuations,
retries, and compaction can make several requests for one user operation.
Stable domain events let callers reason about when automation runs without
learning gptel's FSM. Serial mutation makes input/result rewrites deterministic;
parallel handlers would require a conflict rule for competing outputs.

Declarative command handlers support project automation while Elisp handlers
reuse Emacs integrations. Both use the same decision engine. Project files are
trusted by content and read as data; command execution follows the resource's
origin, so a local user hook does not silently become remote code. Adding each
new handler transport would require its own permission and cancellation contract.

## Consequences

Post-use hooks can change feedback but cannot undo completed side effects.
Commands fail open by default; handlers can request fail-closed behavior where
a supported event can block an operation. Structured decisions make consequential
changes auditable without injecting arbitrary stdout or stderr into model context.

## Decision history

ADR 0066 established lifecycle boundaries and observational Stop semantics.
The original hook design notes compared declarative command hooks, trusted
plugin callbacks, and direct gptel extension points. Their retained tradeoff is
stable mevedel events plus command/Elisp handlers; this record does not assert
that those external products still expose the APIs described in that research.
The notes did not record a separate date or measurement for that choice.
