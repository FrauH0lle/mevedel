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

Command completion uses the shared transport-idle boundary before advancing
the serial chain when its sentinel interrupts remote I/O. Pending completion
remains request-cancellable; it does not change hook ordering or authority.
Command startup checks cancellation before preparation and after yielding work,
and releases child handles returned after teardown. Terminal `Stop` and
`StopFailure` handlers run independently under their own timeout.
Hook approvals and fallback approvals recheck current hard restrictions at
settlement in every mode, including before permission queue admission.

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

Removing sandbox readiness from the session identity fence exposed a local hook
sentinel advancing into remote hooks during diagnostic-flush TRAMP I/O. Captured
stacks showed nested plugin-data creation and process launch failing before
their commands ran. Completion now reuses the transport owner's deferral and
cancellation instead of relying on incidental sandbox-probe timing.

A subsequent held-hook probe installed a command-segment or network deny while
`PermissionRequest` was outstanding. Full Access rechecked it, but Ask and Edits
accepted the hook's later allow. Current-policy revalidation now belongs to the
common approval settlement rather than a mode-specific callback branch.

Terminal cancellation made late cleanup registration synchronous. A subsequent
public-event probe showed command startup continuing after that cleanup, leaving
a child outside timeout ownership. Startup now fences each yielding acquisition
and collects late-returned resources without repeating settlement.
