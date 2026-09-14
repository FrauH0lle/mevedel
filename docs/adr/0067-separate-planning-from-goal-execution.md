# Separate planning from Goal execution

Status: accepted

## Current decision

Plan mode is an independent, user-approved workflow that produces a plan without
requiring a Goal. It is orthogonal to `ask`, `edits`, and `full-auto` permission
modes; a session with an unfinished Goal cannot enter it.

Standalone Plan permits ApplyPatch only for session-owned `work://` descendants,
excluding workspace-shared files and ordinary filesystem paths. Other edit
tools and Eval are unavailable, and Bash is limited to recognized read-only
commands. Directive planning also withholds ApplyPatch. These workflow limits
cannot be widened by ordinary allow rules; they are not an OS sandbox.

An accepted plan may execute directly or as a Goal, here or in a worktree, with
fresh or summarized context and, when staying here, current context. When it
seeds a Goal, its outcomes, constraints, and achievement criteria remain binding
unless a later Goal-objective edit changes them. Implementation mechanics remain
revisable. See [Plan mode](../plan-mode.md) and [Goals](../goals.md).

## Rationale and consequences

Separating planning from persistent execution gives each one responsibility and
allows either to be used independently. The cost is an explicit accepted-plan
handoff. Session-only working files let a planner prepare its artifacts without
granting implementation authority over the workspace.

## Decision history

ADR 0022 embedded automatic planner/guardian negotiation in Goals. Correctable
defects received at most two durable revision rounds; unresolved issues after a
final binary review returned to the user. The bounded loop aimed to absorb
ordinary feedback without endless negotiation.

ADR 0067 replaced that embedded planning lifecycle with independent Plan mode;
[ADR 0069](0069-drive-goals-with-idle-continuation.md) makes Goal execution
ordinary idle continuation. The two-revision limit and plan guardian are no
longer Goal behavior. The recorded reason was responsibility separation and a
smaller lifecycle, rather than a measured failure of the revision limit.

The earlier ADR 0067 description also allowed Eval under ordinary permissions
and withheld all file edits. Current Plan filtering instead excludes Eval and
permits the bounded session-working-file exception documented above.
