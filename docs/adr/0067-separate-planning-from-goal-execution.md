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

Planning may delegate to agents. The boundary for delegated work is an
immutable per-turn Plan read-only stamp, not the session's current phase:
every agent turn started under Plan, in sticky Plan or directive planning,
keeps Plan limits until it ends, and skill preparation inherits the caller's
stamp. A Plan caller cannot steer an agent turn that lacks the stamp.
Directive planning uses the implementation preset's tools under that stamp,
so both kinds of planning share one tool surface apart from ApplyPatch.
Directive Discuss keeps its separate read-only ceiling
([ADR 0090](0090-enforce-discussion-as-a-read-only-capability.md)).

Completed proposals accept an opening tag glued to preceding prose and indented
delimiters. The opening tag still ends its line, and the closing tag occupies
its own line. Parsing and view hiding share those rules. Automatic continuation
compaction restores the current Plan reminder before dispatch rather than
depending on a generated summary to retain the proposal contract.

An accepted plan may execute directly or as a Goal, here or in a worktree, with
fresh or summarized context and, when staying here, current context. When it
seeds a Goal, its outcomes, constraints, and achievement criteria remain binding
unless a later Goal-objective edit changes them. Implementation mechanics remain
revisable. See [Plan mode](../plan-mode.md) and [Goals](../goals.md).

The `planning` workload selects the planning model. The separate
`plan-implementation` workload initializes approval's implementation model and
effort, defaulting to session policy. Both Plan flows share this initialization;
the user's approval selection remains authoritative through revisions, accepted
handoffs, and retries.

## Rationale and consequences

Separating planning from persistent execution gives each one responsibility and
allows either to be used independently. The cost is an explicit accepted-plan
handoff. Session-only working files let a planner prepare its artifacts without
granting implementation authority over the workspace.

Delegation lets a planner gather evidence in parallel and obtain an independent
critique before proposing. Because the stamp belongs to the turn, a planning
agent's authority cannot grow when the root workflow moves on; the cost is
that a planning agent must be followed up from an ordinary turn before it can
implement. Mail and results that reach an agent after Plan ends remain advisory
text acted on under that agent's own authority.

## Decision history

The September 2026 preset configuration used Astra for planning and Sol for
workers, but accepted-plan implementation still inherited Astra from the root
session. Selecting Sol required a manual model change on every new approval.
Resolving `plan-implementation` at initial selection replaces that unconditional
session snapshot while preserving the existing approval and persistence flow.

ADR 0022 embedded automatic planner/guardian negotiation in Goals. Correctable
defects received at most two durable revision rounds; unresolved issues after a
final binary review returned to the user. The bounded loop aimed to absorb
ordinary feedback without endless negotiation.

ADR 0067 replaced that embedded planning lifecycle with independent Plan mode;
[ADR 0069](0069-drive-goals-with-idle-continuation.md) makes Goal execution
ordinary idle continuation. The two-revision limit and plan guardian are no
longer Goal behavior. The recorded reason was responsibility separation and a
smaller lifecycle, rather than a measured failure of the revision limit.

Until 2026-10-09, only directive planning stamped delegated work; sticky Plan
children were bound by the live session flag, and directive planning ran on
the Discuss preset with delegation and Bash denied. Two gaps moved the
decision. A sticky-Plan child still running when its proposal was accepted
regained its role's edit tools, because the session flag cleared on
acceptance. Directive planning had no Bash, although the Plan tool boundary
documented read-only Bash for it, and could not delegate. Stamping every
planning turn, guarding steering, and running directive planning on the
implementation preset under the stamp replaced that split.

The earlier ADR 0067 description also allowed Eval under ordinary permissions
and withheld all file edits. Current Plan filtering instead excludes Eval and
permits the bounded session-working-file exception documented above.

On 2026-09-16, the resource-root investigation again produced a complete plan
whose opening tag was glued to the preceding sentence. The earlier column-zero
parser rejected it; a warning made the failure visible but did not recover the
proposal. Offline gptel Responses replay had also shown assistant messages
concatenated without a separator. Accepting that opening boundary replaces the
formatting restriction without changing transcript bytes or approval authority.
The rendering investigation exposed a separate loss: its Plan reminder was
present before automatic compaction and absent from the rebuilt continuation.
It finished with an edit-permission request instead of a proposal. Restaging
the existing reminder replaces reliance on summary fidelity for this contract.
