# Plan mode

Plan mode is a sticky planning workflow independent of the session's stored
`ask`, `edits`, or `full-auto` permission mode. Enter it with `/plan`,
`/plan PROMPT`, the cockpit, or `C-<tab>` in the composer. `Shift-TAB` in
the composer continues to cycle only
`ask` -> `edits` -> `full-auto` -> `ask`,
including while Plan is active. The composer displays both dimensions as
`[Plan · MODE]`. Directive scope pauses Plan dispatch while preserving Plan for
the return to chat; its composer chrome shows MODE and `Plan paused`. A session
with an unfinished Goal cannot enter Plan; a
completed Goal is historical and does not block it.

`/plan PROMPT` enters Plan and sends `PROMPT` through the ordinary
`UserPromptSubmit` and skill-preparation transaction. Ordinary follow-up turns,
including implementation requests, remain planning input until the user exits
Plan or accepts a proposal.

## Directive Plan before implementation

A top-level directive can independently enable **Plan before implementation**
from its overlay or inspector Settings menu. Nested directive presentations use
their owner's setting. The setting is off by default and does not enter sticky
session Plan mode. Implement, Implement this, Request changes, and Retry first
send the exact eventual implementation request through a read-only planning
turn. Discuss is unchanged. Batch processing remains sequential: an
enabled directive reaches approval and implementation before the next item
starts.

Directive planning uses the directive's model and effort override. Its proposal
uses the ordinary `<proposed_plan>` parser and shared approval interaction. The
directive card offers only implementation Mode, Model, Skills, Instructions,
Accept, Feedback, and Cancel: execution is always Direct in the directive's
bound session and current context. The selected mode and model are request-local
implementation settings; they do not replace the session policy or the
directive's planning model. Selection survives proposal feedback, while a fresh
directive action starts with fresh defaults seeded with the directive's own
skill selection (deduplicated by skill source); the card's selection at accept
time is the single authority for that attempt.

## Directive skills

A directive's Settings menu also toggles **skills** on the workspace record,
selected from the user-invocable roster of the directive's live bound
execution session, else the workspace's active chat session (with no open
session, selection is refused). The selection persists with the record, shows
as a `SKILLS:` label line, and attaches to direct implementation-type prompts
(Implement, Implement this, Request changes, Retry) with the accepted-plan
handoff semantics: argument-free instruction mentions stored by canonical
`SKILL.md` source that ignore skill model/effort, agent, hooks, and permission
policy. Discuss never sees the attachments. Skill sources are
revalidated at every dispatch; a missing, disabled, malformed, or no longer
user-invocable selection starts no request and leaves the directive retryable,
and batch processing skips that directive and continues.

The durable presentation progresses through Planning, Plan Ready, Plan
Accepted, Implementing, and the ordinary terminal lifecycle. Planning turns,
the current proposal, selection, and the exact accepted implementation prompt
belong to the workspace directive record. Resume restores a pending proposal;
cancel keeps the planning history as a draft; Continue Plan re-enters the
directive composer scope. Editing the owner request or a nested detail
invalidates an unstarted proposal. Rewind removes planning turns at and after
the checkpoint and restores an accepted pre-attempt plan when it removes that
plan's implementation attempt.

Only one root workflow owns a session at a time. Ordinary Plan cannot start
while directive planning is active, and a directive cannot begin planning while
ordinary Plan is active. Ordinary chat input uses the existing pending-input
queue while directive approval owns the session.

## Planning model policy

Every root Plan request resolves the current preset's `planning` workload at
request realization. The resolved backend, model, and effort are local to that
request; the session's ordinary model policy and chat buffer remain unchanged.
Changing the preset therefore affects the next Plan request. Missing workload
fields inherit the session policy, while an invalid workload fails before
dispatch.

For example:

```elisp
(:model-workloads ((planning :tier strong)))
```

A single leading user-invoked inline skill may override the planning model or
effort for that request. Instruction skills and model-side Skill calls do not.
Retained agents keep their own workload policies. Plan Summary preparation
uses the shared `summarization` workload.

## Tool boundary

Plan requests expose `ApplyPatch` for all-local `local://` descendants,
including retained agents, so durable plans and notes can be updated through
the ordinary `ApplyPatch` path. Before materialization, the pipeline denies any
proposal with an ordinary, non-local, or bare endpoint tree-wide; this includes
mixed local/ordinary and ordinary-only proposals, and neither local nor
ordinary targets are touched. Permission modes and explicit allow rules cannot
widen this workflow boundary. Other edit tools and `Eval` remain unavailable:
arbitrary Emacs Lisp cannot be classified reliably as read-only, including in
a child process. Resource-address completion remains side-effect free. See
[`address-to-resource.md`](address-to-resource.md#execution-target-and-plan-mode).

Directive Plan before implementation has a separate, stricter boundary: its
planning requests remain read-only and do not expose or allow `ApplyPatch`,
including all-local proposals, or `Eval`.

## Proposal interaction

Only a completed root-assistant prose span can create a proposal. The last
complete line-oriented `<proposed_plan>...</proposed_plan>` block in that span
becomes the one actionable proposal. A later proposal replaces it; an accepted
user follow-up demotes it to a non-actionable draft while preserving the
selected axes.

Every Plan turn includes the preferred proposal shape: a concrete title,
Summary, Key Changes, Regression Coverage, Validation, and Assumptions. This
is model guidance rather than a parser-enforced schema; the proposal parser
continues to accept any nonblank Markdown inside the exact line-oriented tags.

The approval interaction has these axes:

| Location | Current | Fresh | Summary |
|---|---:|---:|---:|
| Here | yes | yes | yes |
| Worktree | no | yes | yes |

- Location: Here or Worktree.
- Context: Current, Fresh, or Summary. Summary costs one additional model
  request.
- Execution: Direct or Goal.
- Budget: a positive Goal token limit, or Unlimited.
- Mode: Ask, Edits, or Full-auto.
- Model: the proposal-local implementation backend/model and reasoning effort.
- Skills: canonical user-invocable skills attached to implementation.
- Instructions: free-form multiline implementation-only guidance.

Keys are `l` for Location, `c` for Context, `e` for Execution, `m` or `TAB`
for Mode, `M` for the proposal-local Implementation model selector, `s` to
toggle skills, `i` to edit implementation instructions, and—when Goal is
selected—`b` for Budget. The ordinary `/model` cockpit remains session-level.
The instructions editor saves with `C-c C-c` and cancels with `C-c C-k`.
`RET` accepts, `f` opens an editable feedback draft, `q` hides the approval,
and `C-g` cancels.
Selecting Worktree while Current is selected changes the context to Fresh. A
dirty source checkout is not copied or stashed; Worktree starts at `HEAD`.

Pending input remains available while the approval overlay is open. `C-c TAB`
queues a follow-up without demoting or settling the proposal. Same-turn
steering left from the Plan request must be delivered or resolved before
acceptance, while queued follow-ups do not block acceptance and run after the
accepted implementation kickoff. The same ordering applies while handoff
preparation or retry owns the session.

The approval card keeps the Markdown proposal expanded above a compact
Implementation section. Cycle keys appear beside their settings; accept,
feedback, hide, and cancel remain in the action footer. A hidden approval stays
pending and hidden across live-session redraws. `RET` or a mouse click on its
`1 plan` interaction-counter segment shows the card again; resuming a persisted
session also shows it because hidden state is not persisted.

Direct remains the default and sends one ordinary implementation turn. Goal is
an explicit opt-in that continues automatically until complete, genuinely
blocked, paused, or budget-limited. When Goal is selected, the approval shows
the proposal's target token budget. `b` accepts a positive integer; empty input
means Unlimited. The setting starts from the effective session default, stays
local to the pending proposal, survives Execution toggles and revised
proposals, and applies only on acceptance. The first proposal snapshots the
session model and effort into the same persisted selection. `M` changes only
that implementation snapshot; choosing a model that cannot support the
selected effort resets effort to its default. Cancellation or Plan exit leaves
the session model unchanged. Acceptance applies the stored snapshot to every
implementation target and retry.

## Acceptance and recovery

Acceptance archives the plan immutably, clears the interaction, exits Plan,
and shows `Preparing implementation...`. Here/Current keeps the planning
transcript. Fresh starts a new context with setup material and the accepted
plan. Both Summary locations project the same effective source history and
generate the shared eight-section handoff background. The accepted proposal is
replaced in that evidence by an omission marker; the exact immutable plan and
implementation-only instructions are supplied separately as relevance focus
and remain authoritative. Worktree variants create a clean target session and
install the accepted artifact there.

Here/Summary applies that background through aggressive root compaction, with
no verbatim tail, and therefore retains `PreCompact`/`PostCompact`, bounded
request retries, segment rotation, persistence, and the new compact context
epoch. Worktree/Summary makes one summary request without compaction hooks,
source mutation, segment rotation, or a context epoch. Before target insertion
it rewrites source-checkout absolute paths relative to the repository root.
The successful result is cached in the durable retry record, so a later
artifact, worktree, or implementation-start failure does not regenerate it.

Durable Plan descriptors contain only a normalized session-relative artifact
path and its content hash. The qualified logical path is derived after the
owning target session is known; remote consumers read verified publication
bytes for that logical artifact. Absolute source or publication paths are not
part of Plan metadata or retry records.

After any valid location/context preparation, Direct submits its ordinary
one-turn instruction. Goal instead constructs a phase-free Goal in the prepared
target session, using the target-owned immutable accepted-plan path and the
Plan-selected Goal budget. Its deterministic objective treats plan
outcomes, constraints, and acceptance criteria as the completion contract while
allowing mechanics to follow current repository evidence. The first canonical
turn receives the prepared context, resolved artifact path, full accepted plan,
and compact kickoff; the transcript stores that full input while the view shows
`Implement accepted plan as Goal`.

The generated implementation turn goes through the ordinary deterministic
skill planner. Skills selected with `s` are stored by canonical `SKILL.md`
source and attached as argument-free instruction mentions. Live `$skill`
mentions written in the `i` instructions editor use the same semantics and
deduplicate with selected skills. Instruction attachments ignore skill
model/effort, agent, hooks, and request permission policy. Their current source
is reloaded at dispatch and retry; an explicitly bound skill that is missing,
disabled, malformed, or no longer user-invocable leaves the accepted handoff
retryable and starts no request. The transcript stores the full generated
prompt while the rendered view keeps the compact implementation label.

For Worktree execution, the target session owns the Goal, accepted artifact,
selected permission mode, accepted model/effort snapshot, and selected Goal
budget. The source session keeps its original permission mode and remains
otherwise unchanged. Branch acceptance first proves that Git and its worktree
command are available on the source session's execution target; preparation
then creates the linked worktree on that target and opens the target session
there. Later Goal turns derive exact read authority only for the validated
target-local artifact.

Acceptance is final even if preparation fails. The source session persists a
bounded retry record, and `mevedel-retry-plan-implementation` resumes from the
completed preparation step instead of recreating artifacts, summaries, or
worktrees. Goal execution reserves and persists its Goal ID in that record.
A retry reuses a target Goal only when both the reserved ID and target-local
accepted-plan reference match; a different unfinished Goal remains untouched
and is reported as a conflict.

Goal construction is the Plan recovery boundary. Once the target Goal is
durable, the source retry is cleared before kickoff. A kickoff startup failure
therefore pauses the Goal with its runtime reason and is resumed with ordinary
`/goal resume`, not Plan retry. Direct keeps the exact prepared retry record
through request startup and clears it only when the canonical request FSM
settles successfully. Provider errors and aborts retain the record with the
terminal reason for `mevedel-retry-plan-implementation`.
If recovery finds the matching Goal already durable but paused by session
restore, it reactivates it without scheduling before retrying the owned
kickoff, preventing both duplicate Goals and duplicate continuations.

For Here + Goal, non-command input submitted after acceptance queues behind the
reserved kickoff. Each queued follow-up then runs as its own Goal turn before
automatic continuation. A Worktree source remains independent; its input stays
ordinary source-session work, while input entered in the prepared target queues
behind that target's kickoff. Paused kickoff-owned input remains held until
resume.

Plan activity, proposal identity, selection, artifact descriptors, and retry
state persist in session metadata. Resume reconstructs an approval only when
Plan is active, metadata says `proposed`, and the current artifact still
matches its recorded hash. Persisted proposals without an implementation model
snapshot are demoted instead of migrated. Drafts never reactivate
automatically.
