# Goals

The Goal workflow is mevedel's single controller for planned work. A Goal is a
session-owned completion contract: it keeps one objective and its achievement
criteria authoritative while the model plans, implements, and reviews as many
cycles as the evidence requires.

Goal work follows this authority order:

1. Goal objective and stated achievement criteria
2. Authoritative referenced PRDs, specifications, and tickets
3. The accepted plan as an implementation approach

The plan does not replace the completion contract. Planning emphasizes desired
behavior, achievement evidence, material risks, and important choices over
routine implementation mechanics. Implementation may reasonably diverge from
plan details when necessary, but not from the Goal, achievement criteria, or
authoritative referenced requirements.

Achievement criteria remain part of the Goal's free-form objective rather than
a separate persisted field. Plans make those criteria explicit as observable
evidence without inventing materially new requirements. Any uncertain inferred
criteria belong under plan assumptions so approval can revise or escalate them.

Planning, automatic revision, and user-requested plan feedback use this
outcome-first guidance:

```text
<proposed_plan>
# Concrete Plan Title

## Goal
- State the desired outcome and important non-goals.

## Achievement Criteria
- List observable conditions that prove the Goal is achieved.
- Reference authoritative PRDs, specifications, or tickets when they provide
  the criteria.

## Approach
- Describe the intended method by subsystem or behavior.
- Include implementation details only when needed to resolve ambiguity,
  material risk, public interfaces, data shape, or architecture.

## Regression Coverage
- List user-visible flows, edge cases, failure scenarios, and intentionally
  unchanged behavior that tests should cover.

## Validation
- List exact focused test/build commands and other evidence to collect.

## Assumptions
- Record defaults, constraints, compatibility expectations, and unresolved
  inferences.
</proposed_plan>
```

The template is prompt guidance, not a parser-enforced schema. Plan parsing
continues to require only one exact line-oriented `<proposed_plan>` block.
Ordinary plans should use the full structure, but genuinely inapplicable
sections may be omitted. Concise plans delegating substance to a clear
authoritative PRD, specification, or ticket may use only the relevant sections
or no headings at all.

## Lifecycle

```text
planning -> awaiting approval -> implementing -> reviewing
   ^                                      |           |
   |                                      |           +-> complete
   +--------------------------------------+-----------+-> next cycle
                                                      +-> blocked
```

The lifecycle status is deliberately smaller than the phase state:

- `active` means the controller may dispatch the current phase.
- `paused` means continuation requires `/goal resume`.
- `blocked` means progress depends on user input or an external change.
- `complete` means review found evidence that the whole objective is done.

Planning and review are inspection phases: native edit tools are structurally
denied, while Bash and Eval remain available through normal permission policy
for inspection, tests, and builds. They are not OS-enforced read-only
environments. The implementer cannot complete a Goal; only a structured review
verdict can do that. Completion review requires evidence that the Goal and
achievement criteria are satisfied; performing every plan step is insufficient
when the desired outcome remains unmet. Conversely, reasonable implementation
divergence does not prevent completion when the authoritative outcomes are
proven. A `continue` verdict carries review findings into a new planning cycle.
Every accepted plan is copied from the candidate named by session plan metadata
to an immutable cycle artifact before implementation. The initial candidate is
`current-plan.md`; automatic replacements use revision-specific artifacts.
Goal phase changes do not weaken or strengthen child confinement: Bash and
batch Eval continue to use the selected sandbox boundary, which remains visible
in the main view's status zone.

## Commands and approval policy

- `/goal <objective>` starts a supervised Goal. Each plan waits for user
  approval.
- `/goal auto <objective>` starts an automatic Goal. Each plan first goes to a
  tool-free Goal guardian. It may approve, request a bounded automatic
  revision, or escalate a genuine user decision to the ordinary approval
  prompt.
- `/goal approval` reports the active Goal's approval policy.
- `/goal approval automatic` and `/goal approval supervised` change that
  policy without restarting the Goal. The Goal cockpit provides the same
  operation as the `o` toggle.
- Bare `/goal` opens the Goal cockpit for inspection and lifecycle actions.
- `/goal pause` stops continuation after the current request settles.
- `/goal resume [context]` continues from the persisted safe boundary.
- `/goal edit <objective>` changes the completion contract and pauses it for
  explicit review.
- `/goal clear` removes current control state while preserving transcript,
  artifacts, worktrees, and filesystem changes.

Automatic approval does not grant tool permission. Fully unattended mutation
also requires the user to select `full-auto`; explicit denies, protected paths,
and the permission guardian remain authoritative. Goal approval policy and
permission mode are orthogonal: `full-auto` cannot approve a plan or bypass a
Goal guardian `ask`, while automatic Goal approval cannot authorize
implementation tool calls. A Goal guardian `ask` always stops automatic
lifecycle progression and presents the plan to the user.

A policy change never cancels an in-flight request. It applies at the next
unresolved plan-approval boundary. Switching to automatic dismisses an open
ordinary approval prompt and consults the Goal guardian; a recorded verdict is
reused only when its plan hash matches the persisted proposal. An `approve`
verdict may continue, `revise` returns concrete feedback to the planner, and
`ask` requires the user.
Switching to supervised while a guardian review or automatic planner revision
is running lets that read-only request settle and records its result, but
dispatches no further revision or implementation. The latest valid plan is
presented to the user. Paused or blocked Goals remember the new policy without
resuming; complete Goals reject the change.

User input during an automatic planner revision or guardian re-review follows
the same rule: the in-flight read-only request may settle, but automatic
continuation stops and the latest valid plan is presented. Queued user input
must be resolved before implementation.

## Dispatch and model policy

The selected session preset describes the model team. At the start of every
phase, the controller resolves that phase's workload (`planning`,
`goal-guardian`, `implementation`, or `review`) from the current buffer-local
tier and workload maps. An in-flight request retains its resolved provider and
effort, while changing the preset affects the next phase. The cycle index
records the provider and effort actually used.

Every phase receives a deterministic context fragment regenerated from the
session sidecar. It includes the objective, lifecycle position, approval
policy, cycle and artifact pointers, latest review, budget, and execution home.
Phase-specific instructions are layered over that common contract. Reminders
and compacted prose can orient the model, but neither can mutate or reconstruct
Goal state.

The Goal guardian is an isolated guardian request rather than an ordinary Goal
phase prompt. Its dedicated system message contains the trusted review policy;
its separate user message contains deterministic Goal context and the proposed
plan as untrusted evidence. It receives no coding-assistant prompt, transcript,
tools, memories, skills, workspace instructions, referenced document contents,
or planning reasoning. Clear references to existing PRDs or tickets are trusted
without tool-based inspection after mevedel verifies that the path exists.

The guardian returns `approve`, `revise`, or `ask`. Approval begins
implementation under the existing permission system. Revision returns
planner-correctable feedback. Asking is reserved for decisions requiring user
judgment or user-only information and presents the unchanged plan in the
ordinary approval prompt. Goal outcomes and achievement criteria outrank plan
mechanics, so concise plans and authoritative references are acceptable. The
guardian treats all supplied Goal and plan text as evidence, never instructions.

Guardian evidence includes the review position and remaining automatic revision
allowance. When none remain, the final review permits only `approve` or `ask`;
the guardian must not approve a flawed plan to avoid escalation. The dedicated
runtime prompt lives at `prompts/goals/goal-guardian-system.md`; the maintained
[guardian prompt contract](guardian-prompts.md) owns the exact trusted wording,
response format, and adversarial examples. The permission and Goal guardians
intentionally share no base prompt abstraction.

One plan-approval boundary permits at most two automatic plan revision rounds.
Each round sends the guardian's feedback to the planner and reviews the
replacement plan again. After the second replacement, one final guardian review
permits only `approve` or `ask`: approval begins implementation, while `ask`
escalates the latest revised plan and latest guardian feedback to the ordinary
user approval prompt, identified as `Automatic plan revision limit reached
(2/2)`. Revised plans use the existing normalized plan hash comparison;
surrounding whitespace and canonical line-ending differences do not count as a
change, while no semantic comparison is attempted. A matching hash escalates
immediately with `Planner returned the same plan after guardian feedback`.
Guardian or planner timeout, request failure, or malformed output also
escalates immediately. The revision count is durable and scoped to the current
Goal cycle; token budgets and continuation deduplication remain secondary
guards rather than substitutes for the fixed ceiling.

Each revision round is recorded in the current Goal cycle with its revision
number, input plan hash, guardian verdict/reason/feedback, replacement plan
hash, planner and guardian provider/effort, request settlement state, and
timestamps. Full plan bodies remain in the existing current and cycle plan
artifacts rather than being duplicated in session state. Planner revisions and
their guardian re-reviews consume the Goal token budget like every other Goal
workload. If the budget is exhausted before either request can start, the
latest plan escalates to the user instead of exceeding the budget or remaining
invisibly paused.

Recovery never automatically replays an interrupted revision request. If a
valid replacement plan reached durable storage, resume at guardian review of
that plan; otherwise present the previous valid plan with an interruption
reason. The durable revision count is preserved and never reset by interruption
or resume.

A parsed, normalized replacement plan with a different plan hash is written to
`cycle-NNN-revision-NNN-plan.md` before guardian re-review, and session plan
metadata makes that revision artifact the latest durable candidate. It is
copied to the immutable accepted cycle artifact only after approval and before
implementation.

Automatic revision exposes only concise lifecycle status such as `revising
plan 1/2` and `guardian reviewing revision 1/2`. Guardian feedback and planner
revision turns are not inserted into the main conversation; they remain in
Goal audit metadata and plan artifacts, and become visible in the ordinary
approval prompt when escalation occurs or through Goal inspection surfaces.

An automatic revision is a fresh planning workload receiving only the
deterministic Goal context, current proposed plan, guardian's exact feedback,
current revision number, and remaining revision allowance. It must address the
feedback together and return one complete replacement plan rather than a
point-by-point reply. The revision request does not expand the conversation
transcript or create a dialogue with the guardian. Revision uses the normal
`planning` model workload and its inspection capabilities: native edit tools
remain denied, while read tools, Bash, and Eval follow the ordinary
planning-phase permission policy. No separate revision workload is introduced;
the provider and effort used are recorded for each round. If the planner fails
before returning a valid replacement, the previous valid plan is presented
with the guardian feedback, concise failure reason, and completed revision
count. Partial or malformed planner output never becomes the candidate plan.

## Continuation and recovery

Before dispatch, each phase writes a checkpoint containing its exact input,
workload, resolved provider and effort, plan reference, attempt identity, and
dispatch state. Automatic continuation is admitted only from a settled,
session-idle state with no pending interaction, queued user message, missing
guardian approval, exhausted budget, or repeated continuation key.

Read-only phase failures can retry within the bounded transport policy. Quota,
credit, authentication, persistent rate-limit, and forced-stop failures pause
the Goal and preserve the checkpoint. Resume resolves the current preset again,
so a replacement provider can continue the saved input.

Implementation recovery is intentionally asymmetric. A checkpoint proving the
request never started may retry the exact implementation input. A started or
unknown implementation is never replayed: resume first dispatches a read-only
repository audit against the objective and accepted plan, then continues from
the evidence-backed cycle boundary.

## Execution home and context

A Goal has one execution home for all cycles. It normally remains in the
current checkout. At the first supervised approval, the user may instead
transfer it to one Goal-owned worktree session; automatic Goals take this
choice from their session preset before work begins. The source session keeps a
handoff pointer but loses continuation authority. Ordinary forks copy session
settings and clear Goal ownership.

Implementation context is either `full` (the existing conversation plus the
accepted plan) or `focused` (fresh system prompt, authoritative Goal context,
and accepted plan). Full is the default in the current checkout; transferred
worktree Goals begin focused. The choice changes model context, not Goal state
or permission policy.

See [sessions.md](sessions.md) for persistence and ownership,
[permissions.md](permissions.md) for phase safety, [compaction.md](compaction.md)
for context rotation, [reminders.md](reminders.md) for event nudges,
[agents.md](agents.md) for model workloads, and [view.md](view.md) for the Goal
and Preset cockpits.
