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
Every accepted plan is copied from `current-plan.md` to an immutable cycle
artifact before implementation.
Goal phase changes do not weaken or strengthen child confinement: Bash and
batch Eval continue to use the selected sandbox boundary, which remains visible
in the main view's status zone.

## Commands and approval policy

- `/goal <objective>` starts a supervised Goal. Each plan waits for user
  approval.
- `/goal auto <objective>` starts an automatic Goal. Each plan first goes to a
  tool-free Goal guardian; anything other than a valid approval escalates to
  the ordinary user approval prompt.
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
and the permission guardian remain authoritative.

A policy change never cancels an in-flight request. It applies at the next
unresolved plan-approval boundary. Switching to automatic dismisses an open
ordinary approval prompt and consults the Goal guardian; a recorded verdict is
reused only when its plan hash matches the persisted proposal. An `ask` verdict
still requires the user, while a matching `approve` verdict may continue.
Switching to supervised while the guardian is running lets that read-only
request settle, records its audit, and presents the plan to the user. Paused or
blocked Goals remember the new policy without resuming; complete Goals reject
the change.

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
