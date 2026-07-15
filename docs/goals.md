# Goals

The Goal workflow is mevedel's single controller for planned work. A Goal is a
session-owned completion contract: it keeps one objective authoritative while
the model plans, implements, and reviews as many cycles as the evidence
requires.

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

Planning and review are always read-only. The implementer cannot complete a
Goal; only a structured review verdict can do that. A `continue` verdict carries
review findings into a new planning cycle. Every accepted plan is copied from
`current-plan.md` to an immutable cycle artifact before implementation.

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
