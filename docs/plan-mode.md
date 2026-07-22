# Plan mode

Plan mode is a sticky planning workflow independent of the session's stored
`ask`, `auto`, or `full-auto` permission mode. Enter it with `/plan`,
`/plan PROMPT`, the cockpit, or the Shift-Tab mode cycle. A session with an
unfinished Goal cannot enter Plan; a completed Goal is historical and does not
block it.

`/plan PROMPT` enters Plan and sends `PROMPT` through the ordinary
`UserPromptSubmit` and skill-preparation transaction. Ordinary follow-up turns,
including implementation requests, remain planning input until the user exits
Plan or accepts a proposal.

## Tool boundary

Plan requests omit `Write`, `Edit`, and `MkDir`. The pipeline denies those
tools tree-wide as a backstop, including for retained agents and already
realized requests. Bash remains available only when the canonical analysis and
policy classify the invocation as read-only. Eval and other tools retain the
ordinary permission policy.

## Proposal interaction

Only a completed root-assistant prose span can create a proposal. The last
complete line-oriented `<proposed_plan>...</proposed_plan>` block in that span
becomes the one actionable proposal. A later proposal replaces it; an accepted
user follow-up demotes it to a non-actionable draft while preserving the
selected axes.

The approval interaction has these axes:

| Location | Current | Fresh | Summary |
|---|---:|---:|---:|
| Here | yes | yes | yes |
| Worktree | no | yes | yes |

- Location: Here or Worktree.
- Context: Current, Fresh, or Summary. Summary costs one additional model
  request.
- Execution: Direct or Goal.
- Mode: Ask, Auto, or Full-auto.

Keys are `l` for Location, `c` for Context, `e` for Execution, `m` or `TAB`
for Mode, `RET` to accept, `f` for an editable feedback draft, and `q` or
`C-g` to cancel. Selecting Worktree while Current is selected changes the
context to Fresh. A dirty source checkout is not copied or stashed; Worktree
starts at `HEAD`.

Direct remains the default and sends one ordinary implementation turn. Goal is
an explicit opt-in that continues automatically until complete, genuinely
blocked, paused, or budget-limited. When Goal is selected, the approval shows
the effective target token budget as read-only information; it is not another
selection axis. Revised proposals preserve the selected execution, while
cancellation or Plan exit discards it.

## Acceptance and recovery

Acceptance archives the plan immutably, clears the interaction, exits Plan,
and shows `Preparing implementation...`. Here/Current keeps the planning
transcript. Fresh starts a new context with setup material and the accepted
plan. Summary first produces a compact handoff. Worktree variants create a
clean target session and install the accepted artifact there.

After any valid location/context preparation, Direct submits its ordinary
one-turn instruction. Goal instead constructs a phase-free Goal in the prepared
target session, using the target-owned immutable accepted-plan path and the
target's ordinary Goal budget. Its deterministic objective treats plan
outcomes, constraints, and acceptance criteria as the completion contract while
allowing mechanics to follow current repository evidence. The first canonical
turn receives the prepared context, resolved artifact path, full accepted plan,
and compact kickoff; the transcript stores that full input while the view shows
`Implement accepted plan as Goal`.

For Worktree execution, the target session owns the Goal, accepted artifact,
selected permission mode, and inherited Goal budget. The source session keeps
its original permission mode and remains otherwise unchanged. Later Goal turns
derive exact read authority only for the validated target-local artifact.

Acceptance is final even if preparation or request startup fails. The source
session persists a bounded retry record, and
`mevedel-retry-plan-implementation` resumes from the completed preparation
step instead of recreating artifacts, summaries, or worktrees.

Plan activity, proposal identity, selection, artifact descriptors, and retry
state persist in session metadata. Resume reconstructs an approval only when
Plan is active, metadata says `proposed`, and the current artifact still
matches its recorded hash. Drafts never reactivate automatically.
