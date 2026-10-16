# Worktree session owns plan execution

Status: accepted

## Current decision

An approved plan implemented in a worktree is a Worktree Fork of the planning
session at the reply that proposed it. The fork carries the planning
conversation, receives its own byte-identical accepted-plan artifact and a retry
record for the equivalent Here handoff, and runs every context (Current, Fresh,
Summary) and execution (Direct, Goal) exactly as Here does. Its view replaces
the planning view in the same windows, and the planning session, saved with
only its approval archive, is closed. Execution stays attached to the session
that changes files, and the user keeps the conversation that led to the plan.

Selected model, effort, and permission mode belong to the one Direct request or
the durable Goal's implementation policy, not to session defaults, which the
fork copies from the planning session. The selected Goal budget sets the Goal
slot only, never a new-Goal default. Standalone Plan has no
verification-pending state.

The source retry record advances through durable steps: fork point, branch and
directory reservation before Git mutation, the fork's published save path, then
the installed fork record. A retry reopens the recorded or uniquely
discoverable fork instead of forking again, recognizes an installed plan by its
hash, and clears the source record only after the fork owns its own, so one
session owns the implementation at every point.

Direct retries reuse the accepted policy. Goal construction persists it before
clearing source recovery, so active Goal turns and resume retain the selection.
New ordinary chat with an inactive Goal uses the baseline. Model resolution is
exact, with no silent baseline fallback. Implementation children inherit the
request's permission-mode override under live shared rules and hard denies;
their model workloads remain independent. Summary and unrelated requests do
not acquire implementation settings. See [ADR 0067](0067-separate-planning-from-goal-execution.md)
for policy ownership and the strict Goal record.

## Decision history

The worktree session was originally a clean session created beside the source,
which kept the retry record and a separate target-preparation step. Context
Current was refused there, Fresh received a setup stub, and Summary was
generated from the source without compaction, then rewritten to
repository-relative paths and inserted into the target. In use, accepting a
plan opened a new buffer that lacked the whole planning conversation, and the
planning conversation simply ended: the user had to find the new buffer and
could not continue the discussion there. Worktree Fork, added later
(`mevedel-session-fork-create`), already copies a conversation into a linked
worktree with retargeted paths and restored session-edited files, so the
clean-session path, its target-only recovery, the worktree-only summary and the
target Goal reservation were removed in favour of forking. Consequences: the
worktree now includes files the session itself edited, restored by the fork;
and accepting refuses while the session cannot fork (pending input, a request,
live executions, active agent turns or an active Goal) instead of failing
afterwards.

The earlier handoff applied the selected implementation settings to the target
session itself. Here execution therefore overwrote ordinary source-session
settings, while Worktree execution replaced the target's baseline. It also
changed the target's new-Goal budget default. Request/Goal ownership replaces
those mutations because approval authorizes implementation, not a lasting
change to ordinary chat policy. A snapshot/restore alternative was rejected:
restoring old settings after execution would overwrite concurrent user changes.
The existing inherited preset and worktree setup context remain part of target
preparation, not an implementation-policy promotion.
