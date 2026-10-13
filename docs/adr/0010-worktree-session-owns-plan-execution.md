# Worktree session owns plan execution

Status: accepted

## Current decision

When an approved plan is implemented in a new worktree session, the source
session keeps the approval archive and durable preparation record, while the
worktree session receives its own byte-identical accepted-plan artifact and
inherited preset. Selected model, effort, and permission mode belong to its
one Direct implementation request or durable Goal implementation policy, not
its ordinary session defaults. A new target retains its usual baseline;
reopening retains its restored baseline. Both Here and Worktree leave source
and target baseline policies untouched. The selected Goal budget sets the Goal
slot only, never the target's new-Goal default. This preserves planning history
while keeping execution attached to the session that changes files. Standalone
Plan has no verification-pending state.

Fresh targets retain the ordinary worktree setup context. Summary targets are
clean sessions: the source transcript is summarized without mutation, source
checkout paths become repository-relative, and the target stores the summary
before its accepted artifact path, full plan, and implementation instruction.
Each preparation attempt reconciles the target summary and saves the target
before advancing the source's durable retry record. The target receives accepted
metadata only after inherited preset restoration returns; interrupted preset
hooks therefore leave setup retryable rather than falsely prepared. Accepted metadata can
survive a failed save in the live target, so its presence alone cannot prove
durable preparation. Retrying this publication does not reinstall the preset
or duplicate a matching summary.

Direct retries reuse the accepted policy. Goal construction persists it before
clearing source recovery, so active Goal turns and resume retain the selection.
New ordinary chat with an inactive Goal uses the baseline. Model resolution is
exact, with no silent baseline fallback. Implementation children inherit the
request's permission-mode override under live shared rules and hard denies;
their model workloads remain independent. Summary and unrelated requests do
not acquire implementation settings. See [ADR 0067](0067-separate-planning-from-goal-execution.md)
for policy ownership and the strict Goal record.

## Decision history

The earlier handoff applied the selected implementation settings to the target
session itself. Here execution therefore overwrote ordinary source-session
settings, while Worktree execution replaced the target's baseline. It also
changed the target's new-Goal budget default. Request/Goal ownership replaces
those mutations because approval authorizes implementation, not a lasting
change to ordinary chat policy. A snapshot/restore alternative was rejected:
restoring old settings after execution would overwrite concurrent user changes.
The existing inherited preset and worktree setup context remain part of target
preparation, not an implementation-policy promotion.
