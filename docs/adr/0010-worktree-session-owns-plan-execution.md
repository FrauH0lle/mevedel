# Worktree session owns plan execution

When an approved plan is implemented in a new worktree session, the source
session keeps the approval archive and durable preparation record, while the
worktree session receives its own byte-identical accepted-plan artifact,
inherited preset settings, and selected permission mode. This preserves the
planning history while keeping Direct execution attached to the session that
changes the files. Standalone Plan has no verification-pending state.

Fresh targets retain the ordinary worktree setup context. Summary targets are
clean sessions: the source transcript is summarized without mutation, source
checkout paths become repository-relative, and the target stores the summary
before its accepted artifact path, full plan, and implementation instruction.
