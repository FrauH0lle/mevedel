# Worktree session owns plan execution

When an approved plan is implemented in a new worktree session, the source
session keeps the approval archive and durable preparation record, while the
worktree session receives its own byte-identical accepted-plan artifact,
inherited preset settings, and selected permission mode. This preserves the
planning history while keeping Direct execution attached to the session that
changes the files. Standalone Plan has no verification-pending state.
