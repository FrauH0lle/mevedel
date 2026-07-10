# Worktree session owns plan execution

When an approved plan is implemented in a new worktree session, the source
session records the approval without pending verification, while the worktree
session receives its own accepted-plan artifact and approved metadata with
verification pending.  This preserves the planning history while keeping plan
execution and verification attached to the session that changes the files.
