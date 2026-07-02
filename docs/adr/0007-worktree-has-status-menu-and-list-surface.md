# Worktree Has A Status Menu And List Surface

The worktree cockpit should split status commands from row actions. `/worktree`
and the main cockpit worktree entry open a transient status menu with compact
repository/worktree state and direct commands such as refresh and create.
`/worktree list` opens a tabulated worktree list only when the user needs
selection-oriented actions on individual worktrees.

Keeping status in transient avoids a persistent buffer that only reformats a
small status report, while preserving a table surface for future worktree row
actions such as opening or pruning a selected worktree.
