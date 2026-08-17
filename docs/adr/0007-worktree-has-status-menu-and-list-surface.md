# Worktree Has A Status Menu And List Surface

The worktree cockpit should split status commands from row actions. `/worktree`
and the main cockpit worktree entry open a transient status menu with compact
repository/worktree state and direct commands such as refresh and create.
`/worktree list` opens a tabulated worktree list only when the user needs
selection-oriented actions on individual worktrees.

Keeping status in transient avoids a persistent buffer that only reformats a
small status report, while preserving a table surface for future worktree row
actions such as opening or pruning a selected worktree.

The status report itself no longer sits above the keys. The transient shows one
line — branch, isolation, dirty state, worktree count, directory — and raises an
untracked `.worktrees` directory as an alert line, because that is the only row
of the nine that asks the user to act. The full nine-row report is the surface's
info panel, opened with `i`, per
[0105](0105-cockpit-surfaces-follow-three-archetypes.md).
