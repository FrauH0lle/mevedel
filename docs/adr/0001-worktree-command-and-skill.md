# Worktree command and skill

Status: accepted

mevedel will model Git worktree-backed checkouts as worktree sessions: sessions whose working directory is a Git linked worktree while still belonging to the same workspace. Human worktree creation belongs in a deterministic `/worktree` slash command, while model-side worktree guidance belongs in a bundled `git-worktree` skill invoked through the model-visible `Skill` tool.

This deliberately avoids adding a model-visible `Worktree` tool in the first pass. The command gives users predictable local workflow and session switching, while the skill gives the model read-only detection and instructions for permission-gated Bash fallback. A schema-driven `Worktree` tool can be added later if model-driven creation through Bash proves too fragile.
