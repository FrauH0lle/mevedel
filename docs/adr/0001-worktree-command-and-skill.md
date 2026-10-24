# Worktree command and skill

Status: accepted

mevedel models Git worktree-backed checkouts as worktree sessions: sessions whose working directory is a Git linked worktree while still belonging to the same workspace. Human worktree creation belongs in a deterministic `/worktree` slash command, while model-side worktree guidance belongs in a bundled `git-worktree` skill invoked through the model-visible `Skill` tool.

This deliberately avoids adding a model-visible `Worktree` tool. The command gives users predictable local workflow and session switching, while the skill gives the model read-only detection and instructions for permission-gated Bash fallback.

Worktree creation and Worktree Fork share an optional project setup boundary.
`mevedel-worktree-setup-enabled` defaults off, independently of the argv setting
`mevedel-worktree-setup-command`. Approved source-checkout directory locals may
configure both under the `mevedel-worktree-setup-mode` key, not the all-modes
`nil` key. The setup lookup enters this dedicated mode before reading locals,
so ordinary file visits do not receive setup settings or their unsafe-variable
approval prompt. Neither option is declared a safe local variable. Merely finding a
repository script must never execute it. When authorized, setup runs on the
source execution target in the new checkout before session creation; its output
is retained and a nonzero status stops creation without deleting the checkout.
The skill's Bash fallback follows the same explicit authorization boundary.

## Decision history

Creation originally ran no setup. Three fresh worktree test sessions failed to
load `gptel-request` and required manual `.eask` links. The optional shared setup
boundary replaces that blanket prohibition without making untrusted repository
code run automatically. This repository configures its offline dependency
bootstrap through directory locals rather than hardcoding development setup in
the shipped product.
