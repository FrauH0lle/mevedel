---
name: git-worktree
description: Check Git worktree isolation and guide safe worktree creation before implementation work
context: inline
user-invocable: false
allowed-tools:
  - Bash(git rev-parse:*)
  - Bash(git status:*)
  - Bash(git check-ignore:*)
  - Bash(git worktree list:*)
  - Bash(printf:*)
---

# Git Worktree Guidance

Use this skill when implementation work should happen in an isolated Git
worktree, or when you need to verify whether the current session is already
isolated.

## Current State

The following checks are best-effort and read-only. Treat `unavailable` as
"inspect manually before relying on this value".

- Repository: !`git rev-parse --show-toplevel 2>/dev/null || printf unavailable`
- Git dir: !`git rev-parse --git-dir 2>/dev/null || printf unavailable`
- Git common dir: !`git rev-parse --git-common-dir 2>/dev/null || printf unavailable`
- Superproject: !`git rev-parse --show-superproject-working-tree 2>/dev/null || printf none`
- Branch: !`git rev-parse --abbrev-ref HEAD 2>/dev/null || printf unavailable`
- Short HEAD: !`git rev-parse --short HEAD 2>/dev/null || printf unavailable`
- Dirty status: !`git status --short 2>/dev/null || printf unavailable`
- `.worktrees/` ignore state: !`git check-ignore -q .worktrees/ 2>/dev/null && printf ignored || printf not-ignored-or-unavailable`

Existing worktrees:

```!
git worktree list --porcelain 2>/dev/null || printf unavailable
```

## How To Use This

Prefer the human `/worktree` command for deterministic user-driven creation:
`/worktree status` or `/worktree create [NAME] [--for "purpose"] [--clean]`.
You cannot invoke slash commands yourself.

If the user explicitly requested worktree isolation and there is no
model-visible Worktree tool, use normal permission-gated Bash as a fallback.
Ask for consent before creating a worktree unless the user already clearly
requested it.

Mirror `/worktree` v1 defaults when creating through Bash:

- Create under `.worktrees/` in the current workspace.
- Add `/.worktrees/` to `.git/info/exclude`; do not edit `.gitignore`.
- Create a new branch from current `HEAD`; do not reuse existing branches.
- Map branch `worktree/foo` to directory `.worktrees/foo`.
- Do not run setup, dependency installation, or tests as part of creation.
- After creation, verify the project baseline with the repo's documented
  commands before implementation.

If the checkout is dirty, warn that uncommitted source changes are not copied.
If `HEAD` is detached, warn that the new branch starts from the current commit.
Do not create a new worktree from a submodule in v1.
