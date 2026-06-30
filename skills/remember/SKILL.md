---
name: remember
description: Review persistent memory and propose cleanup or promotion changes
context: inline
user-invocable: true
argument-hint: "[focus]"
---

Review the user's memory landscape and produce a clear report of proposed
changes. Do not apply changes unless the user explicitly approves them after
seeing the report.

Use `$ARGUMENTS` as an optional focus area, such as a project, memory topic,
or kind of cleanup.

## Scope

Review these layers when present:

- `MEMORY.md` in each configured memory root
- Topic files linked from those memory indexes
- Other `.md` files under configured memory roots
- `AGENTS.md` and `AGENTS.local.md` files that apply to the current workspace

## Classification

Classify each memory or instruction as one of:

- **Promote**: belongs in `AGENTS.md` or project docs instead of
  auto-memory because it is shared, durable project guidance.
- **Keep in memory**: personal, contextual, or external-reference knowledge
  that is not derivable from current repo state.
- **Shorten index**: `MEMORY.md` carries body content instead of a one-line
  pointer.
- **Merge duplicate**: multiple memories say the same thing.
- **Remove stale**: contradicted by current files, docs, or newer memory.
- **Needs user input**: ownership, scope, or truth is ambiguous.

## Checks

- Treat `MEMORY.md` as an index only. Topic bodies belong in separate files.
- Prefer `.agents/memory/` for new portable memories and `.mevedel/memory/`
  only for mevedel-specific memories.
- Prefer current repo state over memory when they conflict.
- Do not propose saving code structure, file paths, architecture summaries, or
  git history when they can be recovered by reading files or git.
- When a memory references a path, function, command, or flag, verify cheaply
  before calling it current.
- Preserve useful `Why:` and `How to apply:` context for feedback/project
  memories.

## Report

Return a concise report with these sections:

1. Promotions
2. Cleanup
3. Stale or risky memory
4. Ambiguous
5. No action needed

For each proposed change, include the source file and the concrete edit in
plain language. Do not edit files in this skill run.
