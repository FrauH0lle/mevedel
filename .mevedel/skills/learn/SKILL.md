---
name: learn
description: Extract durable, non-obvious learnings from a session, commit, or current changes into AGENTS.md, docs, and mevedel memory. Use when user asks to record learnings, update agent guidance, capture discoveries, or run learn on a chat/session/commit/current changes.
argument-hint: "session | current changes | commit/range | notes"
context: inline
allowed-tools:
  - "Bash(git status*)"
  - "Bash(git diff*)"
  - "Bash(git show*)"
  - "Bash(git log*)"
  - "Bash(npx @emacs-eask/cli*)"
---

# Learn

Capture only durable, non-obvious knowledge that will make future agents work
better. Prefer the closest maintained home for each learning; do not turn this
into a session log.

## Source selection

Use the invocation arguments at the bottom of this skill to choose evidence:

- Empty, `session`, or `chat`: analyze the current conversation and tool
  results.
- `current changes`, `working tree`, or `uncommitted`: inspect `git status`,
  staged/unstaged diffs, and any explicit user goal in the chat.
- A commit, ref, or range: inspect the relevant `git show`, `git log`, or `git
  diff` output.
- Free-form notes: treat the arguments as evidence, then verify drift-prone
  claims against files before recording them.

If the source is ambiguous, make the smallest reasonable interpretation and say
what you used.

## Destination rules

- `AGENTS.md`: future-agent operating guidance, hidden coupling, commands,
  traps, or files that must change together. Put it at the closest directory
  whose descendants share the learning.
- `docs/`: durable product, architecture, workflow, or subsystem knowledge. In
  this repo, prefer existing docs under `docs/` such as architecture, tools,
  permissions, agents, sessions, hooks, skills, view, preview, mentions,
  compaction, tutor, reminders, tech debt, or deferred trackers. Do not create
  new docs unless the existing map has no suitable home.
- `.mevedel/memory/`: user preferences, project intent, external references, or
  durable decision context that is not recoverable from code/docs/git. Follow
  the memory frontmatter format, update existing topic files instead of
  duplicating, and add a concise pointer to `MEMORY.md`.

Do not store recoverable code structure in memory; use AGENTS.md or docs
instead.

## Learning gate

A candidate must clear all gates:

1. Non-obvious: not standard framework/language behavior and not already stated
   plainly in README, AGENTS.md, or docs.
2. Durable: likely useful beyond the current session or commit.
3. Action-shaping: changes how a future agent should navigate, edit, test,
   debug, or explain the project.
4. Scoped: has a clear smallest destination.
5. Verified: drift-prone details like paths, commands, flags, or functions were
   checked against current files or git output.

Reject session-specific status, verbose explanations, obvious summaries,
duplicate entries, and speculation.

## Workflow

1. Extract candidate learnings from the chosen source. Include surprising
   errors, misleading symptoms, hidden relationships, command quirks,
   constraints, and coupled files.
2. For each candidate, identify the smallest relevant scope and likely
   destination.
3. Read existing `AGENTS.md` files on that path, relevant docs, and memory
   index/topic files when memory might apply.
4. Deduplicate and decide: record, merge into an existing entry, update stale
   text, or skip.
5. Edit conservatively. Keep AGENTS.md entries to 1-3 lines per insight; docs
   entries should match the surrounding style; memory entries must include YAML
   frontmatter and a `MEMORY.md` pointer.
6. Read touched files after editing. Run only relevant checks if the touched
   format has a focused validation path; for skill/doc-only edits, this is often
   just re-reading and optionally the focused skills test if available.

## Reporting

Return:

- Source used.
- Files created or updated, with learning counts per file.
- Memory topics created or updated, if any.
- Skipped candidates with brief reasons.
- Validation performed, or why no command was necessary.

## Invocation arguments

$ARGUMENTS
