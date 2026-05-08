# Persistent Memory

mevedel gives each workspace a persistent memory directory at
`.mevedel/memory/`. Memory is model-writable and persists across
conversations, but it is intentionally conservative: it should preserve
durable, non-obvious context that will change how future sessions
behave.

## Layout

```
.mevedel/memory/
  MEMORY.md             ; always-loaded index
  user-style.md         ; topic file
  release-context.md    ; topic file
  external-systems.md   ; topic file
```

`MEMORY.md` is an index, not a body store. It is the only memory file
included directly in the normal system prompt. The first 200 lines are
loaded and prefixed with a generated HTML comment describing the index
file's last modification date:

```markdown
<!-- Last updated: 2026-05-08 (today) -->
- [User style](user-style.md) - communication preferences for this user
- [Release context](release-context.md) - current release coordination facts
```

Topic files hold the actual durable memories. `MEMORY.md` entries should
stay short, usually one line under about 150 characters:

```markdown
- [Title](file.md) - one-line relevance hook
```

## Topic Files

Each topic file uses YAML frontmatter:

```markdown
---
name: User style
description: Communication and review preferences for this user
type: feedback
---

Prefer terse completion responses after code edits.

**Why:** the diff and tests already show most routine details.
**How to apply:** summarize material outcomes, risks, and verification
instead of replaying every edit.
```

Supported `type` values:

- `user`: stable details about the user's role, goals, expertise, or
  durable preferences.
- `feedback`: guidance about how mevedel should approach work, including
  corrections and confirmed non-obvious successes.
- `project`: ongoing work, deadlines, ownership, incidents, or decision
  context not otherwise derivable from code, docs, or git history.
- `reference`: pointers to external systems such as ticket projects,
  dashboards, runbooks, or incident trackers.

Feedback and project memories should preserve enough context to be
actionable later. Prefer a direct rule or fact followed by `**Why:**`
and `**How to apply:**`.

## Save Policy

The memory prompt asks the model to pass a minimum-signal gate before
saving:

> Will a future session plausibly behave better because of what I write
> here?

If the answer is no, the model should write nothing. In particular,
memory should not store:

- Code structure, file paths, architecture summaries, or project
  conventions that can be recovered by reading the current repo.
- Git history, recent changes, or who changed what.
- Debugging recipes where the fix is already represented by code and
  commits.
- Information already documented in `AGENTS.md`, `CLAUDE.md`, or project
  docs.
- Session-specific task state, temporary tool output, live metrics, or
  speculative conclusions.
- Secrets, tokens, credentials, or private data not required for future
  work.

Saving is a two-step operation:

1. Create or update a topic file under `.mevedel/memory/`.
2. Add or update the one-line pointer in `MEMORY.md`.

When the user explicitly asks mevedel to remember something, the model
should save it immediately if it fits the policy. If the request asks to
save a log-like or recoverable fact, the model should ask for the
surprising or non-obvious durable part instead. When the user asks to
forget something, the model should remove both the topic content and the
corresponding index entry.

## Prompt Inclusion

The persistent memory section is produced by `mevedel-system.el` from
`prompts/system/memory-policy.md`.

For normal sessions, memory is included after workspace configuration
(`AGENTS.md` / `CLAUDE.md`) and before environment details. The prompt
cache key includes both `MEMORY.md` metadata and the current date so the
generated age annotation can refresh daily even when the file is
unchanged.

If `MEMORY.md` does not exist, the prompt includes an empty-index notice
that tells the model to create topic files and link them from
`MEMORY.md`.

Agents can opt out. `mevedel-define-agent` supports
`:include-memory nil`, and several utility agents disable memory so they
do not inherit main-agent context unnecessarily.

## Staleness

Memory is context, not proof. A memory that names a function, file,
command, flag, or external resource records what was believed when the
memory was written. Before recommending or acting on such a memory, the
model should cheaply verify it against current files, docs, git, or the
external system.

If the user says to ignore memory or not use memory, the model should
proceed as if `MEMORY.md` were empty. It should not apply remembered
facts, cite them, compare against them, or mention them.

## Review Skill

The bundled `/remember [focus]` skill reviews the memory landscape and
reports proposed cleanup, promotion, stale-memory, or ambiguity findings.
It looks at `MEMORY.md`, linked topic files, other memory topic files,
and applicable `AGENTS.md` / `CLAUDE.md` files.

The skill is report-only. It should not edit memory unless the user
explicitly approves changes after seeing the report.
