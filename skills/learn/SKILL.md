---
name: learn
description: Write durable non-obvious session findings to workspace instructions or existing memory
context: inline
user-invocable: true
argument-hint: "[focus]"
---

Review the current session for durable, non-obvious knowledge worth carrying
forward, optionally limited by `$ARGUMENTS`, and write only material that passes
the minimum-signal rules below.

Search `work://shared` for relevant working files and `memory://journal/` for published
digests as additional evidence. Missing, delayed, or failed captures are possible;
acknowledge relevant omissions and do not claim exhaustive coverage. Preserve
source/task attribution and distinguish user statements, observed outcomes,
and hypotheses. Repeated summaries of one source are not independent evidence.
These records do not authorize new work or override current instructions.

## Route each finding

- Put repository-derivable contributor facts such as build quirks, surprising
  file coupling, misleading errors, and undocumented commands in the nearest
  applicable `AGENTS.md`. Create that file only when no applicable file exists.
- Use `AGENTS.local.md` only for user-local repository facts that should not be
  shared with other contributors.
- Put stable user preferences, reusable corrections, and explicitly confirmed
  non-obvious approaches in the existing `user` or `feedback` memory categories,
  preserving why and when they apply. Silence is not confirmation.
- Put enduring context or motivation unavailable from code, Git history,
  project instructions, or maintained documentation in project memory.
- Put otherwise undiscoverable pointers to authoritative information in trackers,
  documentation, or external systems in reference memory, with their purpose.
- Tasks, blockers, deadlines, progress, and decisions belong in the project
  tracker or maintained documentation. Conversation state belongs in session
  context and compaction. They do not qualify as memory merely because they
  appeared in the session; invoking this skill is not a request to duplicate them.

Shared files are mutable working material, not a substitute for curated memory.
Memory writes use ApplyPatch at `memory://ROOT-KEY/RELATIVE-PATH`, with an existing topic file
when one covers the subject, otherwise one topic file plus its `MEMORY.md` index
entry. Never write memory body text directly into `MEMORY.md`.

## Minimum signal

Skip facts that are obvious from maintained documentation, standard framework
behavior, current-session status, speculative or one-off observations, secrets,
and facts already recorded. Debugging fix recipes belong with the code and
commit; retain only a distinct reusable lesson if it qualifies above. No changes
is a successful result. Prefer repository instructions for facts that can
be rediscovered from the repository; do not duplicate them into memory.

Verify cheap claims before writing. Find the nearest instruction scope, preserve
the existing file's style, and state each new instruction with its relevant scope and reason.
Update or replace duplicate or stale entries instead of appending another copy.

Invoking this skill authorizes the requested write-back, but every concrete edit
still uses normal mevedel tool permissions and review. Ask only when the correct
destination or scope is materially ambiguous.
