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
- Put stable user preferences and corrections in the existing `user` or
  `feedback` memory categories, choosing global or project scope with the
  existing memory policy.
- Put non-derivable rationale, deadlines, coordination, and incidents in
  project memory.
- Put pointers to authoritative information outside the repository in
  reference memory.

Shared files are mutable working material, not a substitute for curated memory.
Memory writes use ApplyPatch at `memory://ROOT-KEY/RELATIVE-PATH`, with an existing topic file
when one covers the subject, otherwise one topic file plus its `MEMORY.md` index
entry. Never write memory body text directly into `MEMORY.md`.

## Minimum signal

Skip facts that are obvious from maintained documentation, standard framework
behavior, current-session status, speculative or one-off observations, secrets,
and facts already recorded. Prefer repository instructions for facts that can
be rediscovered from the repository; do not duplicate them into memory.

Verify cheap claims before writing. Find the nearest instruction scope, preserve
the existing file's style, and keep each new instruction to one to three lines.
Update or replace duplicate or stale entries instead of appending another copy.

Invoking this skill authorizes the requested write-back, but every concrete edit
still uses normal mevedel tool permissions and review. Ask only when the correct
destination or scope is materially ambiguous.
