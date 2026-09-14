# Select child-agent context explicitly

Status: accepted

## Current decision

`Agent` requires `task_name` and `message`, with optional `role`, `context`,
`model`, and `effort`. Omitted or `none` context isolates the child. `all` and
positive decimal values copy an immutable selection of effective parent dialogue;
`summary` supplies task-focused generated background without a verbatim tail.
Selection never reconstructs raw turns replaced by compaction.

The snapshot includes already-realized material from the active parent turn,
excludes the triggering Agent call, and never waits for sibling work or later
parent material. Summary preparation reserves the child path and capacity slot
without publishing an addressable identity. It runs ordinary task hooks once
and focuses generation on the hook-accepted task. Failure or cancellation
releases reservations, cancels generation, and suppresses late dispatch.

Copied turns retain their dialogue roles. Generated background lives in the
child transcript immediately before the authoritative initial task, outside
its frozen system configuration. Follow-ups retain it; child compaction may
absorb it into a continuation summary. Neither form synchronizes later parent
turns. The parent retains preparation metadata and a child link rather than a
duplicate summary. See [Agents](../agents.md).

## Rationale and consequences

An isolated default prevents an active parent's orchestration request from
becoming a second assignment for the child. Explicit dialogue selection remains
useful when the task requires the conversation itself; generated background
transfers relevant evidence without replaying its original instructions.
Summaries cost a model request, can be stale or untrusted, and cannot replace
the separately supplied assignment. Freezing evidence before generation makes
the result independent of later parent activity.

## Decision history

- **ADR 0040** introduced immutable post-compaction snapshots through
  `fork_turns`, initially defaulting to `all`. It removed foreground/background
  selection because every agent turn is asynchronous.
- **ADR 0042** reduced spawning to a task name and message plus optional role,
  context selection, and model policy. Redundant descriptions and runtime-mode
  fields were removed rather than retained as aliases.
- **ADR 0056** specified that full or recent-turn copies include effective
  anchored summaries, never reconstructed compacted history; isolated context
  includes neither. This constraint still applies to dialogue selection.
- **ADR 0071** replaced the full-copy default with isolation because spawning
  during an active parent turn could cause the child to execute the parent's
  orchestration request alongside its own task.
- **ADR 0095** replaced `fork_turns` with `context` and added task-focused
  handoff summaries. The original record specifies the lifecycle and authority
  tradeoff but cites no separate measurement motivating this addition.
