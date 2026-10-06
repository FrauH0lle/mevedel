# Claude engine: user-requested changes from read-only review

Date: 2026-10-06. Branch: `feature/claude-code-engine`. Source: a read-only
review of the worktree and progress log, plus a conversation with the user.
Nothing in the worktree was changed except adding this file.

The user decided the points below. They narrow PRD choices that the PRD
otherwise leaves open (§9, §11 "capability-limited operations"). Treat them as
required outcomes for this Goal, not optional limitations.

## 1. Cross-engine history: transfer instead of refusal

Current state: switching between gptel and Claude Code once a root history
exists raises "Cross-engine history transfer is unavailable; start a new
session" (`mevedel-models.el:280`, `mevedel-claude-code-session.el:37`,
`mevedel-chat.el:1240`). The user does not accept this refusal as the outcome.

The user wants both directions:

- **Claude → gptel.** mevedel already writes Claude's text, reasoning and
  MCP tool calls/results into the canonical transcript, so gptel can rebuild
  history from it. Handle:
  - Reasoning: Claude's reasoning in the transcript cannot go back to an
    Anthropic API request as signed thinking. Drop it or convert it to text,
    following gptel's normal serialization rules for switched providers.
  - Length: Claude compacted its own history; the transcript did not. Use §2
    so gptel continues from the latest segment instead of the full record.
  - Context delivery: content that Claude acknowledged (guidance, memory,
    restoration via hooks) must count as undelivered for the new gptel
    conversation so the normal gptel rules send it again.
- **gptel → Claude.** "Continue from summary": start a new Claude
  conversation seeded with a summary or excerpt of the transcript. Label it
  as a summary continuation, never as an exact resume (PRD §8). Reuse §2's
  segment format where possible.

Keep a clear refusal only where a transfer cannot be faithful, with a reason
that names the next action.

## 2. Map Claude's compaction onto mevedel transcript segments

Adapter evidence (`.scratch/upstream/claude-agent-acp`, commit 0724b17,
`src/context-compaction.ts`): when the client advertises
`clientCapabilities.session.compaction`, the adapter sends `compaction_update`
messages and streams the retained summary as `compaction_summary_chunk`, with
the `<analysis>` block and continuation framing removed. Without that
capability it sends a synthetic "Compact conversation" tool call.

Current state: mevedel's ACP client does not advertise the capability. It
reacts to compaction only through the `SessionStart` hook with source
`compact` (`mevedel-claude-code.el:247`) and discards the summary. Verify that
the synthetic tool call is treated as an observation only.

Requested:

- Advertise the compaction capability and consume `compaction_update` and
  `compaction_summary_chunk`.
- On a completed compaction, archive the transcript so far as a segment and
  start a new segment with Claude's summary, reusing the existing segment
  machinery (`mevedel-view-segments.el` and mevedel's own compaction).
- Check: Claude can compact mid-turn between tool calls. Confirm that segment
  boundaries tolerate an unfinished turn, or define where the boundary lands.
- The summary is written for Claude and may refer to hook-injected context.
  It is fine as gptel's starting point, but don't treat it as neutral.

## 3. Model list: aliases plus the live Claude list

Current state: three hardcoded models in `mevedel-claude-code.el:72`
(`sonnet`, `opus`, `haiku`), with effort levels hardcoded per model.

Adapter evidence (`src/session-model.ts:391`): session setup returns
`availableModels` (ID, name, description) and `currentModelId`. Model info
carries `supportsEffort` and `supportedEffortLevels`
(`src/session-effort.ts:89`).

Requested:

- Keep the three aliases always available; the user wants them to resolve to
  the latest model.
- Add the models from a cached `availableModels` list, refreshed at a point
  that needs no model call (setup/readiness check or session start). Take
  their effort levels from the reported capabilities, not hardcoded tables.
- Validate configured models: when a preset, default, workload or directive
  override names a specific model (for example `Claude Code:claude-sonnet-5-5`),
  check it against the list as soon as a session or list is available. Report
  an unavailable model visibly before dispatch, with the next action. Never
  silently fall back to another model.

## 4. Test gap

No test switches Claude models (for example sonnet → opus) within one
established root history. The launch passes the current model on each turn
and resumes the session, so it should work; add a test that proves the new
model is used and history is kept.
