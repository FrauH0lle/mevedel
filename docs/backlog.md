# Project backlog

Canonical home for project notes, todos, feature ideas, fixes, and
explicitly deferred work. Read this before planning work in any listed
area.

Use the inbox for ideas that have not been investigated yet. Promote an
item to a detailed entry when its scope and current status are understood.
Remove items when they are implemented, obsolete, or no longer valuable.

## Inbox

- Add a memory-verification slash command or skill that consolidates project
  memories and checks whether they are still accurate; explore whether a
  weekly automated check is useful.
- Add skill aliases.
- Rename permission modes: `default` -> `ask`, `accept-edits` -> `edits`, and
  `trust-all` -> `auto`.
- Pause the "working..." timer while user input is pending.
- Parse shell commands with tree-sitter when available.
- Explore making Plan mode behave more like Codex goal mode.
- Can Emacs prevent the PC from sleeping? Would be good for long running sessions

## Entry format

Each entry records its source, owed change, reason for deferral, current
status, and blast radius. Keep entries terse and remove them when they
become implemented, obsolete, or unjustified.

## Composer and mentions

### Atomic mention bindings

- **Source:** `mevedel-view-composer.el`; `mevedel-view-history.el`;
  `mevedel-mentions.el`
- **What's owed:** Let completed `$skill`, `@ref`, `@file`, and `@mcp`
  mentions retain the exact resolved target independently of their visible
  text. Live drafts can use text properties; queued prompts, retries, and
  history need a persisted sidecar such as `(:text ... :mentions ...)`.
- **Why deferred:** The binding must survive composer edits, queueing, input
  history, and mention expansion without making ordinary prompt strings
  harder to use.
- **Status check:** Composer, queue, and history paths primarily retain plain
  strings; mention expansion resolves visible text again at submission time.
- **Blast radius:** Duplicate names, renamed targets, and restored prompts can
  resolve differently from the target originally selected by completion.

## Sub-agents and coordination

### Git worktree isolation for parallel workers

- **Source:** `mevedel-worktree.el`; `mevedel-agent-runtime.el`
- **What's owed:** Assign isolated git worktrees to parallel workers that may
  edit overlapping files.
- **Why deferred:** The existing worktree UI manages user-created worktrees,
  but automatic worker allocation and cleanup need separate lifecycle rules.
- **Status check:** `mevedel-worktree.el` supports status, create, list, and
  deletion for sessions; agent dispatch does not allocate worker worktrees.
- **Blast radius:** Parallel workers must avoid or serialize conflicting file
  edits.

## Compaction

### Size-reducing retry for oversized compaction requests

- **Source:** `mevedel-compact.el`; `docs/compaction.md`
- **What's owed:** When a compaction request exceeds the provider context,
  retry with a smaller source body and fail clearly if the reduced request
  still does not fit.
- **Why deferred:** Tool outputs are now capped before summarization, which
  handles the common oversized-input case; arbitrary large non-tool content
  still needs a bounded fallback policy.
- **Status check:** Compaction has transport retries and body/tail tool-output
  caps, but retries resend the same source body.
- **Blast radius:** A transcript dominated by very large user or assistant
  prose can still make compaction fail at the API boundary.

### Sub-agent auto-compaction

- **Source:** `mevedel-compact.el`; `docs/compaction.md`
- **What's owed:** Give sub-agent transcripts an independent compaction and
  rotation path.
- **Why deferred:** The compaction writer rotates the parent session's current
  segment; an agent transcript is persisted separately and is not a session
  segment.
- **Status check:** Continuation compaction is wired into agent FSMs, but the
  eligibility check rejects agent buffers because they are not the active
  parent segment.
- **Blast radius:** A sufficiently long sub-agent run can exhaust its context
  even when the parent session remains below its threshold.

## Tools

### Bedrock backend support for deferred tool loading

- **Source:** `mevedel-tools.el` (`mevedel-tools--handle-deferred-inject`)
- **What's owed:** Read and replace Bedrock tools under
  `(:toolConfig :tools)` when deferred tools are injected or expired.
- **Why deferred:** Bedrock uses a different payload nesting from the other
  supported gptel backends and has not been exercised here.
- **Status check:** The handler explicitly supports only the common `:tools`
  path.
- **Blast radius:** Bedrock sessions cannot use deferred-tool loading
  correctly.

## Skills

### Apply skill `effort` overrides

- **Source:** `mevedel-skills-invoke.el`; `docs/skills.md`
- **What's owed:** Map skill effort to supported provider request parameters,
  with a clear no-op warning for unsupported backends.
- **Why deferred:** gptel exposes provider request parameters but no common
  reasoning-effort control.
- **Status check:** Skill parsing and request/invocation slots retain the
  value; `mevedel-skills--apply-overrides-handler` applies only model changes
  and reports effort as inert.
- **Blast radius:** Skills that declare `effort` do not change model reasoning
  behavior.
