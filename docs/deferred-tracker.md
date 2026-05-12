# Deferred work tracker

Living list of work that has been explicitly deferred — items the
specs, design notes, or in-code comments said "not now". Read this
before planning work in any of the listed areas; entries here may
overlap with what you're about to do.

This is the companion to `docs/tech-debt-tracker.md`. The two are
distinct:

- **Deferred** = something the design called for but consciously
  pushed to a follow-up (scope, blocked, staged rollout).
- **Tech debt** = a shortcut, hack, or rough edge the existing
  implementation already carries.

If an item belongs in both, it lives in `tech-debt-tracker.md` and is
not duplicated here.

Both humans and the agent may consult this file. The agent may
**propose** new entries when it spots deferred work mid-task, but
should flag them in the conversation first rather than appending
silently.

## Entry format

Each entry uses the structure below. Group entries by theme; newest
within a section on top.

```
### <short title>

- **Source:** spec / file / doc reference
- **What's owed:** the actual deferred change
- **Why deferred:** scope, risk, blocked on X, staged rollout, etc.
- **Status check:** what the code base currently shows
- **Blast radius:** what's affected while it's left undone
```

Keep entries terse. Link to the spec or maintained doc instead of
inlining a paragraph.

## Sessions and persistence

### Bulk session-management UI

- **Source:** `specs/19-session-persistence.md` Q4
- **What's owed:** `mevedel-session-list` (or equivalent) for
  listing, renaming, deleting, archiving sessions across a workspace.
- **Why deferred:** v1 users use `dired` / `rm -rf`; UI was scoped out
  to keep the persistence spec focused.
- **Status check:** No `mevedel-session-list` command in
  `mevedel-session-persistence.el`.
- **Blast radius:** Friction for power users with many long-running
  sessions; manual filesystem management.

### Main-session conversation history persistence

- **Source:** `specs/09-file-state.md` "Future work"
- **What's owed:** Persist the main session's conversation transcript
  to disk (mirroring the per-segment / file-history pattern).
- **Why deferred:** Spec 19 covers structural state and file
  snapshots; spec 21 covers sub-agent transcripts. The parent session
  conversation lives only in the live data buffer.
- **Status check:** `mevedel-session-persistence.el` saves segment
  files and the sidecar but the main transcript is in-memory only;
  resume relies on the live segment buffer.
- **Blast radius:** Lost conversation history if the data buffer is
  killed without segment finalization.

### Cross-machine session sync

- **Source:** `specs/19-session-persistence.md` "Out of scope"
- **What's owed:** A documented or tooled story for syncing
  `.mevedel/sessions/` across machines.
- **Why deferred:** Treated as a user-space sync concern (rsync, git,
  cloud filesystem).
- **Status check:** Not implemented.
- **Blast radius:** Multi-machine workflows must roll their own.

### Encrypted session storage

- **Source:** `specs/19-session-persistence.md` "Out of scope"
- **What's owed:** Encryption of persisted session contents
  (transcripts, file snapshots, sidecar).
- **Why deferred:** Users layer encrypted filesystems if needed.
- **Status check:** Plaintext on disk by design.
- **Blast radius:** Sensitive conversations stored in the clear on
  shared / cloud-mounted storage.

### Restore for files mevedel never touched

- **Source:** `specs/19-session-persistence.md` "Out of scope"
- **What's owed:** Rewinding side effects from Bash, manual edits, or
  files outside the snapshot map.
- **Why deferred:** Would require git/filesystem-watch integration;
  scope creep beyond LLM-driven edits.
- **Status check:** `mevedel-session-persistence.el` only restores
  files in the turn's `file-snapshots` map.
- **Blast radius:** Mixed sessions (LLM + Bash + manual edits) cannot
  be cleanly rewound.

### Compaction-summary chain in sidecar

- **Source:** `specs/19-session-persistence.md` Q1
- **What's owed:** `:compaction-summaries` entry on `session.meta.el`
  caching the chain of summaries so a "compaction timeline" UI can be
  built without parsing segment files.
- **Why deferred:** Adds redundancy with segment bodies; punted until
  a UI consumer needs it.
- **Status check:** Sidecar carries `:prompt-index` but no summary
  chain.
- **Blast radius:** Compaction-history UI would have to parse segment
  files on demand.

## Sub-agents, coordinator, and verification

### Sub-agent recovery on crash / resume

- **Source:** `specs/21-agent-transcript-persistence.md` "Non-goal:
  Recovery"
- **What's owed:** Reconstructing in-flight sub-agent FSMs, replaying
  undelivered mailbox messages, and re-arming task overlays on
  `mevedel-resume`.
- **Why deferred:** v1 transcripts are audit-log only; recovery
  requires idempotent FSM rebuild and is its own design.
- **Status check:** On resume, sub-agents with status `running` are
  rewritten to `incomplete`; no FSM reconstruction.
- **Blast radius:** Lost intermediate work if Emacs dies mid-agent;
  user restarts manually.

### Sub-agent reasoning capture

- **Source:** `specs/21-agent-transcript-persistence.md` lines 114–120
- **What's owed:** Optional inclusion of reasoning chunks in sub-agent
  transcripts with a per-agent toggle.
- **Why deferred:** Storage and sensitivity implications need their
  own design.
- **Status check:** `mevedel-agent-exec--run` hard-codes
  `:include-reasoning nil`.
- **Blast radius:** Audit logs lack the model's reasoning trail;
  harder to debug agent decisions.

### Agent transcript search / global management

- **Source:** `specs/21-agent-transcript-persistence.md` line 110
- **What's owed:** Cross-session search and bulk-management commands
  for saved agent transcripts.
- **Why deferred:** Per-session viewing was the v1 surface; indexing
  is its own scope.
- **Status check:** `mevedel-view-open-agent-transcript` opens a
  single saved file; no search UI.
- **Blast radius:** Discovering past agent work requires walking
  `.mevedel/sessions/<id>/agents/` by hand.

### Live transcript follow during a running agent

- **Source:** `specs/21-agent-transcript-persistence.md` lines 122–123
- **What's owed:** `tail -f`-style live view of a sub-agent transcript
  while it's still running.
- **Why deferred:** Renderer opens finalized files only; live view
  needs buffer-watch plumbing.
- **Status check:** Not wired.
- **Blast radius:** Long sub-agent runs are opaque until completion.

### File rollback to sub-agent-local turns

- **Source:** `specs/21-agent-transcript-persistence.md` line 109
- **What's owed:** Restoring a file to its state at an internal turn
  inside a sub-agent's conversation, not just at parent-turn
  boundaries.
- **Why deferred:** Snapshots are keyed to parent turns; sub-agent
  granularity needs a separate snapshot strategy.
- **Status check:** Restore operates on parent-turn snapshots only.
- **Blast radius:** Mid-run rollback inside a file-heavy sub-agent
  isn't possible.

### Live status indicators for sub-agents

- **Source:** `specs/14-chat-view.md` line 300
- **What's owed:** Per-agent progress indicators in the view (e.g.,
  "Explorer: 3/10 turns").
- **Why deferred:** Polish enhancement after the dual-buffer split.
- **Status check:** View shows generic status; no agent-specific
  turn counters.
- **Blast radius:** Coordinator-style workflows look idle until a sub-
  agent finishes.

### Coordinator prompt-cache sharing

- **Source:** `specs/13-coordinator.md` "Future work";
  `COORDINATOR-MODE.md` design notes
- **What's owed:** Thread the parent's pre-rendered system prompt
  into worker sub-agents so they reuse the same Anthropic prompt-
  cache prefix instead of re-deriving.
- **Why deferred:** Needs gptel cooperation to inject a pre-rendered
  prompt; mevedel's own prompt-section memoization helps but is not
  cross-worker cache sharing.
- **Status check:** `mevedel-system--prompt-section-cache` exists;
  workers still build their own prompt strings.
- **Blast radius:** Multi-worker fan-out pays full input cost on each
  worker.

### Cross-session mailbox

- **Source:** `specs/13-coordinator.md` "Future work"
- **What's owed:** Persistent mailbox (e.g., `~/.claude/teams/...`)
  for agents in different sessions to exchange messages.
- **Why deferred:** In-process mailbox is sufficient for single-
  session coordinator; cross-session is its own design.
- **Status check:** Mailbox lives in-memory per session.
- **Blast radius:** No coordination across chat buffers / projects.

### Team mode

- **Source:** `specs/13-coordinator.md` "Future work"
- **What's owed:** Multiple coordinator sessions collaborating with
  shared context and cross-session message delivery.
- **Why deferred:** Significant new mode; layered on top of the
  cross-session mailbox.
- **Status check:** Not present.
- **Blast radius:** Coordinator is single-user / single-session only.

### Git worktree isolation for parallel workers

- **Source:** `specs/01-structs-workspace-session.md` "Git worktree
  isolation (future)"
- **What's owed:** `git worktree add` plumbing so coordinator workers
  can edit the same files in parallel without stomping each other.
- **Why deferred:** Not required for serialized coordination; lifts
  scaling ceiling rather than fixing a bug.
- **Status check:** No worktree management in the workspace code.
- **Blast radius:** Parallel workers must serialize on conflicting
  file edits.

## Compaction (spec 27 staged follow-ups)

### Between-agent-step compaction

- **Source:** `specs/27-compaction-rework.md` §3.2
- **What's owed:** Compaction predicate check at the start of each
  agent step (after a tool result lands, before the next request).
- **Why staged:** v1 ships pre-send compaction only; needs a pausable
  FSM gate after `mevedel-tools--task` and before WAIT.
- **Status check:** Pre-send is wired through gptel's prompt
  transformer; no between-step gate.
- **Blast radius:** Long agentic loops can overflow context mid-flight;
  user must compact manually.

### Mid-stream compaction abort

- **Source:** `specs/27-compaction-rework.md` §3.3
- **What's owed:** Detect overflow during streaming, cancel the
  request, compact, inject a continuation message, retry.
- **Why staged:** Needs stream cancellation, partial-response cleanup,
  and gptel streaming-token hooks.
- **Status check:** No mid-stream monitoring.
- **Blast radius:** Streams that would exceed context fail at the API
  boundary with full response loss.

### Oversized-compaction-body retry

- **Source:** `specs/27-compaction-rework.md` §4.3
- **What's owed:** Two-tier retry when the compaction request itself
  would overflow (drop tool-output budget, then drop further; fail
  loudly if neither fits).
- **Why staged:** v1 assumes the source buffer fits in context.
- **Status check:** No retry; oversized bodies are sent as-is and
  rejected by the API.
- **Blast radius:** Auto-compaction silently fails on huge tool-result
  bodies.

### Per-tool-output truncation in preserved tail

- **Source:** `specs/27-compaction-rework.md` "Tail preservation"
  (defcustom `mevedel-compact-tail-tool-output-max`)
- **What's owed:** Truncate oversized tool results inside the tail
  (head + tail bytes with a `[... N chars truncated ...]` marker)
  while preserving message boundaries.
- **Why staged:** v1 keeps the tail byte-for-byte.
- **Status check:** Defcustom exists (default 4000); the truncation
  logic is not wired.
- **Blast radius:** Multi-megabyte tool results in the tail bloat
  context after compaction.

### Sub-agent auto-compaction

- **Source:** `specs/27-compaction-rework.md` §3.1 (opt-out for
  sub-agents)
- **What's owed:** Auto-gate participation for sub-agent buffers once
  they have independent persisted segments.
- **Why staged:** Pre-send gate keys on the session's current segment
  path, which sub-agents don't own.
- **Status check:** Pre-send gate skips sub-agent buffers.
- **Blast radius:** Long sub-agent runs can overflow independently of
  the parent session.

### Chat input queueing during compaction

- **Source:** `specs/27-compaction-rework.md` "Non-goals"
- **What's owed:** Queue user input while auto-compaction is in
  flight instead of blocking the input area.
- **Why deferred:** Synchronous block is acceptable for the typical
  5–30s window.
- **Status check:** `mevedel--compaction-in-flight` blocks input.
- **Blast radius:** User cannot type while a compaction runs.

## Tools

### Glob `--no-ignore` semantics

- **Source:** 2026-05-12 comparison with ccs and Codex search/glob
  behavior.
- **What's owed:** Reconsider adding `--no-ignore` to the Glob tool so
  ignored-but-relevant agent/project files can be discovered. If adopted,
  pair it with guardrails such as internal exclusions for noisy runtime
  dirs (`.git/`, `.mevedel/sessions/`, `.mevedel/tool-results/`) and
  regression tests for ignored hidden files.
- **Why deferred:** Current Glob keeps developer-default gitignore
  semantics. ccs uses `--hidden --no-ignore`, and Codex uses the same
  flags where completeness matters, but mevedel needs a bit more filtering
  work before copying that globally.
- **Status check:** `mevedel-tool-fs--glob` passes `--hidden` but not
  `--no-ignore`; results are capped at 100 entries.
- **Blast radius:** Glob may miss ignored local metadata such as project
  agent files, but broad searches avoid ignored generated/cache/runtime
  noise for now.

### Bedrock backend support for deferred tool loading

- **Source:** `mevedel-tools.el:273` (comment); `DEFERRED-TOOLS.md`
  research notes
- **What's owed:** Inject deferred tools into Bedrock's
  `(:toolConfig :tools)` nesting alongside the existing
  OpenAI/Anthropic/Ollama/Gemini paths.
- **Why deferred:** Different data accessor; not yet exercised.
- **Status check:** Comment explicitly says "not yet supported".
- **Blast radius:** Bedrock users miss the deferred-tool context
  optimization.

### Per-session deferred-tool registry

- **Source:** `DEFERRED-TOOLS.md`
- **What's owed:** Move `mevedel-tools--deferred-registry` from a
  global to per-session state for true concurrent multi-session use.
- **Why deferred:** Single-session usage doesn't trip on it.
- **Status check:** Registry is global.
- **Blast radius:** Concurrent sessions in one Emacs may interleave
  deferred-tool state.

### `:render-transform` escape hatch on wrapped tools

- **Source:** `specs/17-auto-approve-preview.md` lines 62–64
- **What's owed:** Keyword on `mevedel-define-tool :wrap` that lets
  mevedel synthesize structured `render-data` from upstream string
  output (gptel, gptel-agent, MCP).
- **Why deferred:** Concrete use cases were unclear at spec 17 time.
- **Status check:** No `:render-transform` slot in the tool struct or
  macro.
- **Blast radius:** Wrapped tools fall back to string-based
  rendering.

## Skills and slash commands

### Skill `effort` apply path

- **Source:** `specs/22-skill-refinement.md` "Effort — deferred"
- **What's owed:** Map skill `effort` overrides to backend-specific
  knobs (Anthropic extended-thinking budget tokens, OpenAI
  `reasoning_effort`, no-op + warning for others).
- **Why deferred:** Blocked on gptel exposing an effort/reasoning
  control.
- **Status check:** Slot is populated; `mevedel-skills.el:1592`
  raises a `display-warning` that the value is "stored but currently
  inert".
- **Blast radius:** `effort: high` skills do not actually escalate.

### Live skill discovery refresh

- **Source:** `specs/22-skill-refinement.md` "Discovery refresh"
- **What's owed:** File-notify-driven re-scan of skill directories
  with a throttle defcustom, merge rules preserving `active-p`, and
  an explicit `/refresh` command.
- **Why deferred:** Over-scoped spec 22; pulled into its own future
  spec.
- **Status check:** Watcher plumbing exists at
  `mevedel-skills.el:864` but doesn't trigger a re-scan; explicit
  refresh command not wired.
- **Blast radius:** Adding/editing SKILL.md files requires session
  restart.

## Permissions and modes

### Verify `:trust-literal-p` on skill-body Bash expansions

- **Source:** `specs/22-allowed-tools-detail.md` "Shell injection"
- **What's owed:** Confirm `mevedel-tools--check-bash-permission`
  applies the `:trust-literal-p` carve-out when expanding skill
  ` !`...`` and ```` ```! ```` blocks (deny rules + protected paths
  still honored).
- **Why deferred:** Plumbing was scoped into spec 22 but the audit /
  test pass for skill-body shell wasn't separately confirmed.
- **Status check:** Flag exists on the permission helper; needs an
  end-to-end verification that skill bodies actually pass it through.
- **Blast radius:** Author-written shell expansions in skill bodies
  may surface dangerous-command prompts they shouldn't.
