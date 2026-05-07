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

### Configurable max-turns at agent-spawn time

- **Source:** `future.md`
- **What's owed:** Agent tool parameter that lets the caller override
  the agent struct's `:max-turns` per spawn.
- **Why deferred:** Slot exists; only the tool plumbing is missing.
- **Status check:** `mevedel-agents.el` has the slot; the Agent tool
  in `mevedel-tool-ui.el` does not surface it.
- **Blast radius:** Coordinator can't tighten / loosen turn budgets
  ad hoc.

### Concurrency cap on background agents

- **Source:** `future.md`
- **What's owed:** Defcustom (e.g., `mevedel-max-concurrent-agents`)
  that queues spawns past the cap.
- **Why deferred:** Not yet a pain point in real workflows.
- **Status check:** No cap; spawns are unbounded.
- **Blast radius:** Large fan-outs can exhaust local resources / API
  rate limits.

### `:disallowed-tools` slot on agent definitions

- **Source:** `future.md`
- **What's owed:** Denylist slot mirroring CCS agent frontmatter so
  agents can say "all tools except X" instead of curating an
  allowlist.
- **Why deferred:** Optional refinement; allowlists work today.
- **Status check:** `mevedel-agent` struct has no
  `:disallowed-tools`.
- **Blast radius:** New agents must enumerate every allowed tool.

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

### Read tool: PDF support

- **Source:** `future.md`
- **What's owed:** Native PDF read via poppler with `:pages` budget.
- **Status check:** `mevedel-tool-fs.el` Read does not implement PDF.
- **Blast radius:** Users must convert PDFs externally.

### Read tool: image support

- **Source:** `future.md`
- **What's owed:** PNG/JPG/GIF/WEBP read with base64 + optional
  resize, gated on model-capability detection.
- **Status check:** Not implemented; depends on capability detection
  helpers that don't exist yet.
- **Blast radius:** Multimodal Read is unavailable.

### Read tool: file-not-found suggestions

- **Source:** `future.md`
- **What's owed:** Fuzzy matching on missing paths
  (`findSimilarFile` / `suggestPathUnderCwd` analogue).
- **Status check:** Read returns a flat "not found" error.
- **Blast radius:** More retry round-trips after typos.

### Read tool: Jupyter notebook support

- **Source:** `future.md` (flagged "very low priority")
- **What's owed:** `.ipynb` reading that flattens cells + outputs.
- **Status check:** Not implemented.
- **Blast radius:** Notebook contents must be exported first.

### Bash tool: interactive input / PTY

- **Source:** `future.md`
- **What's owed:** PTY-backed Bash with a permission-overlay UI for
  recent output, user input, interrupt, "keep waiting".
- **Why deferred:** Heavy UX work; timeout support should land first.
- **Status check:** Bash runs non-interactively only.
- **Blast radius:** Interactive commands hang or fail silently.

### Session-wide render-data budget

- **Source:** `future.md`
- **What's owed:** Aggregate cap on accumulated render-data with
  retroactive spill-to-disk for the oldest blocks.
- **Why deferred:** Per-tool `max-result-size` covers the dominant
  case until usage shows the aggregate matters.
- **Status check:** Per-tool persistence only.
- **Blast radius:** Long sessions with many medium-sized results
  bloat buffer memory.

## Skills and slash commands

### Hook execution runtime

- **Source:** `specs/22-skill-refinement.md` "Hooks — Deferred";
  `specs/11-skills.md` "Hooks — future work"
- **What's owed:** Execute SKILL.md `hooks` (PreToolUse, PostToolUse,
  SessionStart, FileChanged, ...). Today the field is parsed and
  stored only.
- **Why deferred:** Substantial subsystem; needs its own spec to fix
  hook record shape and execution model.
- **Status check:** `mevedel-skills.el` parses hooks into raw values
  on the skill struct; no execution path.
- **Blast radius:** Skill authors writing hooks see them silently
  ignored.

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

### Elisp injection in skill bodies (`!el`)

- **Source:** `future.md`
- **What's owed:** Parallel elisp injection path mirroring the shell
  one — inline `` !el`(emacs-version)` `` and fenced ```` ```!el ```` —
  with `:trust-literal-p` so author-written elisp bypasses the
  standard Eval prompt.
- **Why deferred:** Infrastructure is in place for shell; elisp path
  is its own dispatcher.
- **Status check:** Only shell injection is implemented.
- **Blast radius:** Skill authors can't lean on Emacs-native code in
  bodies.

### Skill template variables (`${CLAUDE_SKILL_DIR}` etc.)

- **Source:** `todo.org`
- **What's owed:** Environment-aware variable expansion in skill
  argument templates (analogue of CCS's substitution variables).
- **Status check:** `mevedel-skills-invoke` does basic argument and
  shell substitution; no env-style variables.
- **Blast radius:** Skills can't reference standard tool / skill
  directories portably.

### `/auto` slash command

- **Source:** `future.md`
- **What's owed:** Toggle command that flips `mevedel-permission-mode`
  to `trust-all`, registers an auto-mode reminder, and reverts on
  second invocation.
- **Status check:** Permission modes exist; no `/auto` command.
- **Blast radius:** No quick way to flip into autonomous mode from
  chat.

### `/clear` slash command

- **Source:** `todo.org`
- **What's owed:** Start a fresh segment from chat (interactive
  immediate compaction-style reset).
- **Status check:** No `/clear` handler.
- **Blast radius:** Users must restart the session to reset context.

### `/review` slash command (Codex-inspired)

- **Source:** `todo.org`; `review-command-research.md`
- **What's owed:** Dedicated review sub-session with picker UI,
  rubric, and structured verdict — see the research note for the
  intended shape.
- **Status check:** Not implemented.
- **Blast radius:** No first-class affordance for adversarial review
  agents.

### Internal documentation skill

- **Source:** `future.md`
- **What's owed:** Bundled skill that teaches the model about
  mevedel's own features.
- **Status check:** Not bundled.
- **Blast radius:** New users / agents have to learn mevedel from
  CLAUDE.md and source instead of an in-skill primer.

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

### Session-wide YOLO override

- **Source:** `todo.org`
- **What's owed:** Single permission-mode toggle that bypasses every
  prompt (except protected paths), subsuming today's per-call
  `:trust-literal-p` flags so skills and bare tools share one bypass.
- **Why deferred:** Permission redesign; touches the 9-step decision
  chain.
- **Status check:** `:trust-literal-p` exists per call; no global
  bypass mode.
- **Blast radius:** High-trust setups still see repetitive prompts;
  skill and tool bypass paths are inconsistent.

## View, transcript, and UX

### Distinguish main-session vs. agent tasks in the view

- **Source:** `todo.org`
- **What's owed:** Visually separate the task-overlay list so the
  user sees which tasks belong to the parent session and which were
  spawned by sub-agents.
- **Status check:** Tasks live on the session struct; the view
  renders a single mixed list.
- **Blast radius:** Long coordinator sessions have an unreadable
  task list.

### Suppress org-lint in data buffer / `so-long-mode`

- **Source:** `todo.org`
- **What's owed:** Disable org-lint reporting in the chat data
  buffer and when `so-long-mode` activates.
- **Status check:** No suppression in `mevedel-chat.el` setup.
- **Blast radius:** Spurious org-lint warnings clutter `*Messages*`.

### Message queueing during active requests

- **Source:** `future.md`
- **What's owed:** Buffer user prompts sent while a request is in
  flight and deliver them once the FSM returns.
- **Why deferred:** Lower priority than other UX work; needs FSM
  cooperation.
- **Status check:** New sends during a request are blocked or error.
- **Blast radius:** Users can't queue follow-up thoughts.

### Reference navigation by tag query

- **Source:** `future.md`
- **What's owed:** Jump from a chat mention to instructions matching
  a tag query.
- **Status check:** Overlays carry tags; no navigation command.
- **Blast radius:** Tag-based discovery is read-only.

### Tag autocompletion in directive text

- **Source:** `future.md`
- **What's owed:** `completion-at-point` over the tag-query syntax
  while authoring directives.
- **Status check:** No CAPF for tag queries.
- **Blast radius:** Tag-query syntax has no discoverability.

## Architecture and housekeeping

### Memory-system rework

- **Source:** `future.md`; `memory-system-research.md`
- **What's owed:** Move `.mevedel/memory/MEMORY.md` toward CCS-style
  taxonomy (typed memories, staleness caveats, extraction agent,
  consolidation).
- **Why deferred:** Single-file approach is good enough today;
  research note captures the target design.
- **Status check:** Single `MEMORY.md`, first 200 lines loaded; no
  taxonomy or extraction agent.
- **Blast radius:** Memory bloats and goes stale without structured
  recall.

### System-prompt composition for prompt-cache hits

- **Source:** `future.md`
- **What's owed:** Replace `{{TOKEN}}` string replacement with
  composed snippet functions (and use gptel-agent templating for
  agents) so the cache prefix is stable across requests.
- **Status check:** `mevedel-system-build-prompt` builds eagerly;
  prompt-section memoization is a partial win, not full cache-aware
  composition.
- **Blast radius:** Higher input cost; weaker prompt-cache hit rate.

### Simplify presets/agents request-time wiring

- **Source:** `future.md`
- **What's owed:** Reduce the chain of
  `mevedel-agents--setup-for-request` →
  `mevedel-preset--setup-deferred` → `mevedel-preset--build-handlers`
  inside `gptel-with-preset`.
- **Why deferred:** Works today; readability tax.
- **Status check:** Three-step setup still in place.
- **Blast radius:** Maintenance friction; harder to onboard the
  request setup flow.

### Audit `require` / `declare-function` conventions

- **Source:** `future.md`
- **What's owed:** Establish and apply a consistent convention across
  the codebase; fix the kind of latent loading-order bug that forced
  the explicit `require` for `mevedel-view--ensure`.
- **Status check:** Ad-hoc per file.
- **Blast radius:** Subtle order-of-load bugs as new modules land.

### Polymorphism for reminder / skill variants

- **Source:** `future.md`
- **What's owed:** If reminders or skills genuinely fan out into
  several variants, lift them onto `cl-defgeneric` /
  `cl-defmethod` instead of branching on type fields.
- **Why deferred:** Not yet enough divergence to justify retrofit.
- **Status check:** Flat structs with type-field branches.
- **Blast radius:** Future variants will widen the branches further.

### Relocate structs nearer to their consumers

- **Source:** `future.md`
- **What's owed:** Review whether `mevedel-structs.el` should split
  along ownership lines.
- **Status check:** All `cl-defstruct` definitions live centrally.
- **Blast radius:** Locality cost; cross-file bouncing during
  maintenance.
