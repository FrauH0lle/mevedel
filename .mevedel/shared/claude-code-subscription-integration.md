# Claude Code subscription integration — research (2026-10-06)

> Historical research, retained as evidence. The current implementation
> contract is [the ready-for-agent PRD](../../.scratch/claude-code-engine/PRD.md);
> [the revised handoff](claude-code-engine-handoff.md) provides the Goal command.
> Subsequent discussion selected option two through **ACP plus MCP**, with a
> focused feasibility slice followed by full implementation. The comparative
> option-one bake-off and undecided/default CLI transport below are superseded.
> Memory consolidation and side conversations are not tool-free. Separate
> directive history, compaction-time context restoration and reconciliation of
> external history with mevedel persistence are now explicit requirements.
> Source/version observations below are dated research, not proof of current
> installed behavior. No implementation is claimed by this document.

Question: can mevedel drive Claude Code (ACP via acp.el, or similar) so users
spend their Claude Pro/Max subscription instead of API keys? How does penecho
do it?

## Goal

Use a Claude Code subscription *in mevedel*. Ideally the user selects a
"Claude Code" backend the same way they select any gptel backend/model, and
the rest just works: no MCP configuration, sockets, tool allowlists, or
prompt setup by hand. The only user prerequisite is an installed `claude`
that is logged in.

Acceptance, as seen by the user:
- Claude Code appears in the normal backend/model selection (presets, model
  tiers); Claude models map to its model choice.
- Sessions work end to end: send, stream, tool calls with mevedel permission
  prompts and patch review, interrupt, save, reopen, continue.
- Subagents started in a Claude Code session also run on Claude Code.
- Features that cannot work on this backend are disabled visibly, not left
  silently half-working.
- mevedel generates and wires everything per session: MCP bridge, server id,
  tool set, system prompt, permission mode.

## Verdict

Yes, legally and technically — but only by letting the user's installed,
unmodified Claude Code own the model call. mevedel cannot get the subscription
into gptel. The workable shape is **"Claude Code owns the loop, mevedel owns
the tools"**: Claude Code's built-in tools disabled, mevedel's tools served to
it over MCP, so every effect still passes through `mevedel-pipeline-run-tool`.
That needs a new turn-engine seam; mevedel currently has none (gptel FSM is
load-bearing everywhere).

## Policy (what is allowed)

- Allowed: spawning the user's own `claude` / Agent SDK / ACP adapter where
  login completes through Anthropic's flow. Support article (2026-06-16):
  "Claude Agent SDK, `claude -p`, and third-party app usage still draw from
  your subscription's usage limits."
  https://support.claude.com/en/articles/15036540-use-the-claude-agent-sdk-with-your-claude-plan
  Zed confirms ACP usage is covered: https://zed.dev/blog/anthropic-subscription-changes
- Forbidden and server-side blocked since 2026-01-09: reading OAuth tokens
  (`~/.claude`, `setup-token`) and calling the Messages API directly. Legal
  page: developers "may not collect, store, or intermediate Claude.ai
  credentials or session tokens".
  https://code.claude.com/docs/en/legal-and-compliance
  → A gptel "Anthropic OAuth" backend (analogous to `gptel-openai-oauth`) is
  off the table.
- Caveats: the Agent SDK overview says third parties may not "offer claude.ai
  login ... unless previously approved" (aimed at products brokering login;
  mevedel never touches login). Limits "assume ordinary, individual usage".
  Billing policy changed three times in 2026; a paused plan would move
  third-party SDK usage to a monthly credit.

## Transports

| Option | Notes |
|---|---|
| ACP: `acp.el` + `@agentclientprotocol/claude-agent-acp` (v0.86, Apache-2.0, wraps Agent SDK) | Structured `session/update`, `session/request_permission`, load/resume/fork, modes, model switch. `session/new` `mcpServers` injects tools; `_meta.claudeCode.options` passes SDK options (`tools: []`, `allowedTools`, `hooks`, `agents`); `_meta.systemPrompt` string replaces the system prompt. Uses existing `claude` login. Costs: npm dependency; acp.el (GPL-3, v0.15) says API not stable; no `terminal/*` support. Built-in tools execute inside Claude Code, never via client `fs/*`. |
| Headless: `claude -p --input-format stream-json --output-format stream-json` | No npm adapter. Flags: `--tools ""`, `--mcp-config`, `--permission-prompt-tool`, `--system-prompt`, `--resume`. stream-json input carries user messages only — prior assistant/tool turns cannot be replayed except as text. |

Disabling built-ins is agent-specific (`_meta`), so ACP does not make a
"generic agent" engine for free; Codex/Gemini need their own knobs.

## How penecho does it (penecho@f64a8be, v1.3.5)

Three adapters behind one internal `LlmAdapter`; each reuses the user's CLI
login (`claude auth status` preflight), never handles tokens.

- **Claude: tool-free transport.** `src/providers/claude-cli.js` spawns a new
  `claude -p` per model step with `--tools "" --disallowedTools Agent,Task
  --strict-mcp-config --no-session-persistence --system-prompt ...`; aborts if
  `system/init` lists any tool or a `tool_use` appears. Tool calls are emulated
  as bare JSON text (`cli-adapter.mjs`: `{"type":"tool_call",...}` with one
  repair retry). Its own harness keeps history and compaction; full history
  is resent every step (capped at 500k chars). Costs: no native tool calling,
  JSON repair failures, process start latency per step.
- **Codex: native loop, host tools.** `codex app-server` JSON-RPC with
  `dynamicTools`, builtins disabled, `approvalPolicy: never`. Codex owns
  history, compaction, and cache identity; penecho executes and validates
  tools. This is the shape that maps best to Claude Code + MCP — penecho does
  not do it for Claude.
- **Kimi: ACP**, always rejecting `session/request_permission`.
- Reverse direction: `penecho mcp` lets the user's own Claude Code drive
  penecho as a tool provider.

## Options for mevedel

1. **Tool-free transport (penecho/Claude style)** as a process-backed gptel
   backend. Keeps the gptel loop and almost all of mevedel. But tool calls
   become a text protocol, history is flattened into one user message,
   provider-fragment replay (ADR 0115) and thinking blocks are lost, and every
   step pays process startup. Lowest integration cost and lowest quality; it
   fights mevedel's native-tool design.
2. **Native loop, mevedel tools over MCP (recommended if pursued).** Claude
   Code via ACP (or stream-json) with `tools: []`, `allowedTools:
   ["mcp__mevedel__*"]`, permission mode letting MCP tools through, and the
   system prompt replaced by mevedel's assembled prompt. Tool calls land in
   `mevedel-pipeline-run-tool`, so permissions, execution target/TRAMP,
   sandbox, snapshots, ApplyPatch review, and rewind file state survive.
   Needs:
   - an MCP server exposing registry tools (mevedel has only an MCP *client*
     today; stdio shim → Emacs, or reuse an Emacs MCP server library);
   - a turn-engine seam: a session's turn is driven by gptel FSM *or* an
     external agent session;
   - translation of `session/update` into the gptel Org data-buffer grammar
     (`(tool . id)` spans) so views, codec, collaboration projection work;
   - decisions for what Claude Code now owns: compaction, prompt cache,
     conversation fork/resume (map onto ACP `session/fork`/`load`), model
     choice (Claude only, no per-tier backends).
   Lost or degraded: WAIT-seam reminders and context delivery (could move into
   tool results or Claude Code hooks), steering mid-turn, end-at-boundary,
   goal continuation, mevedel compaction/telemetry, plan-mode tool filtering
   (rebuild via allowed tool list). Agents: mevedel's Agent tool over MCP
   would still need an engine for children — another Claude Code session each.
3. **Just a front-end** (agent-shell style, Claude built-ins on). Cheap, but
   bypasses mevedel's pipeline entirely; agent-shell already does this.

## Existing Emacs MCP servers (checked 2026-10-06)

- **mpontus/emacs-mcp** (last commit 2025-06-10, one 617-line file): runs in
  a *separate* `emacs --batch` started by the MCP client. Cannot reach the
  live session, permission queue, or buffers. Not usable.
- **rhblind/emacs-mcp-server** (GPL-3, last commit 2026-05-04): runs inside
  the live Emacs on a Unix socket or TCP; the client starts a `socat` /
  shell / Python bridge as its stdio MCP command. Right process shape. Gaps
  for mevedel:
  - tool calls are synchronous (`mcp-server-tools-call` funcalls the
    handler and sends the result immediately); mevedel tools complete via
    callback after permission prompts and async execution;
  - one global registry and transport, with a name-only global filter; no
    per-session or per-agent tool sets or call→session routing;
  - protocol `2024-11-05`, tools only, no `notifications/cancelled`;
  - ships its own tools (eval-elisp, org) and a security layer to switch off.
  Fine for a throwaway prototype (block on the pipeline callback); for a real
  integration, either upstream async + per-client tool sets or a thin
  mevedel-owned server reusing its socket-plus-bridge pattern. The MCP
  protocol surface needed (initialize, tools/list, tools/call, cancel) is
  small; the hard part is mevedel-specific routing and async.
- **laurynas-biveinis/mcp-server-lib.el** (GPL-3, MELPA, last commit
  2026-10-05, protocol `2025-03-26`, large ERT suite and ERT helpers): a
  library, not a server with tools. Strengths: several servers keyed by
  `server-id` (passed as `--server-id=` to the stdio script), so a
  per-session server with its own tool set is possible; resources too.
  Gaps:
  - transport is `emacs-mcp-stdio.sh`: a shell loop running one
    `emacsclient -e` per JSON-RPC line and printing the returned string.
    Synchronous by design; requests are serialized.
  - a blocking wait inside the emacsclient eval runs in the server process
    filter, where the command loop does not run, so a permission prompt
    needing user input cannot be answered without `recursive-edit`.
  - `notifications/cancelled` is ignored; no server→client messages.
  - handlers must be named functions; params come from the arglist and an
    `MCP Parameters:` docstring block, with `:param-schemas` per param.
    mevedel's registry has whole JSON schemas, so it would generate defuns.
  Usable for a prototype with auto-approved tools; not for the real thing.

Conclusion: none of the three fits as-is. A thin mevedel-owned server on a
Unix socket (`make-network-process :server t`) plus a `socat`-style bridge,
replying from the pipeline callback, is the cleanest real option.

## Option 1 vs option 2

| | 1. Model pipe (`claude -p`, tools off, text tool protocol) | 2. Claude Code loop, mevedel tools over MCP |
|---|---|---|
| mevedel changes | Process-backed gptel backend + text tool-call parser; rest unchanged | MCP server + ACP client + turn-engine seam + transcript translation |
| mevedel features kept | Nearly all (loop, reminders, agents, compaction, steering, presets) | Tool pipeline (permissions, targets, sandbox, snapshots, review); prompt-side features need rework |
| Tool calling | JSON text in the reply; escaping/repair failures, worst for large ApplyPatch bodies; no native parallel calls or structured tool results | Native `tool_use`, the mode Claude is trained for |
| History | Flattened into one user message per step (stream-json input takes user messages only); no thinking continuity; ADR 0115 replay moot | Claude Code keeps the real conversation |
| Prompt cache | Must be engineered and verified (block boundaries, stable serialization) | Claude Code's own, as designed |
| Latency | New `claude` process per model step | One long-lived process per session |
| Policy posture | Allowed `claude -p` use, but the "subscription as raw model proxy" pattern | Ordinary Claude Code + MCP use |
| Where failures live | Every step of the core loop, not fixable from mevedel | Feature edges, recoverable one at a time |

Variant of 1: a local Messages-API proxy wrapping `claude -p` so gptel's
Anthropic backend works unchanged. Same fidelity costs, and it most plainly
"routes requests through plan credentials"; avoid.

Option 1 is the only way to keep mevedel exactly as it is with a different
bill. Its penalties come from penecho's experience and how Claude is trained,
not from mevedel measurements, so a bake-off is warranted.

## Standalone value of option 2's parts

- MCP server alone: any MCP-capable agent the user already runs (Claude Code
  TUI, Codex, Cursor) can use mevedel tools through the pipeline:
  permissions, execution targets, snapshots, ApplyPatch review. Needs no
  engine seam; penecho's `penecho mcp` is the same reverse direction. Natural
  first stage, and the prototype needs it anyway.
- ACP client: can drive other ACP agents (Codex, Gemini CLI, ...), but
  stripping built-ins and replacing the system prompt is agent-specific
  `_meta`, so each agent still needs its own adapter profile.
- Neither brings the engine seam, which stays the dominant cost.
- Measured against the goal, a standalone MCP server is a side benefit and a
  building block, not the deliverable: the user would still work in the
  Claude Code TUI, not in mevedel.

Implications of "select a backend and it just works":
- The engine choice must ride on the existing backend/model selection, so
  the seam sits below presets: a session whose resolved backend is Claude
  Code runs turns through the external engine; everything above stays put.
- Zero setup favors fewer moving parts: `claude -p` stream-json needs only
  the binary; ACP adds the npm `claude-agent-acp` adapter the user must
  install (or mevedel must manage).
- Everything per session is generated: bridge command, server id, tool list,
  system prompt, permission mode, Claude session id in the sidecar for
  resume.

## Engine seam

### What exists today

- A turn is a gptel request. Root turns build a gptel FSM in
  `mevedel-chat.el:1241` (`mevedel--gptel-send-request`) and
  `mevedel-directive-request.el:771`; agents in `mevedel-agent-exec.el`.
- `mevedel-request` (`mevedel-structs.el:719`) already has an `fsm` slot
  documented as "owning root gptel FSM, or nil for non-provider requests".
- Settlement (`mevedel-turn.el:974` `mevedel--complete-turn`, `:1017`
  `mevedel--fail-turn`) is keyed on the FSM and reads its info plist: record
  outcome, plan handoff, goal settle, token baseline, save, checkpoint,
  Stop hooks, permission-mode restore, end request, goal continuation,
  follow-up drain.
- Precedent: direct skill forks settle without a provider request by
  building a synthetic `gptel-make-fsm` carrying `:buffer` and
  `:mevedel-request` (`mevedel-skills-input.el:672`).
- Coupling: 63 `mevedel*.el` files reference gptel FSM or request APIs; the
  heaviest are `mevedel-turn.el` (41 `gptel-fsm-info`), `mevedel-goal.el`
  (30), `mevedel-compact.el` (17), `mevedel-tools.el`, `mevedel-reminders.el`,
  `mevedel-presets.el` (11 each), `mevedel-agent-exec.el` (10).
- The transcript is gptel's grammar in the Org data buffer: `(gptel
  response)` text and `(gptel . (tool . id))` tool blocks, written by gptel
  insertion functions under mevedel advice (`mevedel-gptel-stream-bridge.el`,
  `mevedel-tool-render-data.el`), parsed by `mevedel-transcript.el`.

### What the seam has to separate

Everything above "run this turn" is engine-neutral: composer, admission
(`mevedel-request-begin`), settlement steps, persistence, views, tool
pipeline, permissions. Everything inside the turn is engine-specific: how
the model is called, how tools are dispatched to the pipeline, who keeps the
model's context.

Turn engine operations (mevedel → engine):
- start a turn with the user input (plus attachments and turn-start context);
- cancel; queue input mid-turn (steering) where supported;
- set model; open, resume and fork the engine session; close.

Turn events (engine → mevedel):
- text and reasoning deltas;
- tool call started / finished (id, name, arguments, result);
- usage; context compacted;
- turn ended: success, error, or cancelled, with stop reason.

Each engine also declares which mevedel features it owns or lacks, so
callers disable them visibly instead of probing.

### Two ways to cut it

- **A. Synthetic FSM** (extends the skill-fork precedent). The Claude Code
  engine fabricates a gptel FSM as an info carrier and calls the existing
  settlement functions at the matching moments. Cheap, enough for a
  prototype. But it is a pun: handlers that read WAIT payloads, tool-use
  lists, or provider data see nothing meaningful, and every caller keeps
  reasoning in gptel states.
- **B. Request-keyed turns** (target). Settlement and turn-scoped state key
  on `mevedel-request` (or a turn struct); the gptel engine maps its FSM to
  the request and translates FSM transitions into the neutral events.
  Larger refactor of the heavy modules above, but callers stop needing to
  know which engine ran the turn.

Suggested: A for the prototype and bake-off; B if the Claude Code engine
ships. No compatibility layer between them.

### Selection ("select a backend and it just works")

- Register a "Claude Code" entry in mevedel's model list
  (`mevedel-models.el` tiers, presets) as a distinct backend type, so it
  appears where users already choose models. The send path resolves the
  engine from the selected backend; gptel never sends for it. Guard plain
  gptel buffers against selecting it.
- Claude model names map to Claude Code's model choice; tiers map to
  opus/sonnet/haiku.

### Engine responsibilities (Claude Code)

- Process lifecycle: one long-lived Claude Code session per mevedel session
  and per agent; start lazily, kill on close, resume by stored session id
  (request-config sidecar, ADR 0110) after Emacs restart.
- Isolation: run in a neutral working directory with no setting sources and
  `--strict-mcp-config`, so the user's CLAUDE.md, hooks, skills, and MCP
  servers do not fire a second time; mevedel already supplies workspace
  guidance through its own prompt.
- Tool routing: a per-session MCP server id exposes exactly the session's
  (or agent's) tool set; each call binds to the active `mevedel-request` and
  origin agent path and runs `mevedel-pipeline-run-tool`, answering from its
  callback. Plan-mode tool changes need `tools/list_changed` (verify Claude
  Code honours it) or a session restart.
- Execution target: the `claude` process always runs locally; all effects
  go through mevedel tools, which already respect the session's TRAMP
  target and sandbox.
- Transcript: write the same gptel grammar the gptel engine produces, so
  views, codec, rewind and collaboration need no second format. For these
  sessions the data buffer is the record, not the model's context; Claude
  Code's session is. User edits to earlier transcript text cannot reach the
  model and must be blocked or flagged.

### Feature mapping

| Feature | Claude Code engine |
|---|---|
| Settlement, save, checkpoint, Stop hooks, goal settle | Work once keyed off the request (B) or carried (A) |
| Turn-start context and reminders | Work: appended to the `session/prompt` user content. Acknowledgement can no longer be checked against the realized payload; treat Claude Code compaction events as context loss and redeliver all sections |
| Mid-turn reminders (WAIT seam) | Ride on MCP tool results, or unsupported |
| Steering | Depends on mid-turn input support; verify for ACP and stream-json |
| End at boundary | Cancel after the current tool result settles |
| Compaction, prompt cache, fragment replay | Owned by Claude Code; mevedel's disabled for the session |
| Usage telemetry | From result usage events |
| Agents | Agent runner goes through the same seam; each child is its own Claude Code session with its own MCP server id and tool set |
| Directives | `mevedel-directive-request.el` builds its own FSM today; same seam |
| Conversation rewind/fork | Map onto Claude Code session fork/resume at a message; file state already comes from mevedel snapshots |

## Suggested next step

Bake-off on a fixed task set: option 1 via a `claude -p` text-protocol shim,
and option 2 as a throwaway prototype: `claude -p`
stream-json with `--tools ""` and `--mcp-config` pointing at a minimal stdio
bridge exposing Read/Grep/ApplyPatch through `mevedel-pipeline-run-tool`.
Measure tool-call fidelity, latency, cache hits (`usage` in `result`), and
whether a replaced system prompt behaves. If acceptable, decide ACP vs
stream-json and write an ADR for the engine seam.

## Sources

- acp.el https://github.com/xenodium/acp.el · agent-shell https://github.com/xenodium/agent-shell
- ACP spec https://agentclientprotocol.com/protocol/overview
- claude-agent-acp https://github.com/agentclientprotocol/claude-agent-acp
- Headless/CLI https://code.claude.com/docs/en/headless · https://code.claude.com/docs/en/cli-reference
- penecho https://github.com/penecho/penecho (`src/providers/claude-cli.js`,
  `src/server/canvas-agent/cli-adapter.mjs`, `codex-native-host.mjs`,
  `src/providers/kimi-acp.js`, `docs/canvas-agent-deepseek-harness-spec.md`)
- Other Emacs: claude-code.el, claude-code-ide.el (Emacs MCP tool server +
  IDE WebSocket protocol), monet, claudemacs — all leave auth to the binary.
