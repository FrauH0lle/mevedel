# Multi-agent system

Agents declared with `mevedel-define-agent`:

- **explorer**: read-only investigation, caller-specified thoroughness
- **planner**: interactive planning via `PresentPlan`
- **coordinator**: orchestrates workers via `Agent(run_in_background=true)`;
  never implements
- **verifier**: adversarial read-only verification; per-turn
  `verifier-read-only` reminder attached at invocation. Final reports must
  end with `VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL`; the
  parsed verdict is stored in transcript render-data for the handle badge.

Each agent's `:tools` resolved via `mevedel-tool-resolve-gptel` at
invocation time. Registered buffer-locally via `gptel-agent--agents` per
request (no caching). Each invocation gets a cloned reminder list with
independent `last-fired`.

Agent definitions may include `:hooks` using the same declarative hook
shape as project hook files. These rules are scoped to invocations of that
agent and are folded into the agent invocation layer before skill-scoped
hook rules for fork skill invocations. Within an agent definition, `Stop`
means "when this sub-agent stops" and is normalized to `SubagentStop`;
top-level `Stop` remains reserved for the main assistant turn.

Agent prompts are built from the agent's own prompt file plus selected
system sections. `:include-workspace-config`, `:include-memory`, and
`:include-environment` control whether AGENTS.md/CLAUDE.md, persistent
memory, and environment details are appended. Utility agents can therefore
avoid inheriting main-agent boilerplate while still receiving environment
context.

## Background spawning

`run_in_background` makes `mevedel-tools--task` call
`process-tool-result` immediately with a launch-status string,
unblocking the parent FSM. The sub-agent completes fire-and-forget; its
result is wrapped in `<agent-result>` and pushed to the parent's mailbox.
When the LLM produces no tool calls but background agents are still
running, the FSM parks in **BWAIT** instead of terminating. Completion
resumes BWAIT→WAIT. Transition injection:
`mevedel-preset--inject-bwait-transitions` (main) and
`mevedel-tools--inject-bwait-transition` (sub-agent). `background-agents`
slot on session/invocation tracks running children.

Foreground-callback suppression: when a foreground agent has background
children, `mevedel-tools--task` stashes the result on the invocation's
`stashed-result` slot; `main-cb` is called once all children finish.

## Inter-agent messaging (SendMessage)

Fire-and-forget async messages. Aliases `"main"`, `"chat"`, `"coordinator"`
resolve to the main session mailbox; exact agent-id or `"<type>--"` prefix
match resolves to a sub-agent. Messages queue on the recipient's mailbox
and drain via `mevedel-tools--handle-message-inject` in WAIT state, wrapped
as `<agent-message from="SENDER">...</agent-message>` and injected as a
user turn via `gptel--inject-prompt`. Polymorphic accessor
`mevedel-tools--ctx-messages` dispatches on session vs invocation.

## Coordinator skill

Bundled at `skills/coordinator/SKILL.md` (discovered via
`mevedel-skills--bundled-dir`). `context: fork` delegates to the
coordinator agent. User/project skills in `~/.claude/skills/`,
`.claude/skills/`, or `.mevedel/skills/` override bundled skills by
name.

The coordinator prompt includes a continue-vs-spawn table for deciding
when to reuse a worker through `SendMessage` versus launching a fresh
worker, and requires synthesis before handoff: follow-up prompts should
name concrete files, constraints, and next actions rather than forwarding
research with vague wording.

## Transcript persistence and views

Each sub-agent invocation runs in its own gptel buffer. When session
persistence is enabled, that buffer is the transcript file under the
parent session's `agents/` directory. The parent session mirrors an
`agent-transcripts` alist into the sidecar with agent id, type,
description, path, status, timestamps, parent turn, and call count.

The main view renders agent handles from tool render-data and sidecar
state. Running handles can expand to show recent ephemeral activity.
Terminal handles open a rendered read-only transcript view via
`mevedel-view-open-agent-transcript`; normal UI does not open a running
transcript file.

The status zone can show aggregate live/recent agent rows so the user
can locate active handles without scanning the whole transcript.

## Task overlay

Tasks tracked per caller (main chat and each sub-agent separately).
`blockedBy` propagates completion. `mevedel-tools--agents-fsm`
(buffer-local on chat buffer) maps agent-id → sub-agent FSM for
SendMessage resolution.

## Model tiers

`mevedel-models.el` maps small tier names (`fast`, `balanced`,
`strong`) to concrete gptel backend/model providers. Agent definitions
use tier defaults; an Agent call or skill invocation can override the
model for that invocation without changing the parent session default.
