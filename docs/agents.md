# Multi-agent system

Agents declared with `mevedel-define-agent`:

- **explorer**: read-only investigation, caller-specified thoroughness
- **coordinator**: orchestrates workers via `Agent(run_in_background=true)`;
  never implements
- **verifier**: adversarial read-only verification; per-turn
  `verifier-read-only` reminder attached at invocation. Final reports must
  end with `VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL`; the
  parsed verdict is stored in transcript render-data for the handle badge.
- **reviewer**: foreground code-review agent used by `/review`; per-turn
  `reviewer-read-only` reminder attached at invocation. Reads diffs and
  surrounding code, then returns prioritized findings as JSON.

Interactive implementation planning is handled by Plan mode (`/plan` or
`/mode plan`), not by a planner sub-agent. Plan mode keeps the main
conversation read-only, extracts `<proposed_plan>` blocks, and routes
accepted plans into implementation.

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

Foreground and background agents share a no-progress watchdog controlled
by `mevedel-agent-no-progress-timeout` (default 600 seconds, nil
disables). It compares transcript buffer size, tool-call count, and
recorded activity from the last observed progress point. If no progress
is observed for the full grace period, the agent is stopped through the
same path as `mevedel-stop-agent`; foreground stops complete the parent
Agent tool, and background stops deliver a stopped `<agent-result>` so
BWAIT can resume. Ordinary runtime errors use the same recovery contract:
when possible the parent receives the safe transcript path, otherwise a
bounded recovered partial response from the live agent buffer.

## Stopping background agents

`StopAgent(agent_id, reason?)` stops a running background agent owned by
the current session or sub-agent invocation. It accepts the full agent id
or an unambiguous displayed short id, marks the transcript `aborted`,
delivers an `<agent-result>` to the parent mailbox with a Read-able
transcript path when persistence is available, removes the id from
`background-agents`, and resumes a parent parked in BWAIT. Without a
saved transcript, the stopped result falls back to a bounded recovered
partial response from the live agent buffer. Runtime error results follow
the same transcript-first, partial-second recovery rule. Stopping is
recursive through a stopped agent's live child registry.

The BWAIT watchdog uses the same recovery contract for stranded
background agents whose live FSM disappeared before normal completion.
It removes the stranded id from `background-agents`, marks the transcript
`incomplete` when sidecar metadata is available, and injects a synthetic
`<agent-result>` pointing at the saved transcript or a recovered partial
when possible. Live background agents are not killed on the first BWAIT
watchdog reminder if the child was not visible when BWAIT was entered;
otherwise the shared no-progress grace period starts as soon as the
parent parks in BWAIT. Later activity resets the grace timer.

`M-x mevedel-stop-agent` uses the same stop path as an interactive
escape hatch for cases where the parent FSM is already parked in BWAIT
and cannot call another tool. The BWAIT watchdog warning includes both
the tool and command names when it is still waiting on live agents.

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

## Review and verify commands

`mevedel-review` / `/review` and `mevedel-verify` / `/verify` run
dedicated foreground validation tasks. They share a target picker for
uncommitted changes, diff against a base branch merge-base, a specific
commit, the last commit, or custom instructions. Unlike ordinary slash
skills, this path is first-class: it ignores user/project skills named
`review`, constructs the agent task explicitly, and shares target CAPF for
explicit slash forms such as `current`, `HEAD`, `branch:<name>`, and
`commit:<rev>`.

`/review` dispatches the `reviewer` agent and parses its Codex-style JSON
finding shape: `findings`, `overall_correctness`, `overall_explanation`,
and `overall_confidence_score`. mevedel renders a readable summary as the
assistant reply and stores a synthetic review `<user_action>` in the
parent transcript so later turns can refer to numbered findings. The view
buffer strips that synthetic block from normal display.

`/verify` dispatches the `verifier` agent with verifier-oriented wording:
inspect adversarially, run or recommend relevant checks when allowed, and
finish with the verifier prompt's `VERDICT: PASS`, `VERDICT: FAIL`, or
`VERDICT: PARTIAL` line. Verifier output is inserted without review JSON
parsing.

While either task runs, the parent view shows an inline `Review` or
`Verify` handle backed by transcript metadata. The handle updates with
running/done/error state and recent tool-call counts like other agent
handles, without exposing the hidden bookkeeping block to the model.

## Transcript persistence and views

Each sub-agent invocation runs in its own gptel buffer. When session
persistence is enabled, that buffer is the transcript file under the
parent session's `agents/` directory. The parent session mirrors an
`agent-transcripts` alist into the sidecar with agent id, type,
description, path, status, timestamps, parent turn, and call count.

The main view renders compact one-line agent handles from tool
render-data and sidecar state. Handles show type, shortened task,
status, call count, and transcript attribution; recent ephemeral
activity is kept out of the default view to avoid churn. Terminal
handles open a rendered read-only transcript view from the saved
transcript file. Running handles open a rendered read-only view over
the live agent buffer when that buffer is available.

The status zone can show aggregate running or blocked agent rows so
the user can locate active handles without scanning the whole
transcript. Terminal agent outcomes stay in their inline tool handles
and transcript views instead of being repeated in the aggregate status
zone.

## Task overlay

Tasks tracked per caller (main chat and each sub-agent separately).
`blockedBy` propagates completion. `mevedel-tools--agents-fsm`
(buffer-local on chat buffer) maps agent-id → sub-agent FSM for
SendMessage resolution.

The default task overlay is compact and appears only while at least one
task is open. Group headers keep open/done counts visible, open tasks
are listed, and completed task details are hidden. `TAB` or `RET`
on the overlay toggles completed task details for inspection. The
overlay caps itself against the live window height; when rows are
omitted, it keeps open rows ahead of completed rows and shows short
summary lines such as `... 4 completed`. Completed tasks are not pruned
from the session task list.

Each owner group can also carry a short status note through `TaskNote`
or the top-level `note`/`noteOwner` arguments on `TaskCreate` and
`TaskUpdate`. Notes render under the owner header and are dropped from
view when that owner has no open tasks, so a completed-only task list
does not keep the overlay visible.

## Model tiers

`mevedel-models.el` maps small tier names (`fast`, `balanced`,
`strong`) to concrete gptel backend/model providers. Agent-like workload
defaults live in `mevedel-model-workload-tiers`; an Agent call or skill
invocation can override the model for that invocation without changing
the parent session default.
