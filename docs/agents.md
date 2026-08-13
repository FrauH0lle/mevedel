# Multi-agent system

The model-facing `Agent` tool starts a retained child asynchronously. It
accepts a lowercase `task_name` path segment, a complete `message`, and
optional `role`, `context`, `model`, and `effort` controls, then returns the
committed canonical path after preparation and provider dispatch succeed.
An omitted role selects `default` and inherits the delegator's effective
instructions, tools, model policy, and delegation capability. A named role
supplies its own configuration rather than intersecting its tools with the
delegator's. Children are created below the caller, so recursive delegation
forms paths such as `/root/implementation/tests`.

`context` defaults to `none`, so the complete initial `message` is the
child's sole assigned task and no parent dialogue is copied. Explicit `all`
copies the complete effective parent conversation, and a positive decimal
string such as `"3"` copies the anchored summary plus the three most recent
live turns. Explicit `summary` freezes the realized parent transcript without
the triggering Agent tool segment, runs one central handoff-summary request
focused on the hook-accepted task, and stores the result as a labelled advisory
`Task background` block before that authoritative task. The copy modes retain
gptel's user/response/tool span properties,
including actionable user instructions, and is taken from the current
post-compaction buffer only. Callers use copied context only when the child
must inspect parent dialogue and identify that dialogue as background in the
initial task. Archived raw segments are never reconstructed. The initial task
is appended after this immutable snapshot; parent turns added later are not
synchronized into the child.

At spawn, mevedel materializes the role's dynamic instructions and effective
tools, then captures the exact request backend, model, reasoning effort,
system prompt, tools, context settings, request parameters, and model policy
maps. Resolution starts with the delegator's current request defaults, applies
the role workload, and finally applies explicit `model` and `effort` values.
`model` accepts either a configured tier or `BACKEND:MODEL`; gptel validates
the selected model's effort support. Follow-ups reuse this frozen
configuration even if presets, role definitions, or parent settings change.
Root-session permission decisions and confinement remain live shared policy,
not part of the frozen request configuration.

The root session retains every child's storage identity, path, activity, and
transcript location after the turn settles. `ListAgents` returns the full
path-sorted retained roster without storage IDs or transcript content.
`FollowupAgent` continues an idle retained conversation or steers a running
invocation at its next safe request boundary; later terminal results still go
to the original spawn parent. A successful follow-up renders a collapsed
`FollowupAgent: PATH (follow-up sent)` disclosure containing the exact
follow-up text.
`SendMessage`
queues interim plain-text mail for `/root` or any retained path without
activating a turn. `WaitAgent` suspends its ordinary asynchronous tool callback
until mail, user steering, follow-up steering, or its bounded successful
timeout wakes it. A `MAIL` wake-up is not sender completion; only the canonical
`RESULT` is terminal.
`InterruptAgent` aborts one retained non-root agent's current turn by canonical
path, returns its previous activity, and leaves its identity, conversation,
mailbox, descendants, and future follow-up capability intact.

Creation validates and freezes its inputs, privately reserves the canonical
path and a tree-wide capacity slot, runs `SubagentStart` exactly once, then
runs `UserPromptSubmit` for the initial task. The hook-accepted task is passed
to dispatch without rerunning either hook. For `summary`, generation starts
only after that final task exists; the parent row remains in `Preparing summary
context...` until generation and durable child setup finish. The reservation is absent from
`ListAgents` and path resolution until a durable transcript and provider FSM
exist. Failure or parent cancellation releases it synchronously and suppresses
late preparation callbacks. Every idle-agent follow-up
runs `UserPromptSubmit` again but not `SubagentStart`. A blocked follow-up is
not appended as a user turn; its additional hook context stays with that
identity and is consumed once by its next accepted task. Every completed,
errored, or interrupted turn runs one observational `SubagentStop` without
removing the retained identity.

Each invocation aggregates only its own direct operating-system children.
After those children are stopped or settled, noteworthy confinement facts are
patched into the exact `Agent` row that started a new conversation or the
exact idle `FollowupAgent` row that activated the retained turn. Descendant
agents, earlier turns, and steering messages sent to an already-running agent
are excluded. The rollup uses hidden transcript render-data and is not added
to agent sidecars or model-visible results.

The built-in role configurations are:

- **worker**: broad implementation, execution, navigation, skill, task, and
  collaboration tools, with explicit concurrent-edit guidance
- **explorer**: directly read-only investigation with authority to delegate to
  workers
- **verifier**: adversarial read-only verification; per-turn
  `verifier-read-only` reminder attached at invocation. Final reports must
  end with `VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL`; the
  parsed verdict is stored in transcript render-data for the handle badge.
  PASS requires an adversarial probe, FAIL requires a concrete actionable
  defect, and PARTIAL is reserved for environmental limitations.
- **reviewer**: retained leaf code-review agent used by `/review`; per-turn
  `reviewer-read-only` reminder attached at invocation. Reads diffs and
  surrounding code, then returns prioritized findings as JSON.

Every named role receives `SendMessage` and `ListAgents`. Possession of
`Agent` grants transitive delegation authority and automatically supplies the
complete `Agent`, `FollowupAgent`, `WaitAgent`, and `InterruptAgent` control
bundle. Worker and explorer therefore orchestrate recursively; reviewer and
verifier are communicating leaves without those control tools. The complete
root-session tree shares the session's active-turn capacity (three non-root
turns by default), regardless of path depth. Waiting and human-blocked turns
remain active and continue consuming their existing slot.

Before the first sample, the WAIT boundary injects only the caller's direct
children as compact path and role references. Later WAIT boundaries add a
child created in the same turn exactly once. Peers and deeper descendants are
not injected; `ListAgents` is the explicit full-tree discovery surface.

A Goal runs in the root session conversation rather than through a special
agent or phase machine. Child-agent turns are excluded from Goal accounting.
Each active root turn receives request-local Goal context, while the existing
agent tree, capacity, and permission rules remain unchanged. Queued user
messages steer the Goal before its next automatic continuation.

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
`SubagentStart :additional-context` is auditable in both transcript
surfaces: the parent Agent tool row records that hook context was supplied,
and the child transcript stores the full hook context on the initial
prompt.

Agent definitions declare an ordered `:system-components` list. Entries are
registered prompt component symbols or inline `(NAME :file PATH)` /
`(NAME :text STRING)` components. Every agent profile is workspace-aware, so
`workspace-config` and `environment` must be present explicitly. There are no
per-section inclusion flags and no automatic skills inference: selecting
`memory` or `skills` is part of the role definition.

The built-ins all receive scoped `AGENTS.md` / `AGENTS.local.md` and
environment context without inheriting the main coding-assistant role. Worker
also receives memory; Explorer, verifier, and reviewer do not. Worker and
Explorer select the active skills roster and expose `Skill` / `ListSkills`;
verifier and reviewer remain skill-free. Worker, Explorer, and verifier share
the reporting tone, while reviewer relies on its strict output contract.

## Asynchronous agent lifecycle

```mermaid
flowchart TD
    A[Validate Agent request] --> B[Privately reserve path and capacity]
    B --> C[Freeze parent evidence and configuration]
    C --> D[Run SubagentStart once]
    D --> E[Run UserPromptSubmit once]
    E --> S{Summary context?}
    S -- Yes --> T[Generate one task-focused handoff summary]
    S -- No --> F[Persist transcript and start provider FSM]
    T --> F
    F --> G[Publish retained identity and Agent result]
    G --> H[Settle and run SubagentStop exactly once]
    H --> I[Release capacity and persist idle record]
    I --> J[Queue RESULT for spawn parent]
    J --> K{Parent needs result now?}
    K -- Yes --> L[WaitAgent wakes]
    K -- No --> M[Parent continues independently]
```

Every agent turn uses this path. A caller that needs the result explicitly
invokes `WaitAgent`; a caller that does not may finish while descendants keep
running. `/review`, `/verify`, and fork-skill workflows may keep their owning
interaction open until a leaf result arrives, but that awaiting behavior does
not create another agent execution mode.

## Agent resource results

Each retained registry record keeps the complete latest settled payload and
its terminal outcome (`completed`, `errored`, or `interrupted`) separately from
the bounded inline `RESULT` mailbox preview. The complete payload is recorded
before the preview is published, including recovery settlements. A new or
follow-up turn clears the previous settled result before it becomes active;
active agents therefore expose no streaming or stale result and are reported
as not ready. A later idle turn replaces the retained result atomically.

`agent://root/PATH` reads that complete settled payload and
`history://root/PATH` reads the same retained identity's transcript through
the shared read-only resource-address resolver. Neither address changes the
conversation, mailbox, transcript, or settlement state. The canonical path,
not the registry's opaque storage identity, is the only addressable identity.
See [`address-to-resource.md`](address-to-resource.md#agent-and-history).

## Interrupting retained agent turns

`InterruptAgent(target)` resolves only canonical or relative retained paths. It
rejects `/root`, the caller itself, malformed paths, unknown paths, and opaque
storage ids. An idle target is a successful no-op. An active target's provider
request or requestless wait is aborted, its transcript is finalized as
`aborted`, its active-turn slot is released, and exactly one canonical RESULT
with outcome `interrupted` goes to the stable spawn parent. The payload includes
the interruption reason, bounded useful partial work when available, and the
saved transcript path when available. Request teardown cancels the active tool
pipeline, terminates the target's child executions, and prevents its queued
execution work from being admitted after the turn becomes terminal.

Interruption never recurses. Descendant turns continue, and the target's path,
conversation buffer, mailbox, and registry record remain retained. A later
`FollowupAgent` therefore continues the same conversation. Interrupt-versus-
settlement races use the ordinary exactly-once settlement gate: whichever
terminal event wins is the only RESULT. The tool result itself contains only
the target's activity observed before the request and renders `Interrupted
PATH` from the canonical event.

## Inter-agent messaging (SendMessage)

`SendMessage(target, message)` resolves canonical or relative retained paths
tree-wide. It queues one canonical `MAIL` record containing type, sender path,
recipient path, and payload; it never starts an idle turn. Successful sends
return an empty result and render a collapsed
`SendMessage: PATH (message queued)` disclosure whose path opens the retained transcript and whose body contains the
sent message. Canonical `MAIL` payloads are retained in full without a mailbox
body cap. Since this delivery is interim and may cross a root-turn boundary,
an agent should put its final verdict in its terminal response rather than
treating `SendMessage` as its completion channel.

Before a recipient's next model sample, its retained unread queue drains in
FIFO order. Each record is injected as a separate user-role communication
block and written to the retained conversation transcript before the unread
record is removed. Mail queued for an idle agent therefore waits for a later
follow-up, while mail for an active agent is delivered at its next ordinary
WAIT boundary. The tool result never duplicates the message body.

`WaitAgent(timeout_ms?)` is a wake primitive over the caller's mailbox, not a
message transport. Its ordinary asynchronous callback stays pending without a
model sample and without releasing the caller's active-turn slot. Existing or
new mail releases it immediately, as does follow-up steering. New root user
input becomes a separate user-role
steering message in the same resumed request, so no intermediate model sample
can run before the input is visible. The default timeout is 30,000 ms.
Values clamp to the 10,000-3,600,000 ms range; malformed values fall back
to the default, and timeout is a successful outcome. Its result
contains only the wake reason. The view renders `Waiting for agents` while the
tool is pending. Settled waits render `WaitAgent: agents (OUTCOME)`;
consecutive calls coalesce into the final row with a count while every
canonical call remains in the transcript.

Independently completed yielded Bash executions use the session or invocation
object captured for their fixed owner when Bash starts. A retained invocation
holds its terminal response while an owned execution is live. Completion is
captured across that boundary in either arrival order, appended to the final
answer, and settled directly without a model request. Bash completion does not
wake `WaitAgent`; execution-only contents never start a paid continuation.

## Review and verify commands

`mevedel-review` / `/review` and `mevedel-verify` / `/verify` run
dedicated asynchronous leaf-agent turns. They share a target picker for
uncommitted changes, diff against a base branch merge-base, a specific
commit, the last commit, or custom instructions. Unlike ordinary user
skills, this path is first-class: it ignores user/project skills named
`review`, creates a context-isolated retained agent at a unique path such as
`/root/review` or `/root/verify_2`, and shares target CAPF for explicit target
forms such as `current`, `HEAD`, `branch:<name>`, and `commit:<rev>`.

The owning workflow attaches a one-shot consumer before provider dispatch and
awaits that leaf's ordinary terminal `RESULT`. Settlement first queues the
canonical envelope in parent mail; after successful workflow delivery, the
consumer removes that exact envelope so a later model turn cannot receive a
duplicate. Handler failure leaves the queued result recoverable. Completion
therefore uses the same settlement and active-turn accounting as every other
asynchronous agent. Cancellation interrupts only the active turn: the
canonical agent path and conversation remain retained for inspection or
follow-up.

`/review` dispatches the `reviewer` agent and parses its Codex-style JSON
finding shape: `findings`, `overall_correctness`, `overall_explanation`,
and `overall_confidence_score`. mevedel renders a readable summary as the
assistant reply and stores a synthetic review `<user_action>` in the
parent transcript so later turns can refer to numbered findings. The view
buffer strips that synthetic block from normal display.

`/verify` dispatches the `verifier` agent with verifier-oriented wording:
inspect adversarially, run or recommend relevant checks when allowed, and
finish with the verifier prompt's `VERDICT: PASS`, `VERDICT: FAIL`, or
`VERDICT: PARTIAL` line. The workflow accepts only one exact final verdict;
malformed reports remain visible but are marked rejected.

While either task runs, the parent view shows an inline `Review` or
`Verify` handle backed by transcript metadata. The handle updates with
running/done/error state and recent tool-call counts like other agent
handles, without exposing the hidden bookkeeping block to the model.

## Transcript persistence and views

Each retained agent runs in its own gptel conversation buffer backed by a
canonical transcript under the root session's `agents/` directory. The
buffer's `default-directory` remains the session working directory (falling
back to the workspace root), including after transcript attachment and cold
hydration; transcript storage location never becomes tool cwd. The
sidecar persists an explicit registry record for its canonical and parent
paths, role and frozen configuration, activity, unread mailbox, pending
conversation-local hook context, conversation location, and internal storage
identity, plus the latest settled payload and terminal outcome when present.
The canonical path is the only model-facing address; storage identities never
enter collaboration tools or resource addresses. The mailbox remains a
bounded delivery preview rather than the source of truth for an agent result.
The frozen configuration is authoritative for the agent's system prompt, so
agent transcripts omit gptel's redundant expanded `GPTEL_SYSTEM` property
while retaining `GPTEL_BOUNDS` and the remaining conversation metadata.
Generated task background is ordinary persisted conversation context with its
own structural type. Follow-ups and agent compaction therefore absorb it
naturally without replaying or regenerating it.

`mevedel-agent-conversation.el` owns conversation creation and hydration,
frozen request-local installation, activity snapshots, response extraction,
and transcript saves. `mevedel-agent-exec.el` is the provider adapter: it owns
the gptel request FSM, prompt dispatch, and streaming callback contract.

Persisted agents may compact older history immediately before a continuation
request.  The canonical transcript path remains stable, the original task and
recent tail remain visible, and later compactions update the existing anchored
summary instead of stacking summaries.  Each rewrite first creates the next
numbered `compact-NNNN` sibling as a recovery artifact.  Those siblings are not
agent handles or sidecar entries; they belong only to the original session and
are not copied by Session Forks. Each retained conversation owns this lifecycle
independently; compacting one agent does not change its registry path or any
other conversation.

Session Forks copy eligible canonical transcript files and metadata only as
historical inspection artifacts. They do not copy registry identities, frozen
configuration, mailboxes, waiters, or active turns. Historical agent
transcripts remain openable from their handles but are absent from the
collaboration roster, and their former canonical task names are immediately
available to the child. Rewind creates no child: it clears the current
session's live agent ownership.

`mevedel-view-agent.el` owns transcript lookup and inspection views plus the
aggregate live-agent status and targeted handle refresh. The main view renders
compact one-line agent handles from tool render-data and sidecar state.
Handles show canonical path, role, status, call count, and transcript
attribution; recent ephemeral
activity is kept out of the default view to avoid churn. Terminal
handles open a rendered read-only transcript view from the saved
transcript file. Running handles open a rendered read-only view over
the live agent buffer when that buffer is available. Open live transcript
views are observation-only projections that follow the main renderer's stream
and tool cadence without redirecting parent interactions. See
[`docs/view.md`](view.md#buffer-roles) for their update, scrolling, header,
settlement, and failure-isolation contract.

The agent view owner supplies aggregate running or blocked rows to the status
zone so the user can locate active handles without scanning the whole
transcript. Terminal agent outcomes stay in their inline tool handles
and transcript views instead of being repeated in the aggregate status
zone.

## Permission and confinement propagation

Every nested agent shares the root session's permission mode, direct rules,
explicit denies, protected resources, exact grants, and confinement policy by
reference. Its Bash and Eval calls therefore follow the same authority state as
the root. Required decisions and direct interactions are attributed with the
requester's canonical path and rendered in the root view's shared queues; child
transcript views remain inspection-only. A turn blocked on either queue remains
active and consumes tree capacity. Interrupting that turn cancels only its own
queued entries.

The retained-agent tree shares the root session's `local://` namespace,
including `local/plans/` for durable plans, notes, findings, contracts, and
handoffs. Standalone/sticky Plan mode keeps all-local `ApplyPatch` available to
retained agents.
It rejects any ordinary, non-local, or bare endpoint before local
materialization, including mixed local/ordinary and ordinary-only calls, while
other edit tools and `Eval` remain unavailable.

Directive planning additionally stamps immutable read-only authority on the
root request and copies it into every delegated invocation and nested request.
Those agents retain Plan tool and Bash restrictions after the root workflow
advances to approval or implementation; mutable session phase is not an
authority boundary. Unlike standalone/sticky Plan mode, directive Planning
remains strictly read-only: its requests and retained agents cannot use
`ApplyPatch`, including all-local proposals, or `Eval`.

Delegated invocation/request rules may narrow authority and may allow ordinary
known-safe commands, but they cannot authorize dangerous or complex Bash, live
Eval, protected resources, or full execution escalation. An ordinary sub-agent
may request additive or full authority only through the same user-visible queue;
there is no separate model-visible access-request tool. The main view's
agent row retains durable warnings for materially non-default child access.
Additional read-only mounts stay silent. Each Bash or batch-Eval result records
the boundary used by that call, and the agent transcript identifies the
affected tool.

## Task status

Tasks are tracked per caller (`/root` and each retained agent path). Agent-owned
tasks and status notes use the retained agent's canonical path for automatic
assignment, grouping, rendering, and terminal finalization; opaque storage IDs
never enter the task surface. Explicit canonical owners must name a retained
agent in the session, while `/root` normalizes to the main owner. Explicit
non-path owner strings remain available as user-defined task buckets.
Resume validates persisted task and status-note owners against the restored
registry and drops entries carrying opaque IDs, malformed paths, or unknown
canonical paths before they can reach model-visible task state. Dependency
edges to dropped tasks are pruned in both directions, so resume cannot leave a
surviving task blocked by an absent task.
`blockedBy` propagates completion. Tasks therefore remain stable across
follow-ups and cold session resume.

The task status fragment is compact and appears only while at least one
task is open. Group headers keep open/done counts visible, open tasks
are listed, and completed task details are hidden. `TAB` or `RET`
on the fragment toggles completed task details for inspection. The
fragment caps itself against the live window height; when rows are
omitted, it keeps open rows ahead of completed rows and shows short
summary lines such as `... 4 completed`. Completed tasks are not pruned
from the session task list.

Each owner group can also carry a short status note through `TaskNote`
or the top-level `note`/`noteOwner` arguments on `TaskCreate` and
`TaskUpdate`. Notes render under the owner header and are dropped from
view when that owner has no open tasks, so a completed-only task list
does not keep the overlay visible.

## Model tiers

`mevedel-models.el` resolves the current session's preset-local named tiers and
workload map. A tier can select a concrete gptel provider and reasoning effort;
a workload can select a tier or exact provider and override effort. Resolution
starts from the session backend/model/effort, then applies tier and workload
values, followed by explicit Agent policy or the policy of a skill that owns
the child request. Explicit Agent `model` and `effort` values have final
precedence. Skill-specific preset entries use `$skill-name` symbols in the
same workload map. Agent buffers receive a deep-copied snapshot of the maps,
so nested agents keep the policy in effect when they were launched.
