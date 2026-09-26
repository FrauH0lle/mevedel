# Architecture

For source-file responsibilities, see the [module map](module-map.md).

## System flow

```mermaid
flowchart TD
    W[Workspace] -->|shared records| D[Directives and source anchors]
    W -->|one or more| S[Sessions]
    S -->|owns| T[Canonical transcript in data buffer]
    S -->|admits| R[Request and gptel state machine]
    R -->|validated and authorized calls| X[Tool pipeline and agent runtime]
    R -->|responses and results| T
    T -->|projects into| V[Emacs view and browser view]
    S -->|publishes| P[Durable session state]
```

A workspace shares directive records across sessions. Each session owns its
conversation, execution target, authority and retained agents. The data buffer
holds the canonical transcript; views project it and submit user actions through
the owning session. A request has its own admission and settlement boundary.
Tool and agent failures return through that boundary; a rendering failure does
not settle the request. See [tool execution](tools.md), [agent lifecycle](agents.md)
and [session publication and recovery](sessions.md) for their failure paths.

Memory has separate workspace and user scopes. Selected memory indexes become
request context; memory is not a transcript or a session-owned storage slot.
See [persistent memory](#persistent-memory).

Browser collaboration projects host state through an encrypted, content-blind
relay connection. The Emacs host owns session behavior and validates guest
actions. The [collaboration manual](collaboration.md) owns deployment, link
authority, attachments, reconnects, notifications and lifecycle details;
[ADR 0099](adr/0099-project-live-collaboration-from-host-authoritative-state.md)
explains the boundary.

## Key data structures

Defined in `mevedel-structs.el` / `mevedel-tool-registry.el`:

`mevedel-structs.el` owns these passive shapes and their shape-local
invariants.  `mevedel-workspace.el` owns workspace registry and state lookup,
`mevedel-directive.el` owns directive mutation and derived lifecycle, and
`mevedel-turn.el` owns request admission, cancellation, and settlement.

- **`mevedel-workspace`**: type, id, root, name, file-cache, and the durable
  directive records shared by every session in the workspace.
  Its journal and memory observations are disposable UI caches; memory counts
  never authorize a proposal decision or application.
  Additional roots live in `mevedel-workspace-additional-roots`.
  `.mevedel/` is derived by
  `mevedel-workspace-state-dir`, not stored as a slot.
- **`mevedel-directive`**: stable directive id, current authored request,
  source-anchor description, lifecycle state, bound execution-session id, and
  Plan-before-implementation preference and proposal, chronological planning,
  implementation, and discussion turns, and current parent-owned
  subdirectives. Source overlays retain the id needed to resolve this record;
  they do not own another request, status, patch, or attempt copy.
  Current top-level and nested ids are globally unique within the workspace;
  durable decode rejects any collision before installing records. Historical
  consumed-subdirective snapshots are evidence rather than current identities.
  The lifecycle state is derived from the current authored request and surviving
  activity whenever the record is loaded or queried; it is not persisted as an
  independent authority.
- **`mevedel-subdirective`**: stable id, current authored request, and attached
  source anchor for one nested detail owned by a top-level directive. It never
  owns a session, lifecycle state, attempt list, discussion, or Rewind action.
- **`mevedel-directive-attempt`**: immutable action framing, submitted request,
  terminal answer or error, authored-request snapshot, outcome, captured patch,
  capture time and completeness, covered files, explicit gaps, the successful
  attempt's consumed subdirective snapshots, conservatively detected tool
  effects outside snapshot coverage, and the session/turn checkpoint
  that can restore the file state around the attempt, plus any accepted plan
  and request-local implementation selection. Its directive-local
  settlement sequence orders it against discussion turns. A complete empty
  patch means the request observed its covered files and made no changes;
  incomplete capture never implies complete coverage.
- **`mevedel-directive-discussion-turn`**: immutable local question, submitted
  request, authored-request snapshot, terminal answer or error, outcome,
  optional selected-attempt index, session/turn checkpoint, and directive-local
  settlement sequence. A settlement sequence identifies one event across
  all three collections -- planning turns, attempts, and discussion turns
  -- and the codec rejects a record that repeats one.
- **`mevedel-session`**: per-chat state: stable name-independent ID, editable
  display name and one-shot title eligibility, workspace, immutable execution target,
  qualified working directory, tasks, touched-files, permission rules/mode,
  exact or recursive resource grants, reminders, persisted per-conversation
  workspace-instruction content hashes,
  deferred tool state, mailbox messages, the retained agent registry,
  transient unpublished agent reservations, root activity and tree capacity, mention
  dedup, queued follow-up user messages, skills, session persistence metadata, agent transcript index,
  invoked skills, session-scoped hook rules/log/context, permission
  queue, one pending plan approval, selected preset and resolved mevedel preset settings,
  the current session-owned Goal, and a transient bounded tool-input repair
  log. Lifecycle events emitted before session materialization wait in the
  transient `telemetry-pending` queue and flush to the diagnostic stream; the
  queue is never persisted as resumable state. A transient side conversation
  may point `audit-session` at its durable parent for redacted audit events and
  shared target mutation authority; its runtime queues and unsanitized logs
  remain side-owned. The transient
  `execution-state` slot is opaque outside `mevedel-execution.el`;
  `mevedel-execution-process.el` keeps child processes, timers, spools, and
  process groups behind an opaque child value, so they never enter the general
  session model or persisted sidecar. `mevedel-execution-telemetry.el` owns an
  opaque per-execution context containing live ownership provenance, mutable
  summary cells, and privacy-safe event facts, but no live process record.
  Portable lease
  generations persist only the boolean unsettled-mutation safety latch needed
  when those transient records disappear.
- **`mevedel-goal`**: identity, objective, lifecycle status and reason,
  token/time/turn accounting, optional budget and accepted-plan reference,
  and timestamps.
- **`mevedel-request`**: per-turn state: process-unique request identity,
  owning session and agent origin, its once-reserved session turn identity,
  request start time, accumulated active-work pause time, file-snapshots,
  directive UUID, immutable Plan read-only authority, cancellers,
  skill-scoped permission rules, user-attached skill records, hook rules, and
  transient one-shot-mutation/ephemeral-artifact boundaries.
  Skill model and effort policy is consumed before
  gptel realizes an owning request rather than stored for late mutation.
- **`mevedel-tool`**: name, handler, description, summary, prompt and prompt
  provenance,
  args, category, read-only/destructive/async/snapshot flags, sync/async
  permission hooks, specifier extractors (`get-path`, `get-pattern`,
  `get-domain`, `get-name`), groups, max-result-size, display argument,
  render transform, renderer, and its provider-facing gptel tool.
- `mevedel--instruction-states`: workspace-keyed instruction alists and ID state
- Instruction types: **References** (source-bound context) and **Directives**
  (workspace-owned prompts with source presentations)

Instruction ownership is split at the durable-record/presentation boundary:

- `mevedel-instruction-registry.el` owns workspace buckets, IDs, traversal,
  lookup, and links.
- `mevedel-directive-source.el` owns anchors and every operation that changes
  both a durable directive record and its source presentation, including
  create, detach, reattach, archive, and delete.
- `mevedel-overlay-ui.el` owns action dispatch, labels, keymaps, styling, and
  redraw.
- `mevedel-overlays.el` remains the geometry, containment, tag, navigation,
  context, and prompt core.

Callers do not update a directive record and then repair its overlay
separately. They enter through the source owner, while registry and UI work
remain presentation-neutral or durable-record-neutral respectively.

Directive anchors are either Attached, with a live source range, or Detached,
with a zero-width source position, former source order, and the last attached
anchor evidence. Archived and Source-missing anchors keep the last attached
region. Every stored anchor names a file and an ordered region, in every one
of those states, and the codec rejects one that does not. Creating a directive
therefore requires a file-visiting buffer: without a file nothing could
reattach it, and detaching or archiving it would freeze a fileless anchor into
a record that no longer loads. Deleting an entire directive range preserves its durable
record and replaces the evaporated range overlay with a compact detached row;
partial edits use normal overlay resizing. Co-located detached rows are ordered
by their former source positions. References keep their source-bound
evaporation behavior.

Top-level directive presentations may persist an exact provider and
reasoning-effort override. Nested directives are durable details owned by the
topmost directive. Acting on any nested presentation resolves that owner, and
prompt construction includes every current nested detail in stable source
order. When the top-level range detaches, its nested source presentations may
disappear, but prompt construction and submission snapshots continue from the
parent-owned records. Without an override, the directive inherits the main
session model at dispatch.

Directive requests submit an explicit string prompt built from the current
authored request and freshly resolved references, so request construction never
reads surrounding ordinary-chat history. Direct implementation-type prompts
additionally append the record's persisted skill selection as instruction
mentions, revalidated against the session at every dispatch; accepted-plan
handoffs instead carry the approval card's selection. Each request streams as a first-class
turn in the bound execution session's canonical transcript with ordinary user,
response, tool, and interaction roles. Paired durable boundary records identify
the directive turn without replacing any of those roles. Before gptel parses an
ordinary chat request, a synchronous prompt-copy transform marks every enclosed
directive body `gptel 'ignore`; explicit directive requests continue to use
only the workspace record and directive-local history.

Starting a Ready discussion submits that request directly as the first
directive turn. Follow-ups enter a sticky directive scope in the shared
composer and add only durable discussion turns whose authored-request snapshot
matches the current directive request. The first accepted request binds the
directive to its execution session. Each accepted request reserves one session
turn identity before tools run; snapshots, transcript metadata, prompt/Rewind
indexing, and directive checkpoint links use that same identity, which terminal
settlement commits without recomputing it.

When Plan before implementation is enabled, every implementation-starting
action first records a read-only planning turn in that same bound session. The
directive record owns the proposal and approval selection; the session keeps
only a transient reservation identifying whether the workflow is planning,
awaiting approval, or implementing. The accepted handoff reuses the ordinary
directive request path with request-local mode and model policy, and stores the
accepted plan on the resulting immutable attempt. Standalone Plan and directive
planning are mutually exclusive session owners.

Terminal settlement keeps the complete turn in the transcript and writes the
immutable attempt or discussion turn to the workspace record even if the source
overlay detached while the request was in flight; this bounded duplication
keeps chronological presentation separate from durable follow-up context.
The FSM terminal transition captures the final patch before publishing the
request outcome, including on abort; the provider's earlier raw abort callback
does not settle the directive.
Overlay updates remain optional presentation work. A successful implementation
records immutable snapshots of, then consumes, exactly the subdirectives present
at dispatch. Failure and abort consume none; details authored while a request is
in flight remain current.

Batch processing queues durable top-level directive records in stable source
order rather than retaining source overlays. Each item resolves the same live
prompt context used by an individual action only when its turn begins, so a
prior implementation may detach or remove later source without corrupting the
queue. Ready records use Implement and Discussed records without an attempt use
Implement this. Either action pauses the batch at the directive approval card
when Plan before implementation is enabled. A Source missing record has
sufficient context only when it is top-level, bodyless, and has no nested
details; region-backed records must be
reattached. Records with any implementation attempt or without sufficient
current prompt context are reported and skipped; the first failed or aborted
request stops the batch. A zero-delay continuation starts the next item only
after terminal request cleanup.

## Workspace context chain

Workspace detection first uses the session or cached workspace, then a
project.el root. Outside recognized projects it uses the nearest ancestor with
`.mevedel/workspace-id`, falling back to `default-directory`. File buffers and
Dired therefore share one directory workspace and the same portable session
authority. Plain directories need no project marker. File-based workspaces
remain available through explicit `mevedel-workspace-functions` customization;
they are not part of default detection.

Retained current-context delivery loads `AGENTS.md` then `AGENTS.local.md` from the
workspace root through the session working directory. A successful `Read` of a
deeper file queues any newly applicable instruction files as a host-generated
same-turn reminder. Content hashes deduplicate unchanged files independently
for `/root` and each retained agent only while that owner's model-visible
context remains current. Each `SessionStart` context epoch resets `/root`;
resume resets every owner, and retained-agent compaction resets that agent.

`M-x mevedel-inspect-effective-prompt` and `/prompt` open the same read-only
report of the live preset, profile, prompt components, exact final prompt,
effective tools, prompt provenance, and provider-schema size estimate.

```
Data buffer (authoritative gptel/org buffer; holds mevedel--workspace,
mevedel--session, and the canonical transcript projected into provider context)
  |
View buffer (mevedel-view-mode; holds mevedel--data-buffer and the
input zone / editable composer)
  |
Derived buffers / previews / transcript inspection views point back to
their data or parent view buffers as needed
```

Tools execute in the data-buffer context with `default-directory` set to
the session working directory. File modifications are tracked per request
via `mevedel-request-file-snapshots`, while cross-turn file metadata
lives on the workspace file cache and session touched-files map. A cached
entry counts as current only while its modification time, its inode change
time, and its reported size are all unchanged: a modification time alone is
not proof, since restoring timestamps, a backwards clock, and
coarse-grained filesystem stamps all leave it standing, while a change time
cannot be set from userland and so moves anyway. A rewrite that restores
all three is the remaining blind spot, and the cache does not detect such a rewrite without a fresh content read. A file over
`mevedel-file-cache-max-file-bytes` is cached as a fingerprint with no
content, and mevedel's own session directories are refused outright
(`mevedel-session-file-cache-excluded-p`) apart from their `artifacts`
subtree; see [`docs/reminders.md`](reminders.md) for what the resulting
`edited-file` reminder reports.

`mevedel-execution-target.el` binds each session to one local or TRAMP target,
owns qualified/native path conversion, and probes target readiness.  Required
`rg` compatibility is verified against a bounded target-side fixture using the
Glob and Grep flag surface, rather than inferred from a version string.  The
project-owned identity in `mevedel-workspace-identity.el` lets equivalent
client-specific TRAMP spellings reopen the same workspace.  SSH-family
`ssh`/`scp`/`sshx`/`scpx`, Docker, and Podman targets are supported; a target
without `HOME` is blocked during readiness rather than failing later path
permission checks.  A target missing `rg`, `bash`, or `setsid` is named in the
readiness diagnostic together with the install command for whichever package
manager the probe finds there, so the fix does not require guessing the
target's distribution.  Mevedel never pushes binaries to a target.

A remote project mounted into the local filesystem -- SSHFS, `rclone mount`,
NFS, SMB -- is deliberately ordinary local operation, not a remote target.
Mevedel sees a local path, so files, search, Bash, and sessions use the
ordinary local execution path and its local dependencies.  The trade is
that the mount's host runtime is unused: commands run against the local
toolchain, compilers, and container environment rather than the storage host's.
Choose a TRAMP target when the project's runtime matters, and a mount when only
its files do.

`mevedel-execution.el` owns managed admission, the per-session registry,
observations, delivery, and the public execution facade. Its opaque child
values belong to `mevedel-execution-process.el`, which owns process creation,
stable child environments, process-group signaling, timeout cleanup, and
bounded disk spooling. Bubblewrap admission and refusal remain with the
managed facade. `mevedel-execution-scheduler.el` admits managed Bash through a
fair session-scoped readers/writer lane. `mevedel-bash-policy.el` owns Bash
classification and reusable rules;
`mevedel-tool-exec-permission.el` owns Bash/Eval authority and prompt adapters;
and `mevedel-tool-exec.el` retains their tool lifecycle, rendering, and
registration. Native filesystem tools use the execution facade's confined
one-shot helper without entering the Bash scheduler. The Bash adapter also
captures its analyzed exit-outcome resolver at spawn, so
later observations derive the same canonical facts without moving command
semantics into the process owner.

`mevedel-execution-telemetry.el` owns privacy-safe execution event projection,
sandbox attempt summaries, Eask workload recognition, and optional GNU time
resource capture. It never receives a live execution record.

`mevedel-transport.el` answers one question: is this Emacs already inside a
remote operation?  Durable target I/O started from a timer, a process
filter, or redisplay must not nest inside an in-flight TRAMP command, so
callers route deferrable work through `mevedel-transport-run-when-idle`
and it runs when the connection is quiet. Deferred work that owns an
in-memory admission fence supplies cancellation cleanup so transport teardown
cannot leave the session permanently busy. Lifecycle events that may fire
repeatedly (turn completion, activation, buffer kill) arm one coalesced
opportunity per key through `mevedel-transport-schedule-idle`, which the
journal processor, the memory pass, and memory recovery share.

Each pending transport entry owns its retry timer and cancellation callback
together. Deferred retries execute and remove entries only while their exact
timer still owns the coalescing key. An inline same-key call retires the pending
timer before running current work. This identity check also rejects cancelled
timers delivered after TRAMP restores a suspended timer list. Bulk cancellation
retires its batch before invoking cleanup callbacks, so reentrantly scheduled
replacement work keeps its own pending entry and cancellation callback.

TRAMP let-binds the timer lists to nil around its critical sections, and a
process filter or sentinel can run inside one. A plain timer armed there either
fires inside TRAMP's own wait, nested in the remote command, or is discarded
with the binding. Continuations armed from that context use
`mevedel-transport-run-at-time`: pipeline yields, the ToolCall trampoline,
WaitAgent timeouts, stream-bridge flushes, and child process settlement and
retirement. Inside a TRAMP handler frame it holds the timer and arms it when
the outermost frame returns. The continuation neither disappears nor runs
nested inside the remote command. A held timer cancelled before arming still
fires once; its callers check that their work is current. Agent conversation
saves refused as busy are requeued through `mevedel-transport-run-when-idle`
rather than dropped.

`mevedel-transport-with-exclusive-connection` suspends existing timers the
same way. `mevedel--timer-pending-p` counts a suspended timer as armed. A
pending retry is therefore not activated a second time on the section's
temporary list. Otherwise that copy could fire there and leave the suspended
original marked triggered, which Emacs skips forever.

Replacing a remote file with `mevedel--write-file-atomically` (ApplyPatch,
skill and plugin files, persisted state) is one pinned control program: the
temporary file, mode and rename happen beside the destination on the target,
where TRAMP's file operations needed about twenty round trips. A symlinked
leaf, an unproven parent spelling, a missing directory or a busy transport falls
back to the TRAMP operations, which keep their existing semantics.

`mevedel-session-durability.el` owns portable project lease and storage
primitives.  `mevedel-session-recovery.el` owns specialized recovery markers,
`mevedel-session-transfer.el` owns cooperative control-transfer records, and
`mevedel-session-publication.el` serializes immutable authoritative
publication and diagnostics.  All of them bottom out in
`mevedel-session-control-fs.el`, which performs control filesystem
operations through a target-side directory descriptor pinning the parent
while the relative operation runs; `mevedel-session-control-transfer.el`
coordinates cooperative control transfer above the transfer records --
persistence calls it for polling, admission, and committed-state adoption.  `mevedel-session-save-as.el` owns the portable
Save As transaction and live-session adoption.  Project sessions use the same
authority profile on local and TRAMP targets: a renewable `.lease/` and an
immutable publication head.  File-workspace sessions use the explicit
`pid-lock` profile and `.lock` instead.  The persisted session profile, not
`file-remote-p`, selects every acquire, release, sweep, and cleanup path;
mixed control artifacts fail closed.  The persistence facade and its codec,
artifact, Rewind, and Fork owners form the workflow layer above that boundary.

Before a mutating managed Bash child can start on a portable project target, the execution module
asserts the durable parent's current lease and commits its unsettled-mutation
latch. Proven terminal settlement clears the latch only after all armed records
sharing that authority have settled. Process records remain transient; restore
and takeover recover the latch, not an invented process registry.

Each mutable process record points to one immutable origin record containing
the session, owner, private mailbox context, data buffer, tool arguments, and
tool-use ID. Delivery state is explicit and separate from process completion:
a finished result rejected by its mailbox remains unsettled and owner-reachable
until either the model or a mailbox consumer claims it.

The execution module also publishes isolated yield, progress, and terminal
event snapshots. The pipeline supplies the durable gptel tool-use ID and
originating data buffer when Bash starts. Live progress remains a bounded,
disposable view projection; terminal output and structured facts replace the
original row's hidden render-data side channel in the authoritative transcript.
If a parallel tool result has not inserted that row yet, the data buffer keeps
a bounded pending terminal projection and retries at later tool and render
boundaries. Agent data buffers also retry unconditionally at their final
response boundary, so this does not depend on an open transcript view.
Completion therefore survives row-order races, cache turnover, and session
persistence without entering the model-visible result. Passive event
subscribers receive independent copies,
never the private owner context, and cannot acknowledge delivery. Terminal
delivery is claimed exactly once by either a model observer or the single
mailbox sink, using the session or agent invocation captured at spawn. The
agent runtime parks an invocation while its owner has an unsettled execution.
The agent's terminal callback remains gated while any owned execution is
unsettled. Whether the last completion arrives before or after the agent's
terminal response, the runtime settles the turn directly. The agent's final
answer remains its `RESULT`; captured completions are separate `EXECUTION`
records committed to the spawn parent's mailbox in the same settlement.
Agent execution completion is invocation-local until settlement, does not
wake `WaitAgent` early, and launches no model request. Ordinary mailbox
messages are delivered before the next model sample or wake an explicit
`WaitAgent`.

`mevedel-executions-list.el` is the user-facing projection of that private
registry. The execution module returns immutable all-owner snapshots and
accepts session-user control by execution ID; process records and operating-
system identifiers remain private. Model tools continue through the narrower
yielded-and-owner-scoped interface. Registry membership changes update the
view's live execution count and cockpit rows, while progress and yield events
refresh live row details without creating transcript state.

## gptel integration

Request admission captures the owning request identity. Late fork-skill and
review preparation/results are ignored after that identity is cancelled,
replaced, or its buffer dies. Synthetic results keep their original identity
through post-response hooks. Failed send startup and directive rollback end only
their own request; nested replacement requests remain active. Interrupted agent
startup uses the same rollback as provider errors.

Direct via `gptel-request` and `gptel-fsm`. Tools registered in
`gptel--known-tools`. Presets use exact declared names and inherit in parent
order (later parents win, then the child). Ordinary preset keys resolve to
`mevedel-foo`/`mevedel--foo` before gptel variables and use gptel's value
composition semantics. Persistent application is buffer- and session-local;
request-only application is dynamically scoped. The built-ins are
`mevedel-discuss` and `mevedel-implement`. Request changes
and Retry use ordinary implementation authority and focused prompt context,
not another preset. Presets can also merge named model tiers and workload maps.
Dispatch resolves session values, tier values, workload values, then explicit
Agent policy or request-owning skill policy. Skill preset entries use
`$skill-name` workload symbols and are consumed before request realization.
Directive overrides are validated before processing starts and appended as the
final prompt transform. They therefore win for that directive request and its
continuations without mutating the session model.
Ordinary-chat prompt assembly also runs the directive-boundary transform in
gptel's temporary request copy. It applies `gptel 'ignore` to complete directive
turns there, including tool spans, while leaving the canonical response and
`(tool . id)` properties intact for persistence and rendering.
System prompts are assembled dynamically from ordered profiles in
`mevedel-system.el`. `mevedel-define-prompt-component` registers reusable
Markdown, literal text, or dynamic producers.
`mevedel-define-prompt-profile` selects components, and the profile list is the
render order; inline `(NAME :file PATH)` and `(NAME :text STRING)` entries keep
one-off role content local. Blank components are omitted. Workspace-aware
profiles must explicitly contain `workspace-config` and `environment`, which
the renderer validates before dispatch.

For retained conversations, named observations are delivered separately through
`mevedel-context-delivery.el`. The table describes component selection across
both channels: roles/policies remain stable system instructions, while workspace
configuration, environment, memory indexes, skills, resource availability and
Goal state become retained updates. Stateless profiles keep current snapshots.
See [retained instruction context](#retained-instruction-context).

The built-in selection is deliberate:

| Consumer | Role/tone/context |
| --- | --- |
| Main | Base role, task policy, main tone, memory use/save policy, tool orchestration, workspace config, memory data, environment, skills, Goal |
| Worker | Worker role, task policy, report tone, memory use/save policy, tool orchestration, workspace config, memory data, environment, skills |
| Explorer | Explorer role, task policy, report tone, tool orchestration, workspace config, environment, skills |
| Verifier | Verifier role, task policy, report tone, tool orchestration, workspace config, environment |
| Reviewer | Reviewer role, task policy, tool orchestration, workspace config, environment |
| Approval reviewer | Isolated approval policy; intent and authority arrive as quoted evidence |
| Context summary | Fixed continuation/handoff summary contract only |
| Buddy / Buddy guide | Respective role, memory use policy, workspace config, memory data, environment |

The shared task policy owns scoped autonomy, permissions, existing edits,
instruction provenance/conflicts, and truthful verification. Role text supplies
the task purpose, direct-tool limits, and consumed report contract. Guardians
and context summaries exclude coding task policy. Tone owns communication;
tool descriptions own suitability and calling contracts.

The shared tool-orchestration component describes useful delegation and batching
without search/file thresholds. Dependencies, waits, approvals, and conflicting
mutations remain sequential. ToolCall suitability lives in its description;
activating it does not rewrite the earlier system prompt with a promotion.
Resource availability remains context-specific. Provider cache reuse still depends on provider policy and request configuration.

`mevedel-gptel-stream-bridge.el` isolates private gptel stream advice. It
repairs detached insertion markers, falls back to raw chunks when a stale
transformer fails, and delays early output until the process has a registered
request state machine. Consecutive plain-text inserts are batched for
`mevedel-gptel-stream-bridge-insert-batch-delay` seconds (0.04 by default);
non-text boundaries and cleanup flush the batch. Setting the delay to nil or
zero disables batching. These mechanisms preserve the data buffer as the
transcript authority; view redraw scheduling remains separate.
`mevedel-gptel-bridge.el` routes native steering commands through the root
composer submission path and refuses native agent/confirmation steering;
there is no second request-local steering queue in managed sessions.
`mevedel-view-stream.el` owns live-tail render scheduling,
pending-tool live rows, and foreground request-progress state, while
`mevedel-execution-transcript.el` owns durable execution render data and
compaction archive reconciliation. View Stream delegates transcript projection
to `mevedel-view-render.el`; `mevedel-view-disclosure.el` owns source-backed
fold state and actions, `mevedel-view-composer.el` owns the editable input,
submission hooks, and send/fork dispatch, `mevedel-view-segments.el` owns
ephemeral archived-segment projection, and `mevedel-pending-inputs.el` owns
queued follow-ups. Durable segment bytes remain in
`mevedel-session-artifacts.el`. `mevedel-view.el` coordinates the view mode,
zones, and session lifecycle. The authoritative text remains in the gptel data
buffer.

`mevedel-mention-bindings.el` owns atomic mention identity as validated text
properties on ordinary prompt strings. Completion or programmatic insertion
binds when an exact target first becomes known; the composer binds remaining
resolvable mentions before asynchronous preparation, queueing, or history
insertion. Draft, queue, retry, transcript, and history paths transport the
same propertized string, while kind-specific skill and mention modules resolve
the stored locator against current state and permissions at dispatch. Valid
unavailability annotates only the temporary request and continues the turn;
malformed live data blocks submission. Input-history persistence rejects and
quarantines incompatible binding data rather than migrating it. The supported
kinds are a closed explicit dispatch over skill source path, reference UUID,
absolute file pathname, and MCP server/URI; there is no resolver registry or
sidecar identity store. See [`mentions.md`](mentions.md#atomic-binding-lifecycle).

`mevedel-turn.el` owns top-level request admission, identity, cancellation, and
the single completion boundary. The ordinary gptel `DONE` state and awaited
fork-skill workflows call it after response hooks. Error and abort terminals
also save partial responses and file checkpoints before teardown, but do not
dispatch successful-turn follow-ups. Final-patch generation and deferred
settlement retain a nested admission fence until their continuations finish;
abort stops producers without discarding a reservation still needed for durable
settlement. If teardown lost the request before a terminal transition arrived,
the machine's request identity keys a degraded settlement. Transport cancellation
releases the settlement fence and clears the machine's settlement stamp so a
later terminal transition can retry it.

`mevedel-turn-end-at-boundary` lets a harness caller end a running root or
agent turn without aborting it. Once the current tool results are recorded,
the machine moves to `DONE` instead of sampling again, so the turn settles as a
completed one: accounting, journal capture, the checkpoint, queued input, and
Goal continuation run as usual. Errors and pending same-turn steering take
precedence, so steering is delivered and the turn ends at the following
boundary. The request is a transition rule added by
`mevedel-preset--build-transitions`, which root and agent machines both use; an
agent turn ended this way settles from its `DONE` handler. gptel's own
post-tool `:stop` differs: it records an error and fails the turn. Callers:
an UpdateGoal completion rejected by its verifier ([Goals](goals.md#goal-tools)).

Terminal continuations settle once and recheck request and session ownership
between lifecycle steps. Old patch and directive-attempt evidence remains tied
to the captured request; an obsolete continuation cannot replace current patch
presentation, clear a newer directive, restore its permissions, or end its
request. Response-end markers bound delayed directive capture to the old answer.

Main and agent data buffers install buffer-local gptel pre/post-tool hooks.
The pre-tool hook preserves raw JSON distinctions, validates the call as-is,
and attempts deterministic repair only after failure. A buffer-local ledger
then associates the raw call with pipeline dispatch and final result without
placing argument values in telemetry. The normal pipeline remains the final
validation, permission, execution, and persistence boundary. See
[`tools.md`](tools.md#tool-input-validation-and-repair) and
[`ADR 0011`](adr/0011-repair-model-tool-input-before-pipeline.md).

`mevedel-tool-repair.el` owns structured contract validation plus generic
atomic repair. `mevedel-tool-repair-gptel.el` isolates the temporary
lossless gptel decoding bridge, while `mevedel-tool-repair-diagnostics.el`
owns value-free audit records, dispatch-result tracking, and redacted
telemetry. `mevedel-tool-registry.el` owns the schema declarations and lowers
the internal `path` type to a provider-facing string.

The `workspace-config` component checks each directory from workspace root
to the session working directory for `AGENTS.md`. `AGENTS.local.md`,
when present, is loaded after the shared file in that same directory.
Matching files are included from broadest to closest scope as
`## Workspace Configuration` so deeper instructions override earlier
ones.

## Retained instruction context

A retained conversation separates behavioral instructions from observations that
change during work. Main and retained-agent system prompts keep role, authority,
style, skill dispatch, memory-use policy and a short memory-manual retrieval
requirement. Named workspace configuration, environment, memory indexes, skill
catalogs, resource availability, the main journal map and root Goal context are
delivered after current input through the existing reminder transaction.

Environment context identifies the execution target as `local` or a TRAMP
method and destination, such as `ssh:alice@build` or `podman:dev`, before the
target-native working directory. Multi-hop connections name the final
destination. Local mounts such as SSHFS remain `local`; the label describes
execution relative to Emacs. Labels use existing target metadata or parse the
working directory without probing. Container, VM, and WSL environments beyond
what the TRAMP method identifies are not detected.

Environment, active Goal, skills, memory, journal and resource availability are
retained as independent current-state sections. If one selected section changes, the next
request appends that section's complete current contents. Its notice explicitly
replaces only that section and says other previously supplied state remains
applicable. Unchanged sections are not repeated; unchanged turns add no update.
Workspace guidance and Goal procedures also update independently, so a counter
change repeats neither their instructions nor the other fact sections.

`mevedel-context-delivery.el` owns this delivery boundary. Only components selected
by an agent's frozen definition reach that agent. Workers do not inherit the
root Goal or the main journal map. An instruction-only recipient receives no fact
sections. Authored inline components remain in the system prompt even when named like a built-in
observation. Stateless Buddy requests retain their current snapshot; the permission reviewer
receives explicit quoted evidence without ambient instruction context;
they do not own a growing conversation prefix.

### Acknowledgement and context loss

The trusted `injected-reminders` transcript record remains the source of truth.
Scanners check an opening marker's provenance before seeking its closing marker,
so quoted audit syntax cannot hide a later trusted record. The complete record
must still have trusted provenance before it is decoded or replayed.
An incremental buffer-local index avoids repeatedly decoding old tool-response
records. Edits before its cursor invalidate the index; character-generation
checks also catch replacements that inhibit edit hooks. A fresh process rebuilds
it. Exact message lookup uses a hash index rather than a nested history scan.
This cache is derived, not separately persisted acknowledgement state.

Before suppressing an update, delivery matches a complete trusted reminder
message against actual user messages in the realized provider payload. The last
matching observation in payload order wins. A substring in tool results, a
system message, or quoted prose cannot acknowledge delivery. Missing or
unsupported representations cause conservative redelivery. Thus filtering,
compaction and rewind cannot silently remove required context while leaving it
acknowledged from excluded source history.

Delivery and its transcript record commit together. Aborted preparation does
not acknowledge an observation. A removed index, empty roster or inactive Goal
produces an explicit current empty/inactive state. Goal procedure text is a
separate component so counter changes do not repeat the completion procedure.
Direct-child agent notices use the same retained transaction.

### Progressive disclosure

The root AGENTS.md keeps project design decisions, before-work retrieval
triggers, verification requirements, and documentation discovery. Detailed
setup, upstream checkout, code-style and test procedures live in
[development.md](development.md).

The system memory-save policy requires Read of `mevedel://memory.md` before
explicit or model-initiated memory mutations. The manual owns root selection,
frontmatter, index maintenance and forget semantics. Existing write permissions
remain authoritative; the manual is not a new permission gate.

Skill catalogs retain canonical invocation names and a one-line purpose (first
sentence/line, at most 160 characters). ListSkills still searches full authored
names/descriptions and returns detailed discovery entries; Skill retrieves the
body. The context-relative total catalog budget remains a final safeguard.
Path-scoped discovery is optional and does not promote a skill into the shared
catalog. Stable skill policy remains available even when that catalog is empty.

### Cache boundaries

The goal is to preserve the eligible matching prefix, not to promise a cache
hit. Native schema, role, model, provider settings and installed behavioral
policy changes can legitimately change that prefix. Provider routing, stored
breakpoints and retention can prevent reuse even for identical requests.

Retained updates preserve earlier request messages. They increase history size;
compaction can retire obsolete observations. See [reminders.md](reminders.md)
and [ADR 0115](adr/0115-retain-delivered-conversation-fragments.md).

### Retained agent configuration

A retained agent keeps the role contract and dynamic component selection resolved
at spawn. Role-definition changes apply to newly created agents. Create a new
agent when its task needs a changed configuration; follow-ups keep the existing
agent's snapshot.

## Resource addressing

Filesystem-shaped tools consume one closed set of eight resource-address
families: `work://`, `artifact://`, `skill://`, `agent://`, `history://`,
`memory://` (including `memory://journal/`), `mcp://`, and `mevedel://`. `Read` supports all eight; `Glob` and
`Grep` support `work://`, `artifact://`, `skill://`, `memory://` (including
`memory://journal/`), `history://saved`, and `mevedel://`. `Grep` also accepts
concrete live history at `history://root[/PATH]`; `ApplyPatch` supports `work://` and explicit memory file descendants alongside ordinary filesystem
paths. Addresses serialize canonical resource locators and do not replace
target-native paths, mentions, or permissions. `mevedel://` is an always-
available, read-only view of packaged Markdown documentation and exposes no
Elisp source.

The resolver prepares an opaque attempt and logical authority facts after
repair, final validation, and pre-use hooks, then permission and any review
authorize it before execution consumes that attempt without reparsing. Content,
backing paths, and helper roots remain behind the boundary. Session work, artifact,
agent, and history resources belong to the session execution target; skills and
memory retain client-local origin; MCP uses the current configured connection.
Journal resources belong to the explicit workspace target and expose only
validated published records; private capture state is excluded.
Freshness and persistence remain owned by each family, while completion and
atomic mention bindings preserve locator identity without side effects.
Standalone/sticky Plan mode keeps session-only `ApplyPatch` available across the
root and retained-agent tree. Any ordinary, shared, memory, or bare endpoint,
including mixed and ordinary-only proposals, is rejected before local
materialization. Other edit tools and `Eval` remain unavailable. Directive
Planning remains strictly read-only and does not allow `ApplyPatch`, including
session-only proposals, or `Eval`. The shared `local/plans/` namespace holds
durable plans for the parent and retained agents. `work://shared/` maps to
workspace-owned `.mevedel/shared/` for working notes and handoffs across sessions,
with agent-chosen filenames and folders. See [`address-to-resource.md`](address-to-resource.md) and
[`ADR 0104`](adr/0104-keep-resource-addresses-closed-and-capability-neutral.md).

## Generated workspace state

`.mevedel/state/` contains internal journal records (`journal/`), target write
coordination (`memory-write/`), clipboard images (`media/`), Git evidence packages
(`review-packages/`), workspace telemetry (`diagnostics/`), and plugin runtime data
(`plugin-data/`). The common parent is ignored by Git. This is persistent state,
not a disposable cache: journal recovery and target write coordination retain
their own cleanup rules, and plugins own their runtime data.

The hourly idle cleanup opportunity also collects generated clipboard/guest PNGs
and review packages older than seven days, up to 100 deletions per run. A complete
target-side ripgrep search of `.mevedel/` retains files mentioned in saved
conversations, historical publication snapshots, input history, memory, journal,
and other retained state. The artifact and diagnostics directories themselves
are excluded. Live buffers, input rings, drafts, and gptel file contexts also
retain files. References are conservative basename matches; unrelated mentions
can retain a file. References outside managed workspace state are not tracked.
Foreign session ownership, linked candidate directories, or an incomplete search
postpone collection. Unknown filenames and files over 32 MiB are retained; a
file changed since selection is skipped. Cleanup runs even without public journal
entries and never calls a model.

Workspace diagnostics rotate when an append would exceed 10 MiB, retaining the
current file and one archive. See [telemetry](telemetry.md).

Public journal entries, curated memory, sessions (including session media and
diagnostics), shared working files, input history, identity, and configuration
remain outside this directory. `mevedel-workspace-state-dir` continues to name
the `.mevedel/` configuration and data root.

## Persistent memory

Memory indexes are read from configured `.mevedel/memory/` and
`.agents/memory/` roots, both workspace-local and user-global. The first
200 lines of each present `MEMORY.md` are included when a profile selects
the `memory` data component, with an absolute modification-date annotation.
Stable `memory-policy` governs use; main/worker also select
`memory-save-policy` before the changing data. This short policy requires
retrieving the memory manual before mutations. Buddy receives no saving
procedure. Unchanged indexes do not rewrite this section at midnight. Durable memory bodies live in linked topic files under the
same root, using `user`, `feedback`, `project`, or `reference`
frontmatter. `MEMORY.md` should contain one-line links only.
LLM-writable. See [`memory.md`](memory.md) for the full layout, save
policy, staleness rules, and the `/remember` consolidation command and cockpit.

Completed saved root turns, digest publication and workspace activation offer
automatic consolidation. A disposable workspace timing cache keeps the hot check
free of target I/O, and the existing transport boundary defers the cold
observation. The coordinator rechecks the 24-hour gate under target ownership
after publication recovery. Five unreviewed digests normally qualify; even one
qualifies one day before ordinary recall expiry. Completed published evidence
is eligible while its source session remains live. Idle workspaces wait for
their next activity boundary and retain unreviewed evidence in the meantime.
Propose is the default; auto applies memory changes through the same checked
decision operation and holds instruction proposals for approval.

## Chat buffer formatting

The data buffer is normally org-mode so gptel can persist
`GPTEL_BOUNDS` (gptel's other config properties are stripped; the
sidecar owns request configuration). Tool results containing
`:PROPERTIES:` are escaped with `,` in the data buffer to prevent
nested-drawer confusion; the rendered view strips those storage
artifacts where appropriate.

## Transcript structure

`mevedel-transcript.el` owns the canonical transcript grammar. Its primary
entry point, `mevedel-transcript-segments`, classifies data-buffer spans as
`(TYPE START END)` where type is `user`, `response`, `tool`, `reasoning`,
`mailbox`, `reminder`, `hook-context`, `task-background`, `render-data`,
`prompt`, or `ignored`.
It combines gptel text-property runs with generated
control ranges, protects literal user examples from structural recognition,
and repairs known org/gptel boundary damage.
Hook audit delimiters are structural only when their complete encoded span
carries live `mevedel-hook-audit` provenance or the persisted
`gptel=mevedel-hook-audit` property; delimiter-shaped user, assistant,
reasoning, and tool text remains ordinary content.

The module also owns the small structural helpers needed to skip leading
property drawers and compaction summaries, recover whole org tool
blocks, parse agent mailbox blocks,
and find the first real user prompt line outside tool/reasoning/summary
scaffolding.

`mevedel-transcript-normalize-properties` applies those same canonical ranges
when a live or restored Org transcript needs its structural `gptel`
properties repaired. `mevedel-transcript-restore.el` owns restoration of
persisted bounds and invokes that normalizer, so persistence and the view do
not maintain their own transcript grammars. Compaction consumes the same
canonical spans directly.

`mevedel-transcript-project-evidence` freezes consumer-selected ranges as
neutral labelled evidence for `mevedel-context-summary.el`. The projection
preserves ordering while excluding hidden UI/audit spans, bounding tool
content, and replacing native media with textual metadata. The stateless
generator owns the isolated request, inherits the session's streaming choice,
accepts both streamed and one-shot delivery, and resolves `summarization` for
continuation/handoff or `journal` for digests unless given a frozen policy. It
owns preflight, heading validation, cancellation, and request telemetry;
consumers retain source selection, hooks, retries, persistence, and mutation.
Plan feeds both Summary locations the same handoff evidence and exact relevance
focus. Here applies the result through root compaction; Worktree generates once,
caches it in retry state, and applies path portability before target insertion.
Agent `context="summary"` projects the frozen realized parent transcript,
excludes the triggering tool call, and applies one handoff result as a distinct
child task-background span before the authoritative task.

View rendering, session prompt indexing/rewind, and compaction all read
these shared spans. They keep their own policies: the view groups and
renders turns, session artifacts build prompt previews, Fork owns projection
state, and compaction chooses response boundaries and preserved-tail policy.
