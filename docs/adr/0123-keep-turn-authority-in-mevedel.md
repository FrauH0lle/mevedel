# Keep turn authority in mevedel across model engines

Status: accepted

## Current decision

Claude subscription access runs the installed Claude agent through ACP, with
mevedel's tools exposed through a private asynchronous MCP bridge. Mevedel owns
request admission, execution authority, effects, canonical transcript and
settlement; the external agent owns sampling, native model history and
compaction. The gptel engine retains its real state machine, while external
turns carry workflow context on the admitted request rather than manufacturing
a gptel state machine.

The bridge resolves every call against current mevedel authority. Discovery is
not permission, and a request's cancellation revokes pending interactions and
effects. The native conversation identity is persisted before its prompt is
sent, so process failure leaves an uncertain turn; the next prompt must
acknowledge current-state reconciliation before further tools. Call identities
are unique only within the live turn. Root and child histories remain separate
and resumable; each directive request or shared-item question starts an
isolated native conversation from its selected context and retains no identity,
so neither enters root native history nor rotates root segments. Only a
retained native identity restricts local-history operations; unstarted and
released markers do not. The existing root interaction and agent registry
decisions remain in
[ADR 0060](0060-centralize-agent-interactions-in-the-root-session.md) and
[ADR 0063](0063-persist-the-agent-registry-explicitly.md).

Context reaches native history only at an observable SDK receipt: the exact
block in the conversation's user-message echo, or a successful hook response
carrying the exact hook output. Required context must be received before
further tool effects and successful settlement. A delivery beyond Claude's
inline hook limit continues the same admitted turn with a new prompt carrying
the complete body; because `SessionStart(compact)` cannot stop the prompt, a
`PreToolUse` hook denies and stops calls while that continuation is pending.
The complete observation baseline stays in Claude's system prompt with the
SDK's first-prompt snapshot disabled, so a resumed launch cannot keep stale
mevedel instructions; restoration after native compaction supplies current
differences. Prompts re-send path instructions and selected context only when
the native conversation lacks their current form. The gptel payload rule
remains in [ADR 0115](0115-retain-delivered-conversation-fragments.md);
behavior is described under
[native context delivery](../sessions.md#native-context-delivery).

Acknowledged typed deliveries use the shared hidden injection record, including
guidance restored after compaction; delivered mail follows as ordinary mailbox
blocks. The receipt correlation marker stays on the wire. The shared record
preserves source types and lets the normal renderer place initial deliveries
with the user prompt and later deliveries at their point in assistant activity.

External history changes what the transcript can faithfully reconstruct.
Claude compaction publishes a successor transcript segment; selected context
must be acknowledged again before subsequent tools. Cross-engine continuation
uses the effective segment and labels a new native conversation as excerpt
continuation. Same-machine resume retains native identity. Operations requiring
an exact native checkpoint mapping, including Fork and Rewind, refuse before
effects when that mapping is unavailable. These constraints are documented in
[session lifecycle](../sessions.md#external-conversation-references), rather
than hidden behind a transcript copy that claims exact model-history
equivalence.

Raw transcript edits mark the owning root or child native history divergent;
continuation requires explicit excerpt recovery. Recovery reads the effective
summary from its canonical root or child bounds and includes it once as labelled
compaction evidence. Ordinary transcript segmentation omits leading root
summaries for rendering, and edits can inherit the closing wrapper's ignored
property; neither determines which summary the recovered model receives.

Connection ownership includes the protocol library's deferred drains and
watchdogs. A connection-local filter holds timers the library schedules while a
TRAMP operation is on the stack and arms them when it returns; outside one the
filter runs unchanged. Watchdogs validate their captured identity before
closing the connection, so a retired timer restored from a suspended list cannot
close a healthy conversation. MCP calls and hook decisions arrive synchronously
on their socket while ACP frames wait for the library's drain timer. Before
queuing socket work, the runner reads ACP output that is already available, then
queues behind the drain that output armed. Every ACP frame Emacs can read when
a socket message is handled is therefore processed first, whichever descriptor
Emacs happened to read first. The receipt and the call reach Emacs through
different relays (the adapter's stdout and the MCP bridge), so a receipt still
inside the adapter is not ordered; Claude emits the receipt and then runs a
model sample before its next call, which leaves that gap theoretical rather
than guaranteed. Queued work waits for idle target transport, and reentrant
arrivals cannot overtake an in-progress segment publication. An interrupted
request still consumes events the agent already reported, such as completed
compaction, and its owned terminal acknowledgement for usage and settlement,
including a reply whose publication is waiting for target transport. It rejects
new tools and hooks: queued ones receive failure replies without running, so
the peer can reach its terminal reply, and cannot acquire a replacement turn's
authority. A hook whose client gave up is not run later. Retained native
invocations use the same bounded acknowledgement; their runtime canceller
defers terminal settlement until the connection completes it.

Provider identity lives in `mevedel-claude-code-backend.el`: the backend type,
model metadata, registration, and generic methods are available without ACP,
MCP, or Claude usage execution. The methods call autoloaded runtime entry
points. Setup and actual Claude requests load those implementations; native
gptel discovery, chat creation, and readiness do not need them.

## Headless recovery and runtime maintenance

Provider readiness belongs to the harness, before root input is committed.
Claude readiness is learned from root turn startup rather than a separate
probe: every send's own launch is the check, and the next send is the retry. A
failed startup leaves its cause as an informational issue, because a blocking
one would also wedge Goal, plan and directive sends; an explicit browser retry
checks again. Configuration, login and runtime changes forget the result. Saved provider selections may fall back to
one configured provider or the host default, with a visible notice; explicit
invalid selections remain errors. A live provider failure moves the session only
on a structured model-not-found signal. Missing presets require owner selection,
without changing permission or sandbox authority.

Only owners that re-check an issue report it as blocking: Codex login, preset
restore and saved-model restore, each clearing only its own issue. Request,
agent and other failures are informational and cleared by the next root
request, which is the user's retry; retained agents never touch root recovery.
Authentication recovery uses asynchronous Codex refresh/device login and the
Claude CLI's subscription login/status interfaces. Credentials remain local;
owner peers receive ephemeral URL/code challenges. Unsent input survives a clean
exit and may resume after repair, while submitted failure or interruption requires
explicit continuation. Native history recovery still requires an idle conversation and
preserves the transcript and effects.

Queued input records one durable delivery intent, a sidecar-only `dispatching`
mark, before dispatch; delivery writes nothing further, so a committed prompt
cannot return to the runnable queue. Other queue and pause changes use the
coalesced sidecar save, so a crash can lose queue changes from its debounce
interval, and a session without a committed sidecar keeps its queue only in
memory; the intent write then has nothing to update. On restart, interrupted delivery and uncertain native
turns require explicit review before continuation. A failed turn pauses delivery
only for input it left undelivered or uncertain. Refused guest input is dropped
with a notice rather than held. OAuth readiness also runs at each gptel sampling boundary,
so expiry during a tool loop settles the turn without synchronous login.

CLI/adapter maintenance runs asynchronously behind an installation lock,
which holds even when `create-lockfiles` is nil. Launches start a due check; an
explicit check reports progress and its result in the echo area. The native CLI
updates on its configured `autoUpdatesChannel` (default `latest`): `claude install`
saves the channel it installs, so a fixed channel would silently move users. Native CLI
releases are pinned before installation can prune them; adapters are staged by
version. Version checks and an initialization-only ACP handshake gate
activation. Current, previous, rejected and active runtimes survive cleanup. A rejected pair
waits for a new version or an explicit check. External installations are not
replaced. Authentication failure is separate from runtime compatibility.
A successful runtime update invalidates cached readiness and clears dependency
failures once per session; it does not mark provider authentication as repaired.
Credential and installation locks remain mandatory when the host disables editor
file locks, and contention never opens a lock-breaking prompt.

## Rationale and consequences

This boundary preserves the existing session workflow and the tested tool
pipeline while using the supported subscription login. Treating Claude as an
HTTP backend would misrepresent its retained history and native tool loop.
Text-emulated tools would replace that loop and add another model-facing
protocol. Direct stream-json would couple session communication to one CLI;
ACP provides reusable session transport, with Claude-specific prompt, tool
isolation, authentication and receipt extensions kept in its engine modules.
Supporting ACP does not promise that arbitrary agents provide those controls.

ACP send completion does not prove that hook context entered the native
conversation, and a hook preview or offloaded file is not the complete input.
The installed adapter exposes the SDK's user-message echoes and hook lifecycle
messages, which make delivery observable without a second history store.
Receipt establishes SDK acceptance, not model understanding or retention
through later compaction.

## Decision history

- **Tool hook decisions:** review found that native turns called post-tool
  hooks but ignored their decisions, allowing a stopped turn to execute another
  tool and sending results a hook intended to withhold. The bridge now applies
  stop, block and replacement results before delivery and publication, and
  passes pre-hook argument replacements through the existing pipeline checks.
  Real MCP subprocess regressions verify both the outgoing payload and the
  absence of a subsequent tool call after a stop.
- **Transport ownership:** the initial callback queue protected work after ACP
  notifications were decoded. Deterministic subprocess tests then showed that an
  ordinary TRAMP wait could discard ACP's earlier drain timer, leaving its queue
  permanently busy, and restore a cancelled startup watchdog that closed an
  admitted conversation. Ownership now begins at the connection's process
  filter and follows its scheduled callbacks. The first retention let-bound
  `timer-list` around the whole filter; `cancel-timer` then reached only the
  temporary list, so cancelled ACP timers fired anyway. Retention now applies
  only while a TRAMP operation is nested, moving filter timers to a held list.
- **Cancellation:** cancellation first discarded every notification after
  abort, losing final usage the agent had acknowledged and a native compaction
  it had completed. Reported events and terminal accounting now survive
  cancellation, without reopening tool or continuation authority.
- **Socket ordering:** a tool call read in the same pass as an earlier context
  receipt ran first and failed the turn with an unacknowledged-context error.
  Queuing socket work behind the drain timer fixed it only when Emacs read the
  ACP pipe before the socket; a review repro with the reads swapped (Emacs reads
  ready descriptors in no fixed order) failed the turn the same way. The runner
  now first reads ACP output already available, so the receipt's drain is armed
  before the socket work's timer.
- **Native call ledger:** call identities were first committed to the session
  sidecar before each tool executed and kept for the conversation's lifetime to
  reject replays. Profiling put that commit at 51 ms of synchronous main-thread
  time per compiled tool call, and the ledger grew without bound and was copied
  every turn. The protection was redundant: the in-flight record is already
  durable before the prompt, a crash restores it as uncertain, and uncertain
  history requires an acknowledged reconciliation notice before more tools. The
  adapter was never observed replaying an old `toolUseId`, and a model retry
  uses a new one. Per-turn uniqueness remains; the ledger was removed.
- **Directive identities:** directive turns first persisted their identity under
  the directive scope, and released or failed startups left `unstarted`
  markers. None were ever resumed, but any entry permanently refused Fork,
  Rewind, Redo, Save As, control transfer, manual compaction and side
  conversations after one Claude directive or child, including the two-machine
  transfer workflow. Directive identities are no longer stored, and the guard
  counts only retained native identities.
- **Oversized delivery:** mail first reached a running conversation only
  through hooks, and updates above the hook limit failed explicitly, needing
  another user turn. A child result above the limit could never reach a running
  native root: the hook skipped it and WaitAgent returned immediately on the
  non-empty mailbox, looping. Prompt submission now carries mail in full, and an
  oversized hook update, including one message, continues the admitted turn
  through another prompt.
- **Compaction stop:** the continuation path first assumed that
  `SessionStart(compact)` honors a stop. A live compaction run showed the CLI
  continuing, with the MCP admission guard failing closed, and showed that
  `PreToolUse` also needs an explicit deny beside its stop.
- **Repeated prompt context:** every prompt first re-sent acknowledged path
  instructions and user-placement selected context, accumulating copies in
  Claude's retained history. Content hashes now suppress unchanged deliveries.
- **Transcript form of receipts:** receipts first stored the wire batch,
  marker included. That lost source labels, placed initial guidance under the
  assistant, rendered markers as user prose or reasoning, leaked them into
  excerpts and cost the view a regexp pass over every hidden segment. The
  transcript now holds only the typed record and mail.
- **Native summary segments:** restoration hooks alone left the transcript with
  all pre-compaction history, which continuing through gptel would revive as
  raw turns. Native compaction events now publish Claude's retained summary as
  a segment.
- **Cross-engine continuation:** an initial blanket guard refused switching
  engines once history existed; the user rejected it. Effective native
  compaction segments and gptel's reasoning-safe projection provide a usable
  continuation boundary in both directions.
- **Setup and maintenance:** the initial integration required terminal login
  and an explicitly installed, pinned adapter. Headless browser use exposed
  inaccessible recovery prompts and stale installations. Owner-only typed
  recovery and automatic checked maintenance replaced that setup policy while
  retaining the installed CLI's credential and conversation ownership.
- **Recovery scope:** the first recovery layer blocked a session on any
  classified request failure, checked Claude readiness with a full native
  session whose result expired after 30 seconds, paused delivery after every
  failed or aborted turn, held refused guest input with a blocking issue, and
  saved the full session on every queue change. Regexes over provider text,
  such as "upgrade" in a billing message, persisted blocking issues no Emacs
  action cleared. Most Claude sends were refused once and started Claude twice.
  A single abort stopped follow-up delivery indefinitely. A refused `/compact`
  from a guest locked out the host. Queue saves cost about 70 ms each on a
  400 KB transcript. Blocking state now belongs to owners that re-check it,
  readiness to turn startup, the pause to affected input, and queue durability
  to the sidecar.
- **Provider discovery:** recognizing the Claude backend first loaded ACP, MCP
  and usage execution, even for native gptel sessions. The backend module now
  owns the small provider contract separately from setup and execution.
