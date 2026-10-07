# Claude engine implementation evidence

Date: 2026-10-06. Worktree and branch: see the [handoff](claude-code-engine-handoff.md).
Implementation and final validation are complete. See the
[acceptance index](claude-code-engine-acceptance.md) for the final result; the
chronological entries below retain earlier incremental results and failures.

## Feasibility observations

- Installed Claude CLI: 2.1.290. Supported `claude auth status` reported
  `loggedIn: true`, `authMethod: claude.ai`, `apiProvider: firstParty`,
  `subscriptionType: enterprise`. No credential files were read. This proves
  the available Enterprise subscription route, not a live Pro/Max account test.
- Installed local adapter: `@agentclientprotocol/claude-agent-acp@0.86.0`,
  SDK 0.3.287. Source inspection commit:
  `0724b17c581e30b1242cfbec7bce44c21df99525`.
- ACP Emacs client: acp.el 0.15.2, commit
  `242cef63d76cc1073485847f67a21f6d8406d158`.
- Two bounded real ACP prompts successfully called the live Emacs MCP server's
  synthetic Probe tool once each and returned `MEVEDEL-314159`.
- Both runs used a neutral temporary cwd, replacement system prompt,
  `tools: []`, explicit MCP configuration, `settingSources: []`, disabled
  slash commands and auto-memory, and removed inherited API-route environment
  overrides. CLI login was preserved. Full hooks/skills isolation remains to
  be tested; absence of unwanted tools is not proof of all isolation behavior.
- Raw SDK initialization reported exactly `mcp__mevedel__Probe` as the tool
  roster. ACP advertised image/embedded context, load, resume and prompt
  queueing support. Advertisement alone is not acceptance evidence.
- MCP `tools/call.params._meta["claudecode/toolUseId"]` matched ACP's
  `toolCallId` exactly. This supplies correlation without argument matching.
- Live probe and logs are disposable local files under
  `.scratch/claude-code-engine/`. Normal root selection/send is now wired as
  described below; the full feature is not complete.

## Implemented and checked

- Private asynchronous Unix-socket MCP server plus a Python standard-library
  stdio bridge. Initialization, discovery changes, concurrent pending tools,
  cancellation, disconnect and reentrant teardown have seven passing ERT
  contract cases through the real bridge. File modes restrict the endpoint.
- Client metadata is preserved for adapter-specific correlation. Duplicate
  completion cannot send another response. Closing retires pending work before
  invoking cancellation callbacks.
- The structured provider projection preserves the existing pipeline's result
  persistence and typed outcomes. MCP calls use the captured request, reject
  obsolete ownership, preserve native tool IDs, run real Read and permission
  behavior, and deliver native image blocks with separate display metadata.
- ACP lifecycle uses the installed acp.el package. Deterministic subprocess
  tests cover streaming, session isolation, cancellation acknowledgement,
  closing, process loss, resume and missing history. Process loss cannot
  silently restart an uninitialized agent or retry the prompt.
- Claude launch configuration checks supported CLI authentication status and
  tested dependency versions, removes inherited API routing, and supplies a
  neutral local cwd and generated prompt/tool settings.
- 315 focused ERT cases passed across the new transport/pipeline boundaries and
  the existing pipeline/input-repair regressions. This is not the full suite.
- All six new/changed implementation modules byte-compiled without warnings,
  including the model-environment pin. Repeat compilation at final acceptance.

## Live production-module probes

These use the new modules in isolated Eask sessions, with only the spawned Claude
process using the existing login HOME. Mevedel's test state stays temporary.
They are transport/pipeline evidence, not proof of composer or persistence UI.

- A real Read call returned a nonce from a temporary session file. After the
  ACP process closed, a new process resumed the same external session and
  recalled that nonce without another tool call. Exact exposed tool roster:
  `mcp__mevedel__Read`. Evidence: `live-pipeline.el` / `.log` in the scratch
  feature directory. The first execution had a 90-second prompt timeout; its
  scratch `maxTurns` assignment was corrected afterwards, so that execution
  must not be described as enforcing the intended three-iteration cap.
- A configured `PostToolBatch` command hook returning `continue: false` stopped
  the agent after one real pipeline Read, before a second filename-dependent
  read. ACP returned `stopReason: end_turn`; raw SDK result was `success` with
  `stop_reason: tool_use`. No transport abort was used. This is evidence for a
  successful tool-boundary stop. Production delivery still needs to bind that
  hook to mevedel's actual Goal/agent boundary request.
- That probe exposed a consequential model-selection detail: the adapter's own
  settings manager can override `options.model`. Its first result named
  `claude-opus-5-5` despite requesting `sonnet`. Pinning `ANTHROPIC_MODEL` to
  mevedel's explicit selection as well as setting the SDK option fixed the
  behavior; the repeated probe named `claude-sonnet-5-5`. Regression coverage
  verifies that inherited model selection cannot override the launch pin.
- The raw adapter usage also included a small Haiku helper workload. Model
  accounting must distinguish the requested conversation model from the
  external runtime's helper calls; do not claim every token uses one model.

The SDK's [settings-source contract](https://code.claude.com/docs/en/agent-sdk/claude-code-features)
documents the remaining managed-policy/global-config inputs even with empty
setting sources. The [hook contract](https://code.claude.com/docs/en/hooks)
provides `SessionStart` with source `compact` and `additionalContext` before the
next model request. A live 22-Read turn verified that mechanism: auto-compaction
occurred after call 15, the hook supplied the previously undisclosed marker
`RESTORED-938127`, the model continued the remaining seven reads and included
that marker in its final answer. ACP and the SDK both settled successfully.
Evidence: `live-compaction.el`, `context-hook.py` and
`live-compaction-100k.log` in the scratch feature directory. This is a synthetic
file-chain stress test, not the required real programming acceptance task.

An initial version requested a 10k auto-compact window and completed all 22
reads without compacting. CLI validation then established that the supported
range starts at 100k; the successful retry used both the explicit `--autocompact
100k` argument and valid settings. Do not infer effective controls merely from
an options object: invalid programmatic settings can be silently ignored.

The documented 10,000-character cap per additional-context value must be
accounted for when delivering large required instruction sets.

## Next integration boundary

The live resume result supports a small ownership design: one fresh ACP process
and private MCP endpoint per mevedel request, resuming an independently retained
external conversation ID between turns. Every server can capture one immutable
request and tool scope, avoiding delayed-call authority transfer across turns.
The startup cost is an explicit tradeoff; process multiplexing is unnecessary
for initial correctness. Root, directive and retained-agent identities still
need their own durable external-history references.

The native hook bridge now uses the same private socket for request-owned
control events. Generated Claude `PostToolBatch` hooks read the current boundary
stop decision and return `continue: false`; a lost owner also stops instead of
silently continuing. Hook events are not model-visible tools. A live Goal test
paused the actual mevedel Goal after the first Read; the generated hook returned
`goal-paused`, Claude stopped successfully without reading the second file,
and the common transaction published one turn. Evidence: `live-goal-boundary.el`
and `live-goal-boundary.log` (3 native iterations maximum, 90-second deadline).

The production `SessionStart(compact)` context restoration and acknowledgement
remain to be built. The scratch compaction probe above proves native injection,
not a completed mevedel context-delivery implementation. Account for the native
additional-context size limit when restoring large instruction sets.

## Request-owned turn integration

`mevedel-engine.el` now supplies workflow context access for real gptel FSMs
and admitted requests. The common terminal transaction, Goal accounting, Plan
handoff and follow-up drain accept either owner. External turns use the request
context and do not create a gptel FSM. Native gptel still stores its workflow
context in its real FSM; moving that native ownership fully onto the request
remains open. The direct skill-fork result path no longer fabricates an FSM.

Tool dispatch carries an engine owner through the existing pipeline, including
nested ToolCall. Goal tools validate that owner, and inherited agent Goal
accounting now names its captured owner rather than assuming an FSM. A real MCP
pipeline test creates a Goal, attributes usage, pauses at a boundary and settles
25 tokens exactly once with no FSM.

`mevedel-acp-turn.el` owns a fresh connection and private MCP endpoint for one
admitted request. It streams through gptel's existing insertion helpers (these
take plists, not FSMs), renders canonical tool blocks from pipeline outcomes,
rejects duplicate native tool identities and closes both processes at terminal
settlement. ACP tool notifications never execute effects. It shares final patch
capture and the ordinary success/error/abort transaction. Interruption retains
partial text; a failing post-response hook warns without stranding admission.
Retained identity is handed to the caller before prompting; durable history
storage and composer/model-selection wiring are not implemented yet.

The installed Claude adapter passed a bounded live run through this complete
request runner: one real Read, native tool identity, streamed answer, one
committed/published turn, no gptel FSM. Evidence: `live-turn.el` and
`live-turn.log` in the scratch feature directory (3 native iterations maximum,
90-second deadline). This uses the existing Enterprise login and isolated
dependency setup; it is not a Pro/Max account or normal composer acceptance.

Validation so far: 482 focused Goal/tool/turn/agent-runtime/skill cases passed;
75 ACP-turn/final-patch/settlement cases passed after adding the external runner.
16 transport/adapter/turn cases then passed with the generated hook and a
batched-stream regression: pending text is flushed before canonical publication.
The final focused integration run passed all 584 cases. A full compile of
221 files produced no warnings, followed by Eask bytecode cleanup. The full suite and required final review remain outstanding.

## Test environment

Eask dependencies are a frozen copy of the main checkout's isolated environment,
Emacs 31.1 and gptel-20260906.334. They are not claimed equivalent to the user's
running editor. Loaded setup equivalence must be checked before representative
end-to-end provider tests. Source hashes of the copied dependency:

- gptel.el: `9b436e2bf0f1775c1cad0fedcc39bb3274028f0d5caf0326114db8f6f43ac2fb`
- gptel-request.el: `dbaa2d15f2569b12cdfbb88f4f86d1eb0e1c54901f28f8a3e3d4ab772a5b9354`

No running default Emacs server was available for a read-only loaded-library
inspection (`emacsclient` reported no socket). The standalone probes above do
not claim equivalence to an interactive editor's configured dependency branch.

The upstream gptel inspection checkout was refreshed to
`edb3fee3b5266e9060f6d121e9b3914eb7c3409d`.
GNU ELPA archive refresh stalled; a temporary, untracked `Easkfile` redirects
archive metadata to local snapshots of the already installed environment.
Remove this machine-specific runner override before committing.

## Remaining acceptance work

The PRD remains canonical: complete native request-state migration, guided
dependency setup and complete Claude authentication/isolation validation,
all workload call sites, directive/agent histories, complete prompt inputs,
permissions/patch/media/remote behavior, interruption and Goal continuation
acceptance, context restoration during compaction, editor restart and retained
agent resume, visible crash reconciliation, complete command/menu capability
guards, full tests, maintained documentation and review are still pending.
The bounded probes do not satisfy the complete long-turn programming task or
workflow acceptance cases. Ordinary root selection, composer submission and
close/reopen now have passing integration evidence.

## Immediate next work

Finish prompt/context delivery, retained agents and all workload dispatches.
Preserve the confirmed native ownership and batch-stop behavior while completing
context restoration and UI acceptance. External-history state is persisted, but
an `uncertain` record still needs visible reconciliation before continuation.

Usage normalization needs adapter-specific scope: the ACP SDK schema describes
`Usage` as accumulated session totals, but Claude adapter 0.86.0's `turnOutcome`
returns `session.accumulatedUsage`, reset on turn activation. Its `_meta.quota`
model breakdown can include helper work with a different scope. Do not sum that
breakdown into turn usage or treat `usage_update.used` (current context size)
as request spending. The existing gptel Anthropic normalizer uses `:input` for
uncached input plus cache creation, `:cached` for cache reads and `:output` for
output; Goal accounting currently follows normalized input plus output. The
external runner now normalizes final prompt reports with these semantics. It
does not yet obtain reliable per-step spending or claim subscription allowance
telemetry. Missing/invalid counters remain absent. Sources
inspected: adapter `sessionUsage`/`turnOutcome`/`turnQuotaMeta` near lines
10039-10120, result handling near 5915, and the frozen gptel Anthropic
`gptel--anthropic-update-tokens`.

## Normal root sessions and usage

- Registered Claude Code in ordinary provider selection with an HTTP-dispatch
  guard. Root generated sends and composer submissions use the admitted ACP
  runner. Existing API sends retain the gptel path.
- Persisted scoped external IDs with machine, stable local installation and
  ready/in-flight/uncertain state. Identity publication runs outside a busy
  execution transport and precedes the native prompt. A restored in-flight
  record becomes uncertain. The sidecar version is now `v0.5.7`; older saved
  schemas are deliberately not migrated.
- Deterministic real-process tests cover two normal sends, retained ID reuse,
  actual save/close/reopen, deferred publication and composer draft preservation
  including a multiline draft starting with `>`.
- Save As/Fork/Rewind/Redo, manual compaction, /btw and cross-engine history
  changes have direct-call guards. Menu disabling and the complete operation
  audit remain outstanding. Adapter 0.86.0 supports a fork extension; the
  missing piece is a faithful mapping of native message boundaries to mevedel
  checkpoints and independent histories, not absence of a protocol method.
- Terminal normalization uses Claude's per-prompt tally, includes cache writes
  in normalized input and keeps cache reads separate. It ignores quota model
  rows and context occupancy. Terminal failure metadata cannot become success.
  Duplicate terminal replies settle/charge once; the real-MCP Goal pause test
  charges exactly 140 tokens from 100 input + 17 cache writes + 23 output,
  excluding 1000 cache-read tokens.
- Broader regressions found nil turn-count fixtures and the session clone slot
  policy/count needing updates. After correction, the focused 51-case
  session/fork/ACP/adapter run passed; the additional six runner cases passed.
  The full suite has not run. All 222 source files compiled without warnings
  in `outcome-compile.log`.
- Bounded live `live-session-reopen.el` passed in 6.52 seconds: normal send,
  one Read, canonical save, close/reopen through persistence, new adapter
  process resuming the native ID, marker recall without another tool call.
  Both native init events named `claude-sonnet-5-5`; low effort and maxTurns=3
  were asserted in launch options before dispatch. Normalized usage was
  `(input 2251, output 104, cached 5315, cache-write 2247)` then
  `(input 72, output 10, cached 3843, cache-write 70)`. This exercised the
  available Enterprise login, not a live Pro/Max account or an Emacs restart.
- Rechecked the [subscription notice](https://support.claude.com/en/articles/15036540-use-the-claude-agent-sdk-with-your-claude-plan)
  and [integration terms](https://code.claude.com/docs/en/legal-and-compliance)
  before the live check. The June 15 pause still applies on the retrieved page;
  login and credentials stay inside the user's unmodified Claude binary.

## Isolated text workloads

- Added `mevedel-engine-request-text` with native gptel and Claude ACP
  implementations. `mevedel-acp-text` owns one tool-free conversation and
  buffer-scoped cancellation; it translates streamed/collected text and
  normalized usage to the existing callback convention. No fake FSM, root
  request, retained root ID or MCP tools are acquired.
- Naming and permission review now dispatch through that interface. Summary
  generation (including journal digests) uses it for the external engine;
  native summaries retain their existing WAIT-side output-limit handling.
  Digest validation and client byte limits remain enforced. Native CLI output
  control negotiation remains outstanding, so the external branch must not be
  described as enforcing gptel's server token limit.
- Actual subprocess fixtures verify workload model overrides, no root policy
  leakage, no root admission changes, no persisted root history, valid naming,
  guardian allow-once and malformed-response human fallback, and journal
  digest validation/usage. Existing native workload tests also pass.
  `workloads-green.log`: 125/125 tests passed.
  `workloads-compile.log`: all 223 source files compiled without warnings.
- Prompt projection now removes trusted display/audit side channels before
  encoding ACP input, retaining text properties until the existing scrubber
  has recognized ownership. A real-peer echo test covers this. Pending skill
  context drains inside the runner's startup failure boundary.
- Call-site audit is captured in local `model-call-sites.txt`. Buddy is **not**
  tool-free: it owns bounded note tools. Consolidation also owns bounded
  investigation tools. Both require an MCP adapter for their existing tool
  owners, without manufacturing session pipeline authority. Agent and
  directive dispatch, raw-send/init paths, steering, and shared editing still
  need explicit engine integration/audit.

## Scoped background workloads

- Added `mevedel-engine-request-workload` and `mevedel-acp-workload` for
  isolated ACP conversations with caller-owned tools. Existing gptel tool
  functions retain authority; the bridge owns private MCP/process lifetime,
  native call identity checks, once-only callbacks, and per-batch guards.
  This path creates neither a root request nor a synthetic gptel FSM.
- `mevedel-acp-text` now accepts caller-owned MCP scopes and forwards native
  reasoning chunks. Claude's PostToolBatch hook can invoke a workload guard
  and deliver its bounded additional context. The inspected SDK declares
  `PostToolBatchHookSpecificOutput.additionalContext`; this is an adapter
  capability, not a generic ACP guarantee.
- Buddy uses its existing captured note tools through that bridge. Real
  subprocess tests cover admitted versus foreign buffers and abandonment at
  a native batch boundary: no subsequent note appears and unreviewed changes
  remain eligible. Native Buddy tests still pass.
- Consolidation now resolves Claude Code without reaching gptel's HTTP payload
  builder. It retains captured Read/Glob/Grep authority, async search cleanup,
  the 180-second deadline, output guards, 64-call/64-KiB result budgets,
  proposal parsing and once-only settlement. Explicit system/input/tool
  schemas determine initial input admission; visible accumulated output,
  results and reminders supply a conservative follow-up context estimate.
- Client output guards run on streamed text/reasoning and before each tool.
  Native batch boundaries stop exhausted work and emit the existing sparse
  budget reminders. Available final-prompt usage is charged once; there is
  no exact per-step provider usage or server output ceiling on this path.
  ACP review `:rounds` counts completed tool batches plus terminal prompt
  completion, not inaccessible HTTP retries.
- A new negative case exposed an MCP-layer gap: rejected unknown tools never
  reached the workload owner, allowing a later final reply to succeed. MCP
  now offers an optional rejection callback; scoped workloads retire on that
  diagnostic. Ordinary MCP error behavior remains covered and unchanged.
- Red/green evidence: `memory-acp-red.log` failed because the selected external
  backend entered gptel payload parsing. `memory-guards-red.log` exposed the
  unknown-tool settlement gap. Final `scoped-workloads-regression.log` passes
  156/156 cases across Buddy, consolidation, isolated text, MCP, admitted ACP
  turns, Claude normalization and context summaries. Real fixtures exercise
  both MCP subprocesses and the generated native hook command. Tests include
  rejection before the first tool on oversized arguments, refusal of call 65,
  and cancellation before late replies. This is deterministic evidence, not
  another live subscription acceptance claim.
- `scoped-workloads-compile.log`: all 224 source files compile, no warnings.
  Eask cleaned bytecode before the final regression run. `git diff --check`
  is clean. Updated implemented memory/Buddy/module-map contracts.
- Remaining work is still substantial: shared native request context,
  directives and retained-agent dispatch, full input/context delivery and
  compaction restoration, uncertain-effect reconciliation, setup/capability
  UX, Emacs-restart acceptance, provisioned remote acceptance, the full suite
  and the real multi-file programming acceptance task. No completion claim.

## Directive ACP integration

- Directive requests now dispatch through the resolved engine, preserving
  request-local model/effort overrides. Native gptel still uses its real FSM.
  The directive and Plan callbacks read generic engine context; the admitted
  ACP request is their owner, with no synthetic FSM.
- Native and ACP turns now share the terminal callback/settlement helper after
  final patch capture. ACP captures the response-end marker before that patch
  work. Success, subscription failure and cancellation create exactly one
  durable directive activity; implementation attempts retain patch evidence.
- Pending skills and directive read-only rules are captured through one shared
  request preparation helper. A real MCP test deliberately exposes ApplyPatch
  in a custom discussion preset and proves the request rule denies mutation
  even with Full Access. A following implementation applies that patch and
  captures the successful immutable attempt.
- Each directive prompt starts a fresh native conversation from its existing
  precisely selected evidence. This preserves discussion versus implementation,
  retry and request-changes semantics; it does not accumulate hidden prior
  attempts. The latest native directive ID remains a correlation reference in
  its own scope. Root chat retains/resumes its separate conversation.
- A new workflow test found that committing a native identity via a full save
  tried to serialize an intentionally open directive boundary. The directive
  path now publishes its complete prior transcript before opening the boundary,
  then strictly commits only the new identity metadata before model dispatch.
  Generalized the existing strict sidecar publication entry to PID-lock
  sessions; a real-file test verifies that metadata changes leave unpublished
  transcript input out of the committed segment.
- A second test exposed a scope error in history guards: an isolated Claude
  directive blocked native root chat. Root guards now inspect root identity
  and completed ordinary prompt entries, excluding shared directive turns.
  An empty root can change engines after directive-only work; an established
  root history still refuses unsupported cross-engine transfer.
- Real ACP/MCP subprocess fixtures exercise root -> local discussion -> local
  follow-up -> resumed root -> failed/cancelled discussion -> denied mutation
  -> permitted implementation. They assert Haiku directive overrides leave
  the root on Sonnet, callbacks occur once, eight turn identities reach the
  committed sidecar, and final patch evidence is complete. A separate Plan
  fixture presents the real approval, accepts with an implementation model
  selection, settles the attempt, and preserves a multiline ordinary composer
  draft beginning with `>`.
- Verification: initial `directive-acp-red.log` reached unsupported native
  payload parsing. `directive-root-scope-red.log` caught the over-broad root
  guard. `directive-regression.log` passed 248/248 cases. The final broader
  `directive-final-regression.log` passed 428/429; its sole failure was the
  new assertion using `:turn-count` instead of the sidecar's
  `:total-turn-count`. After correcting that assertion,
  `directive-durable-green.log` passes all 16 subscription workflow cases,
  including committed-state and no-autosave-failure checks. Other cases in
  that broader run passed unchanged: native directive/Plan, cold owner load,
  deferred ownership, shared engine, artifacts, presets, models and chat.
- `directive-compile.log`: all 224 source files compile without warnings.
  Eask cleaned bytecode before the broad run; `git diff --check` is clean.
  Updated sessions/architecture and amended ADR 0091 with the context and
  metadata-publication decisions and their evidence.
- Next integration boundary is retained agents: `mevedel-agent-exec-run`
  unconditionally creates a gptel FSM; runtime stores it in `runtime-fsm` and
  has its own callback-driven terminal publication/retry gate. The ACP root
  runner currently owns root settlement, so attaching it to a child unchanged
  would be incorrect. Preserve the runtime's frozen configuration, caller
  request/effect authority, capacity, retained transcript, mailbox, and
  publication gate when introducing its external turn owner. Root/native
  request context migration, complete context/media/compaction delivery,
  uncertainty reconciliation, setup/capability UX, restart/remote/live
  acceptance and the full-suite/review gates remain outstanding.

## Retained-child MCP ownership

- Confirmed that the existing agent runtime owns its terminal transaction and
  does not admit a root `mevedel-request` in the child buffer. Reusing the root
  ACP runner unchanged would publish the wrong lifecycle. External child
  context now has a runtime-only slot on the invocation, accessed through the
  same engine interface as native FSM context.
- The MCP pipeline entry now accepts the captured retained invocation as its
  owner. It verifies buffer identity, current invocation, owning session,
  running/unsettled state, and absence of a borrowed root request. The native
  child path remains unchanged. Root and child MCP admission both reject an
  owner whose turn has requested a boundary stop.
- Deterministic MCP seam coverage reads a real file under child authority and
  denies an actual ApplyPatch under child Plan restrictions despite root Full
  Access. Separate checks reject settled/replaced children, mismatched session
  authority, a borrowed root request, and calls after a boundary stop. The
  root request remains independent; no synthetic child FSM is created.
- Red evidence: `retained-mcp-red.log` rejected the active child; after child
  ownership support, `retained-mcp-boundary-red.log` exposed a handler running
  after a requested turn stop. Both are fixed. `retained-mcp-regression.log`
  passes 72/72 MCP, turn-engine, agent, runtime and ACP-turn cases; the added
  real Plan-patch refusal passes in `retained-mcp-plan-green.log` (8/8).
  `retained-mcp-compile.log` compiles all 224 files without warnings. Eask
  cleaned bytecode afterwards; `git diff --check` is clean.
- This is the child tool-ownership seam, not completed retained ACP dispatch.
  Next connect agent execution to ACP while preserving runtime publication,
  retry/interrupt gates, frozen configuration, native conversation identity,
  mailbox delivery and child usage. Existing `runtime-fsm` remains the actual
  native handle; external context is separate. All previously listed mandatory
  context, recovery, setup, acceptance and review work remains outstanding.

## Retained-child ACP dispatch and settlement

- `mevedel-agent-exec-run` now routes external selections to the Claude child
  driver with the existing frozen configuration and bookkeeping callback.
  Native execution still creates its actual gptel FSM. The agent runtime
  captures the prepared task plus selected background before dispatch, so a
  follow-up sends only its accepted new input instead of replaying the whole
  displayed transcript.
- Generalized the existing ACP turn runner to accept a retained invocation and
  a child settlement callback. Transport, canonical streaming, MCP dispatch,
  cancellation and remote-transport deferral remain shared. Root final patch
  capture and turn settlement are not run for a child. Native Claude boundary
  hooks can read the child's own stop/cancellation state.
- The child driver composes frozen role instructions with only that role's
  selected retained observations. Native histories are keyed by the canonical
  child path; identity is strictly committed to the root sidecar before the
  prompt. Follow-up uses that identity with the frozen model, independently of
  later root settings. Terminal status updates the history reference to ready
  or uncertain. Final normalized usage reaches the original RESULT transaction
  and existing Goal charging interface.
- Agent interruption now retires an external transport through an invocation
  canceller; the normal runtime still owns terminal publication and capacity.
  A real public-runtime publication rejection exposed an unpublished child
  left unsettled, keeping its terminal retry alive. Cleanup now marks either
  provider kind settled and drops both runtime handles. The existing retry
  gate can then retire without another provider event.
- The subscription workflow fixture now exercises public spawn, real MCP Read,
  ordinary retained follow-up, interruption, process death and rejected
  publication. It checks independent root ownership/turn count, child-only
  instructions, frozen Sonnet after the root changes to Haiku, retained native
  ID, canonical tool evidence, one RESULT per child turn, settled usage and
  capacity release. First fixture drafts needed valid tool specs, registration
  and a role description; those failures were fixture errors, not evidence
  about ACP. `agent-acp-publication-red.log` is the meaningful failed lifecycle
  assertion before the cleanup fix.
- `agent-acp-verified.log` passes 305/305 cases across agent control, execution,
  runtime, conversations, persistence, definitions, subscription workflows,
  admitted ACP turns, MCP tools and Goals. `agent-acp-compile.log` compiles all
  225 files without warnings. Eask cleaned bytecode before final verification;
  `git diff --check` is clean. No live subscription call was made in this slice.
- Retained-agent integration remains incomplete: implement in-turn mailbox and
  roster delivery/acknowledgement, reminder and compaction restoration, correct
  custom sample-limit handling, activity/tool-boundary saves and restart
  recovery; validate nested agents and parent-wait/child-permission behavior.
  The shared root/native request context, complete media/input delivery,
  uncertainty reconciliation, remaining model-call audit, setup/capability UX,
  provisioned remote/live programming acceptance, full suite, ADR review and
  final code review/commit remain required. No completion claim.

## Acknowledged native mailbox delivery

- External root and child turns can now deliver queued MAIL/RESULT/USER/
  EXECUTION records at PostToolBatch. The new Claude context owner captures
  whole FIFO messages within the native inline limit and leaves them unread
  while delivery is pending. Directive conversations do not consume root mail.
- Enabled SDK hook lifecycle events in the isolated launch. The shared ACP
  runner routes owned notifications to an adapter observer before rendering.
  Only a successful PostToolBatch `hook_response`, with exit code zero and the
  exact complete JSON `additionalContext`, acknowledges a batch. A unique
  marker prevents identical later batches from accepting an old receipt.
  This is SDK acceptance, not a claim about model understanding or permanent
  retention through compaction.
- Acknowledgement writes the canonical recipient transcript once, then removes
  only the captured message objects. Mail arriving after capture stays unread.
  Consumption retains the existing observational persistence/debounce contract;
  process loss before that save can redeliver mail. Failed/missing receipts do
  not manufacture delivered transcript content.
- Verified the [official native hook limit](https://code.claude.com/docs/en/hooks#json-output):
  strings beyond 10,000 characters become a file reference and preview. The
  implementation counts UTF-16 units conservatively and never treats that
  preview as delivery. Oversized whole mail remains queued. Initial-prompt
  delivery is still required to handle it fully; this is not a completed
  large-input solution.
- The ordinary child workflow test runs real ACP, MCP and hook subprocesses.
  It queues steering into a running child, admits a subsequent SendMessage
  during receipt flight, and checks duplicate receipts, foreign sessions,
  mismatched output, malformed JSON, failed hooks, missing receipts and a
  5,000-emoji oversized message. Only the exact successful batch is consumed;
  late mail stays queued and the root transcript never receives child mail.
  `mail-acp-red.log` exposed the missing delivery; `mail-acp-contracts.log`
  passes all 26 focused cases. `mail-regression.log` passes 210/210 agent,
  subscription, ACP, MCP, Buddy and memory-review cases.
- One bounded live call (`live-mail.el`, `live-mail.log`) used the installed
  unmodified CLI 2.1.291, adapter 0.86.0 and the existing Enterprise login via
  `CLAUDE_CONFIG_DIR`; no HOME replacement or credential inspection was needed.
  Rechecked the subscription support notice and integration terms before it.
  The SDK maxTurns was 3, effort low, built-in tools empty, and actual model
  init was `claude-sonnet-5-5`. In 7.81 seconds the model performed one mevedel
  Read, the native SDK emitted the exact successful receipt, the mailbox
  emptied, and the final answer included `MAIL-ACK-847296` supplied only in
  hook mail. Usage: input 3978, output 308, cached read 3739, cache write 3974.
  This verifies native receipt behavior, not a Pro/Max-specific account run.
- The isolated gptel source hashes remain
  `9b436e2bf0f1775c1cad0fedcc39bb3274028f0d5caf0326114db8f6f43ac2fb`
  and request source `dbaa2d15f2569b12cdfbb88f4f86d1eb0e1c54901f28f8a3e3d4ab772a5b9354`
  under `.eask/31.1/elpa/gptel-20260906.334/`; no equivalence to a live editor's
  dependency branch is claimed. `mail-compile.log` compiles all 226 files with
  no warnings, followed by Eask bytecode cleanup. `git diff --check` is clean.
  Updated agent/module contracts and ADR 0115 with the receipt boundary.
- Next: initial-prompt mail (including oversized messages), direct-child roster,
  reminder/observation acknowledgement, in-turn compaction restoration and
  context-loss recovery. Native hook receipts are available as an evidence
  seam for that work, but mail acknowledgement must not be conflated with
  general observation retention. All previously recorded recovery, remaining
  engine-call audit, setup/capability UI, live programming/remote acceptance,
  full-suite and review/commit requirements remain open.


### Initial-prompt mail and oversized delivery — 2026-10-06

- Root and retained-child turns now snapshot their mailbox immediately before
  ACP prompt submission, after readiness has persisted native identity. The
  shared ACP runner accepts a deferred content function for this ordering.
  Directives still cannot consume root mail. Mail is a separate complete text
  block, with a fresh delivery marker, rather than a hook-limited preview.
- Only an exact full mail block in the owning SDK user-message echo consumes
  that prompt batch. Nested-agent echoes, foreign sessions, changed text,
  duplicate receipts and missing receipts cannot acknowledge it. The canonical
  transcript records acknowledged mail once. Late mail continues through the
  bounded PostToolBatch path; messages too large there wait for the next prompt.
- Consulted refreshed gptel source (already up to date), the installed SDK's
  SDKUserMessage contract and the adapter's promptToClaude/raw-message path.
  The adapter retains plain ACP text blocks in its SDK user content. Matching
  the complete uniquely marked mail block avoids depending on unrelated
  conversion of other prompt content or on mere request submission.
- `initial-mail-red.log` demonstrated the missing pre-turn delivery.
  `initial-mail-final.log` passes 19 subscription workflow cases, including
  12,000-character root mail, large child mail, missing/foreign/mismatched/nested
  receipts, duplicates and delivery on a fresh turn after a missing receipt.
  The existing child hook test now queues mail through real SendMessage after
  prompting, preserving independent coverage of in-turn hook delivery and
  mail arriving during receipt flight. `initial-mail-regression.log` passes
  198 ACP, agent-control, MCP, Buddy and memory-review cases.
- Bounded live acceptance (`live-initial-mail.el`, `live-initial-mail.log`):
  installed CLI 2.1.291, adapter 0.86.0, same existing Enterprise login through
  CLAUDE_CONFIG_DIR, model init claude-sonnet-5-5, low effort, empty built-in
  tools and maxTurns 3. The SDK echoed two text blocks of 84 and 12,223
  characters; the full second block was acknowledged, one mevedel Read ran,
  the mailbox emptied and the final answer included the mail-only marker.
  It completed in 6.34 seconds. Usage: input 14384, output 125, cached read
  17442, cache write 14380. This is not Pro/Max-account-specific validation.
  The isolated gptel source hashes/configuration remain as recorded above;
  no equivalence to the live editor is claimed.
- `initial-mail-compile.log`: all 226 files compile without warnings, followed
  by Eask bytecode cleanup. Updated current agent/module contracts and ADR 0115.
  No commit or editor reload. The complete implementation goal remains active.
- Next: native roster/reminder/observation delivery, acknowledgement and
  restoration across compaction; uncertain-effect reconciliation and verified
  restart recovery; remaining workload/capability/setup audit; real programming
  and remote acceptance; full suite, review and final commit. Initial mail does
  not establish permanent retention or complete the broader context contract.


### Native compaction restoration — 2026-10-06

- Previous goal turn was verified progress (initial and oversized mail delivery).
  Inspected the current worktree/spec and shared observation/reminder contracts.
  No blocking condition. The full implementation goal remains active.
- Source inspection found SDK 0.3.287 documents default system-prompt snapshotting:
  a later resume may ignore new prompt text until compaction. Claude launch now
  supplies the custom system prompt with `snapshot: false` through the adapter's
  allowlisted SDK options. The adapter's top-level object prompt path forcibly
  selects the Claude preset, so the custom object belongs in SDK options. Only
  that options field is now authoritative; removed the redundant top-level text.
- Root and retained-child drivers share baseline composition in
  `mevedel-claude-code-context.el`, reusing the normal context-delivery renderer
  and the recipient's selected components. Complete baseline instructions stay
  in native system input, including strings exceeding the hook limit.
- A generated `SessionStart` hook matching `compact` re-renders current selected
  observations and supplies differences from that launch baseline. An exact
  successful SDK hook receipt records the full restoration once. It does not
  change mailbox consumption. Subsequent tools and successful settlement require
  receipt; missing acknowledgement cannot be reported as a healthy continuation.
  The shared runner exposes an adapter validation callback rather than knowing
  Claude-specific state keys. Hook preparation errors settle the turn as errors.
- Large changed observations currently fail visibly before continuation when
  their total restoration output exceeds 10,000 UTF-16 units. A new turn supplies
  the full current baseline. This is an explicit incomplete requirement, not a
  completed automatic large-context solution. Ordinary in-turn observations,
  rosters and reminders still require integration at tool/request boundaries.
- TDD evidence: `context-red.log` fails the missing explicit snapshot policy;
  `context-workflow-red.log` fails absent native restoration. The final
  `context-final.log` passes 22 subscription/adapter workflow cases. Compaction
  scenarios cover changed and unchanged context, duplicate receipts, missing
  receipts before another tool or final answer, oversized changed context and
  a system baseline exceeding 12,000 characters. `context-regression.log` passes
  284 ACP, agent lifecycle, context-delivery, MCP, Buddy and memory-review cases.
- Live normal-session acceptance: `live-context-restore.el` and its log use the
  installed CLI 2.1.291, adapter 0.86.0 and existing Enterprise login through
  CLAUDE_CONFIG_DIR. Sonnet, low effort, no built-in tools, maxTurns 30,
  autocompact 100k, and actual custom snapshot=false options were asserted before
  dispatch. Native compaction occurred after 12 Read batches. The production
  compaction hook received changed memory containing a new marker, the SDK
  acknowledged the exact output, and 10 further reads completed. The final
  answer included SYSTEM-RESTORED-826491 from the large system prompt and
  MEMORY-RESTORED-937582 introduced only at the compaction boundary. Total:
  22 reads, 42.99 seconds, success; usage input 116512, output 2076, cached read
  839723, cache write 116466. This is a controlled context-retention fixture,
  not A16's real multi-file programming acceptance or a Pro/Max-account run.
- Current docs and ADR 0115 distinguish the implemented baseline/restoration
  path from remaining context work. The isolated gptel configuration and source
  hashes are unchanged from prior evidence; no live-editor equivalence claimed.
  `context-final-compile.log` compiles all 226 files without warnings, followed
  by Eask bytecode cleanup. No commit or editor reload.
- Outstanding scope remains intact: ordinary roster/reminder/observation
  delivery, oversized changed-context continuation, native lifecycle ownership
  cleanup, uncertain-effect reconciliation and restart recovery, remaining
  engine-workload/setup/capability/media audit, real programming and remote
  acceptance, final full suite, review and commit.


### Durable tool admission and interrupted recovery — 2026-10-06

- Previous goal turn was verified progress. Inspected current drivers, codec,
  shared resume/reconciliation and settlement contracts; refreshed upstream gptel
  (already current). No blocker; the full spec remains the objective.
- `mevedel-claude-code-history.el` owns durable admission of native tool IDs and
  names. Root and child records retain that conversation's admissions across
  turns. The shared ACP runner invokes admission before entering the existing
  tool pipeline; failed publication or a previously admitted ID prevents effects.
  The codec validates the ledger's pairs, nonempty strings and uniqueness.
  These records assert possible execution, never successful effects or receipt.
- An uncertain/in-flight prior history adds uniquely marked recovery guidance
  to the next submitted prompt. It reuses the native reconciliation body now
  owned by reminders, and explicitly forbids replay based on a missing result.
  Only its full SDK user-message echo records delivery; until then more tools
  and successful settlement are blocked. Root and child scopes remain separate.
  One echo can acknowledge both queued mail and recovery without duplication.
- `recovery-red.log` demonstrated the missing durable call identity. A real Bash
  append followed by deterministic peer death now persists the ID, saves/reopens,
  rejects a new call without recovery acknowledgement, and rejects the prior ID
  with acknowledgement. The file remains one line. A separate publication-failure
  case proves that a failed admission write prevents the Bash effect entirely.
  Retained-child tests now assert successful normal follow-up with distinct native
  IDs, recovery after interruption/death, and cross-turn duplicate rejection.
- The first bounded live recovery run (`live-effect-recovery.log`) found a real
  public-abort gap: generic abort released the request before native cancellation
  acknowledged, leaving in-flight history and skipping terminal accounting.
  `recovery-abort-red.log` reproduces it through `mevedel-abort`, replacing the
  weaker direct request-cancel test. The ACP runner now uses the existing nested
  settlement hold while its cancellation reply is pending, releasing it once
  terminal processing begins. `recovery-abort-green.log` passes all six cases.
- The repeated live run (`live-effect-recovery2.log`) passed in 7.70 seconds:
  one native Bash appended RECOVERY-5843, PostToolUse invoked public abort,
  save/close/reopen resumed the same native history, the SDK acknowledged the
  recovery notice, and one Read inspected the existing single line. Both turns
  settled and two durable native call IDs remained; no write was repeated.
  CLI 2.1.291, adapter 0.86.0, existing Enterprise login via CLAUDE_CONFIG_DIR,
  actual models claude-sonnet-5-5, low effort, maxTurns 3, built-ins empty.
  First-turn usage: input 2184, output 100, cached read 2904, cache write 2182;
  resumed usage: input 3898, output 140, cached read 9575, cache write 3894.
  This is same-editor reopen evidence, not a fresh Emacs restart or a Pro/Max
  account test. No credentials were read or copied and no editor was reloaded.
- Verification: `recovery-focused.log` 212/212 chat, reminders and subscription
  cases; `recovery-regression.log` 235/235 subscription, agent lifecycle, codec
  and ACP cases; after the abort fix `recovery-settlement.log` 197/197 chat,
  turn, Goal, preset and ACP cases. These sets overlap and are not a full-suite
  result. `recovery-compile.log` compiles all 227 files without warnings, then
  Eask cleans bytecode. `git diff --check` passes. Updated current session/agent
  contracts, module map and ADR 0115. No commit.
- Still required: fresh-Emacs root and retained-child restart/resume, remaining
  context/reminder/roster coverage and oversized changed-context continuation,
  provider-neutral lifecycle state migration, model-workload/setup/capability
  and media audit, remote and real programming acceptance, full suite, review
  and final commit. Receipt of recovery guidance is not proof that all prior
  external effects have been reconciled; capable models must inspect current
  state under the existing permission and tool contracts.


### Separate-editor root and retained-child resume — 2026-10-06

- Previous goal turn was verified progress. Inspected current persistence and
  registry contracts and the current spec. Added a deterministic acceptance test
  that launches two distinct batch Emacs processes from the isolated Eask runner.
  They share only published session/workspace files and the protocol peer, not
  session objects, buffers, registries, timers or subprocess handles.
- `test/test-mevedel-claude-code-restart.el` and its fixture create a root turn
  and retained child in phase one, then exit. Phase two restores the provider,
  resumes the separate root/child references, preserves transcripts and durable
  call IDs, and follows up the frozen Sonnet child after changing the parent's
  model. The fixture supplies distinct native session IDs. Opening the restored
  session dispatches nothing; a saved active Goal is checked to restore paused.
- The cold test exposed a real persistence dependency (`restart-run6.log`):
  the child was dropped because its frozen roster included an unloaded ToolCall
  reference. Warm tests had populated gptel's registry. The persistence decoder
  now asks the owning mevedel registrar to initialize a missing built-in before
  resolving its exact category/name. Unknown tools and foreign-category names
  still fail as invalid persisted data. No substituted schema or compatibility
  reader was introduced. Consulted gptel's exact-path lookup and the existing
  mevedel registrar owner.
- `restart-green.log` passes 34 persistence/restart cases. After strengthening
  unknown-tool coverage, `restart-regression.log` passes all 102 ACP, subscription,
  retained-conversation and persistence cases. `restart-final.log` additionally
  passes the separate-process Goal-paused assertion. The fixture's initial
  setup failures were missing test-only dependency imports, resolved before
  diagnosing the actual child-restore failure.
- Live counterpart (`live-restart-test.el`, `live-restart/claude-restart.el`,
  `live-restart.log`) passed in 17.78 seconds with actual Emacs PIDs 1799825 and
  1800926. Phase one created two different native histories; phase two resumed
  exactly those IDs. Each phase completed one Read in root and one in the child;
  both ledgers grew from one native call to two, and both returned ready.
  Configured Sonnet/low effort, maxTurns 3 per call, empty built-ins and supported
  CLAUDE_CONFIG_DIR login were checked. Existing CLI 2.1.291 and adapter 0.86.0,
  Enterprise account as before; no Pro/Max-account-specific claim. The actual
  SDK model-init and per-call usage were not captured in this restart fixture.
  This verifies real same-machine editor restart, unlike prior same-editor
  reopen checks, and does not satisfy the separate real-programming acceptance.
- Current session/agent docs and ADR 0063 now describe lazy built-in resolution
  and external root/child restart behavior. `restart-compile.log` compiles all
  227 files without warnings; Eask bytecode cleanup follows. `git diff --check`
  passes. No editor reload, commit or full-suite claim.
- The goal remains active. Remaining requirements include ordinary context,
  roster/reminder delivery and oversized changed-context continuation; complete
  provider-neutral lifecycle/workload coverage; child permission while parent
  Wait is active; setup/capability/media interfaces; broader recovery/missing
  history verification; remote and real programming acceptance; full suite,
  review and final commit. Preserve the full spec during the next work slice.


### Ordinary observation and turn-event delivery — 2026-10-06

- Continued in the existing feature worktree; the approved workflow seams and
  complete implementation Goal remain unchanged. Selected observations now
  update at ordinary PostToolBatch boundaries, using the existing renderer and
  exact receipt path. The accepted snapshot advances only on receipt. Unchanged
  sections stay omitted, returning to the baseline is an update, and native
  compaction restores current differences even if accepted before compaction.
- The same envelope carries shared turn events, including deeper AGENTS.md
  instructions queued by Read. Receipt runs captured commits; missing receipts
  block subsequent effects and successful settlement. Shared event consumption
  now removes only captured objects and preserves replacements queued during
  delivery. Oversized required context fails visibly; no preview is accepted.
  Unacknowledged mail cannot silently obstruct later required-context updates.
- Red/green workflow cases cover root changes, unchanged/reverted state,
  compaction after ordinary delivery, malformed/missing receipts, missing final
  receipt, UTF-16 overflow, pending mail, child selected-context isolation,
  path-scoped guidance and late same-key replacement. `observations-red.log`,
  `turn-events-red.log` and `pending-context-red.log` demonstrate the missing
  behavior. The shared-queue regression initially caught empty-queue shape;
  fixed while preserving later events. Final `context-delivery-final.log`
  passes 289/289 subscription, ACP-turn, reminder and Read cases. These are
  focused regressions, not the full suite.
- Bounded live `live-context-updates.el` / `live-context-updates.log` passed in
  8.34 seconds: one real Read, changed memory plus discovered path instructions
  in one exact successful PostToolBatch receipt, and both markers in the final
  answer. Actual model `claude-sonnet-5-5`; configured Sonnet/low, maxTurns 3,
  built-ins empty, custom system snapshot=false. CLI 2.1.291, adapter 0.86.0,
  supported existing Enterprise login via CLAUDE_CONFIG_DIR. Usage: normalized
  input 1158, output 354, cached read 3939, cache write 1154. This is neither a
  Pro/Max-account-specific test nor A16 programming acceptance. Loaded isolated
  gptel source hashes still match the frozen runner recorded above. No user
  editor reload, credential copying or paid-overflow configuration.
- Updated architecture/reminder contracts, module map and ADR 0115.
  `context-delivery-compile.log` compiles all 227 files without warnings; Eask
  cleanup removes bytecode afterward. `git diff --check` passes. No commit.
- Next context work: retained-child roster and configured reminder delivery,
  native-compaction handling for path-scoped turn events/instruction hashes,
  and oversized changed-context continuation. Initial system-baseline receipt
  evidence remains distinct from these hook acknowledgments. The other full
  spec requirements listed above remain outstanding; do not mark complete.


### Shared rosters and configured reminders — 2026-10-06

- Previous goal turn was verified progress. Refreshed upstream gptel (already
  current), consulted its injection contract and current reminder/agent/hook
  owners, and continued in the existing feature worktree without changing scope.
- Extracted the direct-child roster producer from its gptel WAIT handler.
  Both engines use that producer and its deferred commit. Claude delivers the
  initial roster with the prompt, new children at tool-batch boundaries, and the
  complete roster after native compaction. Missing initial receipt prevents
  further tools. Existing gptel direct-child/grandchild and delta tests pass.
- One shared reminder collector now serves gptel transforms and Claude prompt
  preparation, using the session or invocation's existing firing policy and
  counter. Exact SDK receipt commits firing marks, silent commits, pending
  events and pending root hook context. Large initial reminders are complete
  prompt blocks, not hook previews. Child configuration remains isolated from
  parent reminders. Local context-pressure estimates are suppressed for external
  history instead of making unsupported claims about Claude's window.
- Pending-event commits remove only captured items; newly appended events stay
  pending. Hook-context consumption reuses its existing prefix transaction and
  likewise preserves later arrivals. Root recovery uses this same pending-event
  owner: the existing recovery workflow caught two copies after reminder wiring,
  and now again records exactly one. Retained-child recovery remains scoped to
  that child. Native compaction also restores active root Plan guidance through
  its existing reminder producer.
- Added normal workflow tests in `test/test-mevedel-claude-code-context.el`.
  Red evidence: `roster-red.log`, `roster-compact-red.log`, `reminders-red.log`,
  `reminders-compact-red.log`, `reminders-recovery-red.log` and corrected
  `reminder-hooks-red2.log`. They cover missing roster/reminders, lost Plan
  guidance after compaction, duplicate recovery, and pending hook context.
  Coverage includes exact/absent receipts, root/child isolation, large initial
  guidance, silent commits and late pending items. The hook fixture was corrected
  to use the runner's list-valued additional-context contract before red/green
  verification; its earlier malformed value was not evidence of production behavior.
- Final `roster-reminders-final.log` passes 444/444 subscription/context, restart,
  ACP-turn, reminder, tool, retained-agent, hook, compaction and context-delivery
  regressions. `roster-reminders-compile-final.log` compiles all 227 files without
  warnings after fixing an unused-name convention warning; Eask cleans bytecode
  afterward. `git diff --check` passes. Updated architecture, agents, reminders,
  module map and ADR 0115. No new live call, full-suite claim, editor reload or
  commit in this slice.
- The full Goal remains active. Context work still includes native-compaction
  restoration for path-scoped instruction events/hashes and accepted-plan
  references, automatic oversized-update continuation, and initial baseline
  receipt evidence. Native child sample limits/counting, broader shared lifecycle
  and workload coverage, setup/capability/media interfaces, missing-history
  recovery, remote and real programming acceptance, full suite, review and final
  commit remain required. No blocking condition prevents independent progress.


### Scoped instruction and accepted-plan restoration — 2026-10-06

- Continued the complete Goal in the feature worktree. Native compaction now
  re-reads the path instructions already learned by its conversation, restores
  broad scopes before deeper/local overrides, and acknowledges current hashes
  only on exact successful receipt. Deleted instruction files explicitly
  withdraw old guidance. Missing receipt blocks subsequent tools and successful
  settlement. Oversized instructions stop visibly and can be delivered in full
  by the next prompt; automatic continuation remains outstanding.
- Retained children restore only their own learned scopes. A normal directive
  workflow test exposed the root's hash cache suppressing a fresh directive's
  guidance. Directive requests now own transient hashes; neither root nor
  sibling scope leaks into their restoration. Root/child persisted hash format
  remains the same; the new request field is transient.
- Accepted-plan references restore through the existing reminder producer,
  retaining its eligibility and active-Goal deduplication rules. A normal
  send/compact/continue test proves the reference is restored and that a missing
  receipt blocks the next Read. Its setup uses published session artifacts;
  direct cache writes were not valid evidence in a portable session.
- Red/green evidence: `scoped-context-red.log`, `scoped-retry-red.log`,
  `directive-instructions-red.log`, `plan-restoration-red.log`. Context tests
  cover changed/deleted guidance, ordering, root/child/directive isolation,
  missing acknowledgment and full-prompt retry after oversized restoration.
  `directive-instructions-green.log` passes 128/128 context and Read cases.
  `scoped-context-final.log` passes 526/526 broader regressions before the
  accepted-plan addition; `plan-restoration-green.log` passes 139/139 context and
  reminder cases afterward. These are focused regressions, not a full suite.
- Bounded live `live-scoped-context.el` / `live-scoped-context.log` passed in
  59.315 seconds: 22 real Reads, native compaction after batch 12, exact successful
  restoration receipt containing revised path instructions plus current memory,
  all remaining tools completed, and the new path marker in the final answer.
  Actual model `claude-sonnet-5-5`; configured Sonnet/low, maxTurns 30, built-ins
  empty, custom system snapshot=false. CLI 2.1.291, adapter 0.86.0, supported
  existing Enterprise login via CLAUDE_CONFIG_DIR. Usage: normalized input
  117200, output 2452, cached read 845860, cache write 117154. This synthetic Read
  chain is neither A16 programming acceptance nor Pro/Max-account-specific
  evidence. No credential copying, overflow configuration or editor reload.
- Updated architecture, reminders, agents and ADR 0115 with current behavior and
  evidence. `scoped-context-compile.log` compiles all 227 files without warnings;
  Eask cleanup removes bytecode afterward. No commit; full Goal remains active.
- Extended the separate-editor restart workflow to learn a deeper instruction
  file in both root and child, change it between processes, and then read only
  an unrelated root-level file. `scoped-restart-red.log` failed in phase 2 with
  no updated guidance: local SessionStart discarded the learned scopes.
  Native-history owners now retain their hashes across that local epoch and
  refresh complete current contents on each resumed prompt before tools. Local
  gptel epoch resets remain unchanged. `scoped-restart-green.log` passes 89/89
  restart, chat, context and subscription-session regressions. Updated session
  persistence documentation as well as the context contracts. Final
  `scoped-restart-compile.log` again compiles 227 files without warnings; Eask
  cleanup follows, and `git diff --check` passes.
- Next context audit: directive selection of session-wide configured reminders,
  automatic oversized-update continuation,
  and initial system-baseline receipt evidence. Remaining full-spec work includes
  native child sample limits, shared lifecycle/workload coverage, guided setup,
  capability/media interfaces, missing-history recovery, remote and actual
  programming acceptance, full suite, review and final commit. No blocker.


### Retained Claude agent sample limits — 2026-10-06

- Previous goal turn was verified progress. Revalidated the feature worktree,
  existing context/restart evidence, agent runtime and reminder contracts, and
  refreshed upstream gptel (already current). The complete Goal remains active.
- Native children now count sample one at prompt preparation and subsequent
  samples at completed tool-batch boundaries. Multiple tools in a batch count
  once. A configured cap stages final-sample guidance, allows that sample's
  tools to settle, then ends the native loop through the existing boundary
  mechanism. The shared child callback appends its incomplete-result note.
  Follow-ups get a fresh counter; normal answers before the cap have no note.
- The native engine reuses the invocation's one-shot warning producer and
  existing acknowledged turn-event queue. A shared producer now renders current
  cap guidance for gptel's last sample and native compaction restoration. Exact
  receipt commits warning firing marks. No new agent lifecycle controller or
  fabricated provider FSM was introduced.
- Source decision: [Claude's loop documentation](https://code.claude.com/docs/en/agent-sdk/agent-loop)
  defines SDK maxTurns in tool-use rounds, while mevedel's existing cap counts
  final text-only samples too. [PostToolBatch](https://code.claude.com/docs/en/hooks#posttoolbatch)
  runs once after the full batch settles and can stop before the next model
  request. The integration therefore preserves mevedel's sample semantics with
  that boundary instead of mapping its cap to SDK maxTurns. Internal provider
  retries are not counted as new mevedel samples. Inspected adapter 0.86.0 source
  as well; the tested production path uses no native maxTurns substitution.
- `agent-limits-red.log` exposed the absent counter; a subsequent normal
  spawn/follow-up test passed one-sample, three-sample and unlimited cases.
  `agent-limit-compact-red.log` then exposed lost final-sample guidance after
  native compaction. The test now verifies restoration, two tools per sample,
  independent follow-up counts, boundary notes, and an eight-sample cap where
  the child finishes normally after six samples. A test expectation was corrected
  to use the existing record outcome `completed`, rather than engine `success`;
  test cleanup was also fixed before final verification.
- `agent-limits-regression.log` passes 250/250 subscription, native context,
  restart, ACP-turn, agent runtime/execution and reminder cases. After adding
  the normal-before-cap case, `agent-limits-final.log` passes 180/180 focused
  cases. `agent-limits-compile.log` compiles all 227 files without warnings;
  Eask cleanup follows. `git diff --check` passes. No full-suite claim or commit.
- Bounded live `live-agent-limit.el` / `live-agent-limit.log` passed in 2.797
  seconds: one real retained child, one Read, one model sample, one PostToolBatch
  boundary, completed settlement and the explicit 1-turn-limit stop note. Actual
  model `claude-sonnet-5-5`; configured Sonnet/low, built-ins empty, custom system
  snapshot=false. The disposable live fixture had an SDK maxTurns=3 backstop;
  mevedel stopped after sample one. Usage: normalized input 3980, output 86,
  cached read 0, cache write 3978. CLI 2.1.291 / adapter 0.86.0, existing supported
  Enterprise login via CLAUDE_CONFIG_DIR; no Pro/Max-account-specific claim,
  credential copying, overflow setting, editor reload or A16 acceptance claim.
  Isolated gptel source hashes still match the frozen runner recorded above.
- Full Goal work remains: complete shared lifecycle/workload and directive
  context audit; automatic oversized-context continuation and initial baseline
  receipt evidence; guided setup and capability/media interfaces; broader
  missing-history recovery; child interactions while parents wait; remote and
  actual multi-file programming acceptance; full suite, final review and commit.
  No repeated blocker prevents autonomous progress.


### Guided subscription setup and managed installation — 2026-10-06

- Previous goal turn was verified progress. Revalidated the worktree, A01 and
  the installation requirements, launch owner, package recipe, dependency sources
  and provider selection. Refreshed upstream gptel (already current). The full
  Goal remains active; this slice does not replace its acceptance requirements.
- Added `M-x mevedel-claude-code-setup`: it runs the real launch preflight without
  a model call, reports readiness or an actionable error, links the official CLI
  installation instructions, explains supported login and ordinary model
  selection, and provides refresh and explicit adapter-install actions.
- `mevedel-claude-code-install-adapter` asks before installing the pinned
  `@agentclientprotocol/claude-agent-acp@0.86.0` and dependencies into the configured
  local directory. It passes separate argv values to npm, runs asynchronously,
  records output and terminal status in a compilation buffer, rejects concurrent
  installs, and respects custom-adapter overrides. Declining does not install.
  Ordinary turns never download dependencies. A live installation used a
  directory containing spaces and the normal managed-path lookup.
- Launch and setup now check the actual Emacs ACP version (>=0.15.2), Node
  (>=22), Python (>=3.8), CLI, adapter, supported subscription auth status and
  packaged bridge. The shared MCP configuration also rejects a missing bridge.
  Failed executable checks identify the actual command without forwarding raw
  diagnostics. Setup refresh preserves the registered provider object so open
  sessions keep their existing backend identity.
- Fixed the README's straight.el recipe to include
  `("scripts" "scripts/mevedel-mcp-stdio.py")` and documented the guided route,
  local prerequisites, managed/custom installation choices and stable history
  directory. Updated dependency, module, session and documentation-map entries.
  The [official CLI setup](https://code.claude.com/docs/en/setup) and installed
  adapter package metadata informed the prerequisite instructions; credentials
  remain owned by the installed CLI.
- Red evidence: `setup-red.log`, `setup-dependencies-red.log`,
  `setup-install-red.log`, `setup-bridge-red.log`, `setup-diagnostics-red.log`,
  `setup-refresh-red.log`. Tests use real temporary executable fixtures and an
  actual asynchronous installation process. They cover readiness/API-auth
  rejection, outdated runtime/client, missing packaged script, user confirmation,
  decline, success/failure, concurrent install, path quoting and provider identity.
  `setup-final.log` passes 123/123 subscription/ACP/MCP/model cases; after the
  refresh fix, `setup-refresh-green.log` passes 59/59 setup and model cases.
  `setup-compile.log` compiles 227 files without warnings. Eask cleanup follows;
  `git diff --check` passes. No full-suite claim, editor reload or commit.
- Bounded real `live-setup.el` / `live-setup.log` passed in 6.315 seconds. In a
  disposable session it ran the public install command against real npm,
  verified SDK 0.3.287, refreshed readiness, selected the normal Claude Code
  provider and completed one Read through the newly managed adapter. Actual
  model `claude-sonnet-5-5`, Sonnet/low, maxTurns=3 in the disposable live guard,
  built-ins empty, system snapshot=false. CLI 2.1.291, adapter 0.86.0, ACP 0.15.2;
  the existing supported Enterprise login was reached only through
  CLAUDE_CONFIG_DIR and supported CLI status/agent interfaces. Usage: normalized
  input 2256, output 113, cached read 5318, cache write 2252. The isolated gptel
  source hashes remain the same frozen copies recorded above. No Pro/Max-account
  specific claim, credential copying, paid-overflow change or A16 acceptance
  claim. Installation and session files were removed by fixture cleanup.
- Remaining full-spec work includes capability/menu and media input coverage;
  shared lifecycle/workload and directive context audit; automatic oversized
  context continuation and initial baseline receipt evidence; broader missing
  history recovery; child interactions while the parent waits; real remote and
  multi-file programming acceptance; full suite, final review and commit. No
  blocking condition prevents the next independent slice.

## Native attachment input (2026-10-06)

- Root/directive and retained-child submissions now reuse the shared structured
  mention resolver, including permission checks and authoritative artifact bytes.
  Its temporary buffer explicitly retains the selected model. Native models
  advertise PNG/JPEG/GIF/WebP MIME support. Accepted images and selected gptel
  media contexts become ACP image blocks, with duplicate paths sent once per
  submission. The generic ACP transport checks negotiated image capability
  before dispatch. PDF/audio input is not advertised: adapter 0.86.0's
  `promptToClaude` converts images, but does not convert blob resources to native
  document blocks. No silent PDF transport claim.
- Exact SDK user-message receipts must match all submitted text plus image order,
  MIME and complete base64 bytes before mention dedup commits. Missing or changed
  image echoes prevent successful settlement and subsequent effects through the
  existing context guard. Canonical transcripts retain references/reminders,
  without base64 blobs. Fresh directives and child tasks/follow-ups expand
  independently of root deduplication. This slice does not complete the remaining
  shared-transform/context audit.
- Focused ordinary workflow checks cover composer attachments, selected context,
  duplicate context-plus-mention paths, missing/altered image receipts, unchanged
  next composer draft, fresh directive discussion, retained-child spawn and
  follow-up, and parent dedup isolation. ACP contract checks cover unsupported
  image input and continued usability for a later text prompt. Red evidence:
  `media-input-red.log`, `media-input-green.log` (selected model lost in temporary
  expansion buffer), `media-capability-red.log`, `media-child-red.log`.
- `media-regression.log`: 155/155 mention, root/directive, retained-child,
  ACP and MCP tests passed. `media-scopes2.log`: all three scope cases passed
  (directive fixture now installs isolated presets). `media-final.log`: 137/137
  context, ACP turn/text, queue/steering, tools and attachment cases passed after
  Eask bytecode cleanup. `media-compile2.log`: 227 files compiled without warnings;
  the first compile caught and prompted correction of one overlong docstring.
  `git diff --check` passes. No full-suite, editor reload or commit claim.
- Bounded real `.scratch/claude-code-engine/live-image.el` /
  `live-image2.log` passed in 2.745 seconds. A generated 96x64 red PNG passed
  through normal selected-provider dispatch; Claude identified its color and
  the complete native image echo committed the attachment. Actual model
  `claude-sonnet-5-5`, Sonnet/low, maxTurns=1 in the disposable guard, built-ins
  empty, system snapshot=false. CLI 2.1.291, adapter 0.86.0, SDK 0.3.287,
  ACP 0.15.2, same isolated gptel sources as previously recorded. Supported
  Enterprise login through CLAUDE_CONFIG_DIR and the installed CLI; no Pro/Max
  account-specific claim. Normalized input 2187, output 4, cached read 0, cache
  write 2185. The first disposable script failed local adapter discovery before
  a model call because it resolved the path relative to the temporary session;
  the corrected script uses the actual installed adapter path. Temporary image
  and session state were cleaned up. This is not A16 programming acceptance.
- Remaining full-spec work: shared lifecycle/prompt/workload audit and capability
  menus; automatic oversized context continuation and initial baseline receipt
  evidence; missing-history recovery; child interactions while parent waits;
  real remote and 20+ tool multi-file programming acceptance; final maintained
  docs/ADR/glossary audit, full suite, review and commit. Goal remains active.

## Root request policy before engine dispatch (2026-10-06)

- Auditing the registered gptel transforms found that native root dispatch used
  the session model directly, bypassing Plan's planning workload and leading
  skill model/effort policy. Composer and generated-turn sends now share
  `mevedel--dispatch-request`, which resolves policy before choosing gptel or
  Claude. The composer retains its native gptel-send path and startup fencing;
  generated turns retain their native gptel-request path. Saved session provider
  and effort are unchanged. Directives retain their explicit dispatch policy.
- The ordinary composer test first reproduced Haiku being launched instead of
  the configured Plan Sonnet/high (`policy-red.log`). It now covers Plan
  Sonnet/high, a prepared leading skill selecting Opus/xhigh, and a Plan workload
  selecting Claude when the saved session provider is an API backend. A later
  attempt to send that native root history through the saved API provider is
  rejected before its send callback, with the explicit cross-engine error.
- A second failing check (`policy-context-red.log`) showed the function-valued
  system prompt recomputing Plan's model after the pending skill override was
  drained. Request admission now captures effective backend/model/effort on
  `mevedel-request-model-policy` before draining skill context; shared policy
  consumers use that frozen value. Native deferred attachment preparation binds
  the captured model/effort as well. This makes roster sizing use the actual
  request model, and avoids mutating session selection during asynchronous work.
- `policy-regression2.log`: 452 selected cases, 451 passed, zero unexpected,
  one existing skip (`mevedel-view--fontify-composer/test@2`, no Markdown
  fontification mode). Covers composer, chat, presets, skill invocation,
  native root/directive and images. The initial regression found changed
  whitespace from falling back to transcript extraction; the composer now
  passes its exact selected input to the shared dispatch boundary.
  `policy-guard3.log` passes all three selection scenarios and the direct
  no-API-send guard. `policy-compile.log`: 227 files compiled without warnings,
  followed by Eask cleanup. Architecture and skill contracts describe the
  implemented policy capture. No live model call, full-suite, editor reload
  or commit in this slice.
- Audit findings to carry forward: paired composer skills already produce
  complete model input before engine dispatch, so the raw-gptel attachment
  transform is not their missing delivery path. In contrast, native context
  input collects selected image contexts but not gptel's selected text
  buffers/files; the standard text context and asynchronous formatter path
  still need integration. Upstream gptel was refreshed (already current) and
  its context collection/wrapping and policy-transform ordering inspected.
  Exact Goal-budget controls and menus also still require verification; earlier
  summaries mentioning restrictions are not proof of a native budget guard.
  All other remaining full-spec gates listed above remain active.
- Final focused `policy-final2.log` passes 58/58 native context, retained-agent,
  model-policy and composer selection cases, with no unexpected diagnostics.
  The cross-engine check asserts the public user-error reason (not a transcript
  summary that is not promised on this rejection path). `git diff --check`
  passes. All test processes from this slice are terminal.

## Selected text context and owned preparation (2026-10-06)

- Native root/directive and retained-agent startup now collects selected gptel
  text files and buffer regions as well as media. It reuses
  `gptel-context--collect`, `gptel-context-string-function` and media collection;
  explicit text MIME sources are not treated as image attachments. System/user
  placement is preserved. Region tests prove surrounding private buffer text
  is excluded. Empty collected sources do not fall back to the unfiltered media
  context list.
- `mevedel-acp-turn-start` owns an optional asynchronous preparation continuation
  before MCP/ACP startup. Cancellation and ownership checks apply to that wait;
  duplicate callbacks start at most one connection, and callbacks arriving
  after interruption cannot launch. Malformed formatter output fails before
  launch. Root system-prompt functions run before the wait, within the captured
  effective model/skill policy; changes to editor tool settings while formatting
  no longer alter that submitted system prompt. Child system configuration stays
  frozen on its invocation.
- User-placed selected text requires its complete native SDK prompt echo. Native
  compaction restores the formatted turn snapshot through the existing exact
  SessionStart receipt before subsequent tools. System-placed text is in the
  native system baseline. Oversized restoration still fails explicitly; automatic
  oversized-update continuation remains a required unfinished feature.
- New ordinary workflow cases exercise file text, explicit text MIME, selected
  buffer regions, both placements, async callbacks in another buffer, duplicate
  callback suppression, public abort and unchanged multiline composer draft,
  malformed output, frozen authored system prompt, and compaction restoration
  before a real Read. Missing restoration receipt blocks Read. Red evidence:
  `selected-context-red.log`, `selected-context-async-red.log`,
  `selected-context-restoration-red2.log`, `selected-context-frozen-red.log`.
  The earlier restoration log records a test syntax error, not behavioral proof.
- `selected-context-regression.log`: 45/45 cases passed before the final
  region/malformed-output/frozen-prompt additions. Final
  `selected-context-final-regression.log`: 61/61 native root/directive/agent,
  context, image, policy, fresh-Emacs restart, ACP and MCP cases passed without
  unexpected diagnostics. `selected-context-compile.log`: 227 files compiled
  without warnings; Eask bytecode cleanup followed. `git diff --check` passes.
  No full-suite, editor reload or commit claim.
- Bounded live `live-selected-context.el` / `live-selected-context.log` passed
  two turns in 5.459 seconds. Each used an asynchronous gptel formatter and a
  marker supplied only by the selected text file; the model returned the marker
  in both system and user placement. Actual model `claude-sonnet-5-5`, Sonnet/low,
  maxTurns=1 guard, built-ins empty, system snapshot=false. Same installed CLI
  2.1.291, adapter 0.86.0, SDK 0.3.287 and ACP 0.15.2 as previous checks; supported
  Enterprise login through CLAUDE_CONFIG_DIR, no credential access or Pro/Max
  account-specific claim. System placement usage: normalized input 2120, output
  13, cache read 0, cache write 2118. User placement: input 2104, output 13,
  cache read 0, cache write 2102. User placement passed the exact prompt-receipt
  guard. The live log's `:receipt t` for system placement only means no input
  receipt was pending; the returned marker proves model use, not an exact SDK
  system-prompt receipt. These are not A16 programming acceptance.
- **Next reproduced defect:** aborting the first native turn during preparation
  leaves a completed local turn but no external root identity. The next native
  send is incorrectly rejected as cross-engine transfer. Disposable public-send
  reproduction `first-preparation-abort-retry.el` / `.log` fails with
  `Cross-engine history transfer is unavailable; start a new session`. Fix the
  distinction between attempted native startup and actual gptel root history,
  including persistence/close-reopen semantics; do not weaken the existing real
  cross-engine guard. Other full-spec gaps listed above remain. No blocker.

## Review scope, startup retry and model catalog (2026-10-06)

- Incorporated the user-authorized review handoff into the existing PRD; all
  four sections are required outcomes of the active goal. Sections 7, 9 and 11
  now require bidirectional cross-engine continuation, native compaction mapped
  to transcript segments, live model/effort discovery and retained-history model
  switching. Added A17-A21; existing A01-A16 and completed work remain required.
  Cross-engine continuation is no longer an allowed blanket limitation. The
  guards still exist in code and must be replaced by the requested transfers.
- Verified the technical observations against pinned adapter source 0724b17
  and the installed adapter. ACP session initialization exposes `configOptions`
  model choices, derived internally from `availableModels`; the actual response
  has no top-level `models`. Effort options describe the selected model only.
  Compaction capability/events exist in the adapter, but are not yet advertised
  or consumed by mevedel. Legacy synthetic tool notifications are observations
  only: ACP rendering accepts message/thought chunks, and execution uses MCP.
- Fixed the preceding startup-retry defect. Root admission records
  `(:engine claude-code :state unstarted)` before asynchronous preparation.
  It carries no ID, installation or tool ledger; real native readiness replaces
  it before prompt dispatch. Abort/startup failure can settle and persist it;
  retry, including save/reopen, creates a fresh native conversation. A late
  preparation callback cannot affect the replacement request. Codec v0.5.8
  validates this variant and rejects fake identities/effects in it. Old sidecars
  remain rejected without migration. The real cross-engine guard is unchanged
  pending the required transfer implementation.
- The tracked public-send regression covers preparation abort and launch error,
  both immediate retry and save/reopen. Its first version needed to wait for
  normal async settlement before testing retry. With that corrected, the
  disposable `startup-retry-red-driver.el` removes only the ownership fix and
  reproduces the cross-engine error (`startup-retry-confirmed-red.log`). The
  fixed suite passes 26/26 (`startup-retry-green2.log`), then 31/31 selected
  context, policy and fresh-Emacs restart cases (`startup-retry-regression.log`).
- Model discovery now runs through an optional ACP launch `:check-session`
  callback before readiness; failure closes startup before a prompt can run.
  Claude uses current `configOptions` to refresh its in-memory provider catalog,
  keep all three latest aliases, add reported IDs and learn the current model's
  effort choices. Unknown configured IDs/effort can be staged before discovery,
  but unavailable IDs, unsupported effort, model substitution and effort
  substitution fail before dispatch. Native models own uninterned symbols so
  discovery does not overwrite API model metadata. No fixed effort tables remain.
  Discovery does not sample a model; setup's installation check remains local
  preflight and catalog refresh occurs at ACP session start.
- Catalog test red/green evidence: `catalog-red.log` fails the missing picker
  choice; `catalog-green.log` passes 17/17 catalog and ACP lifecycle cases.
  `catalog-regression.log` passes 88/88 model/session/policy cases. The supported
  but substituted effort case first failed (`catalog-effort-red.log`), then
  passed with the additional check (`catalog-effort-green.log`, 68/68). Unknown
  IDs, different reported model and unsupported/no effort are covered through
  isolated text dispatch as well. The broader native/image/context/agent/cold
  resume/codec run passed 125/125 (`catalog-final-regression.log`) before the
  final effort-substitution guard; targeted validation afterward passed.
- A21 live evidence: `live-model-switch.el` / `.log` used ordinary provider
  selection and two root submissions, Sonnet then Opus, retaining the same
  native ID. Actual models were `claude-sonnet-5-5` and `claude-opus-5-5`; Opus
  returned a marker supplied only in the first prompt, proving retained context.
  Two turns finished in 5.535 seconds. Both used low effort, maxTurns=1,
  tools=[], custom system snapshot=false. Usage: Sonnet normalized input 2083,
  output 5, cache read 0/write 2081; Opus input 2179, output 51, cache read 0/
  write 2177. This was before the catalog implementation; the later no-prompt
  discovery check separately validated the new startup path.
- `live-model-catalog.el` connects and initializes without sending any prompt.
  The final low-effort check passed in 0.662 seconds and populated the normal
  picker with reported IDs plus aliases, including Sonnet's reported maximum
  effort (`live-model-catalog-effort.log`). Earlier default-effort discovery
  also passed. Installed CLI 2.1.291, adapter 0.86.0, SDK 0.3.287, ACP 0.15.2;
  supported Enterprise account via its own CLAUDE_CONFIG_DIR. No credentials
  accessed and no Pro/Max-account-specific claim. These are not A16 acceptance.
  The isolated gptel source hashes still match the preceding recorded fixture;
  refreshed upstream gptel was already current.
- Initial compilation reported one overlong docstring, now wrapped. Final
  `catalog-compile-final.log`: all 227 files compile with zero warnings. Eask
  cleanup followed. `git diff --check` passes. No full-suite, editor reload, final review
  or commit claim. No blocker.
- Next: implement native compaction lifecycle/summary segment publication (A19)
  and use that effective history for both engine-transfer directions (A17/A18).
  Account for mid-turn boundaries, running executions, stream markers, duplicate
  terminal frames and context acknowledgements. The older provider setter also
  compares saved selection rather than actual root ownership after a workload
  override; resolve it with transfer routing. A20 needs final normal-picker and
  explicit invalid default/preset/workload/directive workflow audit; the common
  startup validation is in place. Other full-spec gaps remain: oversized-context
  automatic continuation, capability/Goal-budget audit, missing-history summary
  recovery, nested/waiting child interactions, real remote/A16 programming
  acceptance, full suite/docs/ADRs/review/commit. The goal remains active.


## Native compaction segments (2026-10-06)

- Root and retained-child ACP sessions now advertise session compaction support.
  Completed summaries rotate ordinary root segments or the child's private
  archive, preserving its task anchor. Output arriving after compaction starts
  stays in the live tail. The boundary uses gptel's tracking marker; point-max
  lost streamed tail text in the first regression. Stream markers reset after
  publication, and an active multiline composer draft survives the redraw.
- Terminal summary text supersedes streamed chunks. Duplicate terminal updates
  cannot rotate twice. Failed/cancelled compaction keeps history; completion
  without a summary fails visibly. Directive and isolated text conversations do
  not advertise root segment operations. Native context receipt remains separate.
- ACP updates, MCP tool admission, hook decisions and terminal outcomes now use
  one owned FIFO deferred until the execution target is idle. The busy-target
  regression initially failed during publication. Hooks now complete
  asynchronously through the private MCP control boundary. Cancellation fences
  pending operations; late replies cannot publish a new segment or execute tools.
- A real running Bash command exposed an existing shared archive error: the
  classifier relied on a renderer-only live flag. It now uses canonical queued,
  running and stopping lifecycle states. Terminal delivery replaces its archive
  after the root turn settles. The final fixture also installs both production
  execution subscribers (transcript and mailbox); missing the latter suppressed
  delivery in the test. No production delivery workaround was added.
- Deterministic checks cover root/child compaction, cancelled/failed/empty and
  duplicate events, real MCP Read batches, save/reopen/resume, view draft,
  transport busy/abort and a live Bash execution completing after compaction.
  segment-regression.log passed 120/120 before the final execution classifier
  correction. segment-execution-green3.log passes 58/58 including that correction
  and ordinary compaction/archive tests. segment-compile.log compiled 228 files
  without warnings before the final classifier and provenance-label changes;
  compilation will be rerun. No full-suite or final-review claim.
- Live proof: live-segment-compaction.el/.log completed 22 actual chained Read
  calls in 53.309 seconds using claude-sonnet-5-5, low effort, maxTurns30 and
  supported 100k automatic compaction. Compaction occurred after batch12:
  two segments, ten tools in the current segment, all22 admissions retained.
  The archived segment has prior tools, and the current segment has Claude's
  retained summary. The real compact hook restored changed memory and path
  instructions with the exact receipt before later reads; the answer included
  the system and new-context markers. Usage: input118607/output2171,
  cache-read842514/cache-write118561. Own supported Enterprise login, CLI2.1.291,
  adapter0.86.0, SDK0.3.287, ACP0.15.2. No credentials accessed, no paid-provider
  fallback, no Pro/Max-specific account claim. This synthetic Read chain is not
  the required A16 programming acceptance. Script assertion now matches the
  backend-derived summary label; no extra live call was needed for that label.
- Updated current compaction/tool contracts, module map and ADR0115. Next:
  bidirectional cross-engine continuation using effective segments (A17/A18),
  then remaining acceptance gaps listed above. Goal remains active.


## Cross-engine continuation (2026-10-06)

- Removed the blanket provider-selection and root-send refusals. Selection is
  non-destructive; dispatch follows the resolved request policy. A Claude
  planning workload can return to its saved API provider through the ordinary
  composer, without comparing the saved selection to historical ownership.
- Claude-to-gptel detaches/persists the root native reference before dispatch.
  If publication fails, the reference is restored in memory. The actual gptel
  Anthropic request serializer uses only the effective segment: retained
  summary, ordinary assistant output and real MCP tool calls/results. Prior
  archived text stays out, and unsigned reasoning produces no signed thinking.
  This uses existing transcript serialization rather than another wire format.
- Gptel-to-Claude uses the existing neutral evidence projector on the current
  segment before the submitted prompt. Independent directive/item ranges are
  excluded. The seed is explicitly an excerpt continuation into a new native
  conversation; historical effects must not be replayed and shortened tool
  outputs are marked. It is sent separately from mention/media expansion of
  the current prompt. Later native turns resume the replacement ID. A failed
  first startup can now preserve its earlier submitted input as evidence on
  retry, rather than dropping that input.
- Root mention marks, path acknowledgements and the one-shot plan reference
  are reset at transition. Known instruction paths remain with nil hashes,
  preserving scope through failure/reopen. Gptel stages full current path
  content through normal reminder commits; new/resumed native prompts use
  exact SDK context receipts. Child path hashes are unchanged. Codec v0.5.9
  persists nil acknowledgement hashes; old sidecars are rejected, with no
  compatibility reader. The existing codec round-trip fixture includes an
  undelivered nested path as well as an acknowledged root path.
- New public-send tests: both directions first reproduced the old guard
  (transfer-red2.log). Basic continuation passed in transfer-green3.log.
  The context test then demonstrated loss of known path identity after a hash
  reset (transfer-context-red.log); nil acknowledgements and receipt-driven
  refresh pass in transfer-context-green.log. Actual MCP Read output survives
  into the serialized API tool pair (transfer-tools.log). The final round trip
  starts a different native ID and resumes that replacement after save/reopen
  (transfer-roundtrip.log). The API side uses real dry-run serialization; this
  is not a live API inference claim.
- The first broader run passed103/104, with the old startup-retry fixture
  assuming its first text block was the only submitted input. It now echoes
  all submitted text blocks and verifies the retry input in the answer. The
  final broader run passed253/253 across native root/child/input, persistence,
  codec and transcript publication (transfer-retry-green.log). A separate
 104-case run had already passed context and request-policy tests except that
  one now-corrected fixture. All228 files compile with zero warnings
  (transfer-compile.log), followed by Eask bytecode cleanup. Final focused
  transfer/compaction/context/policy/cold-restart regression passed23/23 in
  transfer-final.log. No full-suite, live editor reload, final review or commit.
- Updated README, current session contract and ADR0115. No new blocking user
  decision. A17/A18 have deterministic workflow evidence; live/remote acceptance
  remains to be completed with the full feature. Next priorities remain exact
  Goal budget/capability and model-callsite audits, oversized-context automatic
  continuation, explicit missing-history recovery, waiting/nested agent cases,
  real A16 programming and provisioned remote acceptance, then the full suite,
  final review and commit. The implementation goal remains active.

## Native sample usage and Goal workflow (2026-10-06)

- The final-turn-only usage concern is resolved for the pinned adapter. Its
  raw SDK stream reports top-level message identities and cumulative sample
  counters before PostToolBatch. Added native usage observation that merges
  streamed deltas and consolidated snapshots by identity, ignores nested and
  synthetic messages, preserves missing fields, and never adds a final prompt
  total to already observed samples. Cache creation is normalized input;
  cache reads stay separate. Terminal totals replace only reported fields.
- Root budget warnings use the existing Goal threshold and acknowledged
  reminder delivery at the post-tool boundary. Child samples charge their
  owning Goal incrementally; final settlement charges only the remaining
  delta. No second Goal controller or unsupported hard token-cap claim.
- Two bounded live probes used the supported own Enterprise login, Sonnet/low,
  CLI2.1.291, adapter0.86.0, SDK0.3.287 and ACP0.15.2. The first ran two real
  MCP Reads and three samples in5.1435s: normalized input2425/output173,
  cache-read9216/cache-write2419. Per-identity sample totals exactly matched
  final prompt totals. The second set an explicit one-token fixture budget
  and ran in5.6218s: at the first post-tool boundary, known input2707/output84
  delivered the100% reminder; Claude wrapped up without the next planned Read.
  Final input3111/output259, cache-read5897/cache-write3107 charged3370 once.
  These establish hook timing and accounting, not exact quota consumed,
  Pro/Max-account-specific validation, or A16 programming acceptance.
  Disposable evidence: live-step-usage.el, live-step-usage.log and
  live-goal-budget.log under .scratch/claude-code-engine. No credentials were
  inspected/copied and no paid-overflow or API fallback was enabled.
- Native budget test first reproduced absent50% delivery in
  goal-usage-confirmed-red2.log after correcting the fixture's nested JSON
  arrays. It now verifies50/80/100 crossings, duplicate and older snapshots,
  ignored foreign/synthetic messages, exact intermediate counters and final
  single charging. The new retained-child test checks progress before final
  settlement, missing terminal input, two retained follow-ups and highest
  threshold delivery while the root is still running.
- The normal session workflow test starts a Goal, allows automatic
  continuation, queues composer input while busy, pauses at the tool boundary,
  resumes and verifies the queued input reaches the native prompt before a
  generic continuation. Three ordinary turns settle successfully and charge45.
  A separate MCP UpdateGoal test runs the real retained verifier through the
  goal-review workload on Opus: FAIL ends the first root turn, the next root
  continues, PASS completes it. Both root and verifier usage total60 once;
  the verifier receives the objective without the implementer's assertion.
  Early verifier fixture failures were missing built-in tool registration,
  followed by an incorrect expected error phrase; no production workaround.
- Final targeted regression:103/103 across native Goal/root/child/context,
  ACP settlement, existing Goal tools and retained-agent runtime
  (goal-workflow-regression.log). Earlier accounting-only regression37/37.
  All 229 files compile with zero warnings (goal-compile.log), followed by
  Eask bytecode cleanup; git diff --check passes. Refreshed upstream gptel
  (already current) and compared its Anthropic usage normalization.
- Updated current Goal contract, ADR0069, module map and the glossary's stale
  child-agent budget exclusion. A13 now has deterministic end-to-end workflow
  evidence plus bounded live budget-boundary evidence. The full goal is not
  complete: oversized-context continuation, explicit missing-history recovery,
  remaining workload/capability/model-policy audits, waiting/nested agent
  permission acceptance, real A16 programming and provisioned remote acceptance,
  full suite, final review and commit remain. No blocking user decision.

## Explicit missing-history recovery (2026-10-06)

- Added `M-x mevedel-claude-code-recover-history` for root and retained-child
  native conversations. The command runs from the session view/data buffer,
  offers only root/retained-child references, requires a writable owned idle
  session and a non-active Goal, and persists detachment without starting any
  model request. The next ordinary root send or child follow-up starts a new
  Claude conversation with a labelled excerpt; no exact-resume claim and no
  transcript/effect deletion. Directive prompts already reconstruct their
  selected context and are not recovery selections.
- Reused the existing projected-evidence continuation. Its owner moved into
  the native-history module so root and child paths share framing and omission
  rules. Root recovery excludes independent directive/item turns; child
  recovery projects only its private effective transcript. A child's durable
  unstarted marker preserves recovery intent across startup failure/reopen;
  ready publication replaces it with the real new native ID. Frozen agent
  configuration, root history and sibling references remain intact.
- Shared detachment now serves API transitions and explicit recovery, with
  all in-repo callers renamed directly. Scoped path acknowledgement reset
  preserves known paths. Failed storage publication restores previous native
  references, root mentions, instruction hashes and the plan-reference receipt
  in memory. Existing pending-publication authority gates remain responsible
  for uncertain target publication; recovery does not bypass them.
- Root and child startup failures retain their original diagnosis and name
  the recovery command when native history is unavailable. Actual pinned
  adapter source maps missing resumed CLI history to resource-not-found; the
  deterministic peer exercises its startup-error path. No live subscription
  history was deleted to test recovery.
- Tests at approved public seams: root workflow first failed for the missing
  command (recovery-root-red.log), then passed. Root/child tests then failed
  for the missing actionable option (recovery-child-red.log), then passed
  with recovery, retained evidence and a replacement ID. Guards prove busy,
  read-only, active-Goal and unknown-scope refusal without detachment, and a
  paused Goal stays paused. Injected failure at the storage publication
  boundary proves reference/context rollback. The initial rollback fixture
  used an unsupported reminder constructor keyword; replaced it with the
  existing plan-reference factory.
- Broader native root/child/session/restart/Goal/transfer regression passed
  36/36 (recovery-regression.log). Final recovery/transfer run, including
  rollback, passed6/6 (recovery-final2.log). Compilation first exposed one
  long docstring; corrected it. Final229-file compilation has zero warnings
  (recovery-compile-final.log), followed by Eask bytecode cleanup.
  git diff --check passes. Updated README, sessions/agents contracts,
  module map and ADR0115. No full suite, final review or commit yet.
- The previous turn was verified progress, and this turn closes the explicit
  missing-history recovery gap. Next: oversized changed/restoration context
  currently errors at the10k UTF-16 hook limit in the context module and needs
  an automatic full-context prompt continuation before subsequent work.
  Preserve receipt checks, interruption, per-sample/terminal usage and child
  sample limits when adding that behavior. Remaining workload/capability/model
  audits, waiting/nested child permission acceptance, live A16 programming,
  provisioned remote acceptance, final full suite/review/commit remain in scope.
  The implementation Goal remains active; no blocking user decision.

## Automatic full-context continuation (2026-10-06)

- Oversized required context no longer needs a new user turn. Context staging
  retains the full body above the10k UTF-16 hook limit. The ACP turn can submit
  another native prompt after a successful prompt result, keeping the same
  conversation, request admission, MCP server, tool ledger and transcript owner.
  Only final settlement advances the canonical/Goal turn. Exact SDK user receipt
  publishes the captured body once and commits its original deliveries. Pausing,
  cancellation and child sample caps prevent a further prompt; failed prompts
  and missing receipts still fail closed. Oversized mail alone retains its
  existing queued-until-next-prompt policy.
- Native usage retains a frozen base for completed prompts and adds the current
  prompt once. Missing final fields retain known progress; unknown is not zero.
  Delayed snapshots from retired message IDs are ignored. The regression first
  reproduced duplicate accounting (43/25 instead of30/18 input/output), then
  passed after tracking retired sample identities. Root Goal and child delta
  charges remain on their existing shared accounting paths.
- Ordinary live evidence: a two-Read task changed memory after its first tool
  batch to a body above the hook limit. It stopped at PostToolBatch, received
  the complete body in a second prompt, completed the second Read and returned
  OVERSIZED-CONTEXT-731942 from the end of that body. It passed in6.4056s with
  one admitted turn and two native prompts (live-large-context.log).
- The first real oversized restoration probe failed after compaction at batch12:
  SessionStart(compact) acknowledged its hook but ignored continue:false. The
  next MCP tool correctly failed admission for unacknowledged context. A
  diagnostic run established that error and event ordering. The initial peer
  had incorrectly stopped at SessionStart and was corrected. Official hooks
  documentation likewise describes SessionStart as context-only.
- PreToolUse now blocks pending effects until full-context continuation. A stop
  alone proved insufficient in the live adapter, so the hook explicitly denies
  the call and stops the native prompt. The peer now requires both decisions.
  These are native flow controls; actual allowed tool work still goes through
  mevedel's permission/review pipeline and independent receipt admission guard.
- Real restoration then passed: the22-Read chain compacted after batches12 and21,
  used two automatic continuations/three native prompts, and retained three
  readable transcript segments. All22 admitted tools completed exactly once;
  the current segment held the final tool and earlier tools remained archived.
  Its final answer contained SYSTEM-RESTORED-826491, PATH-RESTORED-583294 and
  MEMORY-RESTORED-937582, the last beyond11k characters of restoration padding.
  Elapsed76.1937s; normalized totals input130052, output2406, cached914652,
  cache-creation130002. These are reported token/cache counters, not subscription
  allowance. Log: live-large-restoration-deny.log.
- Live configuration: production launch with Sonnet/low, built-in tools disabled,
  custom system snapshot disabled,100k automatic-compaction window and a bounded
  native maxTurns/deadline in a disposable workspace. Actual model
  claude-sonnet-5-5; CLI2.1.291, adapter0.86.0, SDK0.3.287, Node26.10.0.
  Used the supported existing Enterprise login, no API billing or overflow.
  This verifies supported subscription transport and restoration, not Pro/Max
  account-specific validation or A16's multi-file programming acceptance.
- Retained-child tests caught another boundary bug: a denied post-compaction
  attempt had spent a sample, but continuation reused that reservation. A child
  now reserves its next sample after the previous prompt settles; a two-sample
  cap stops after the denied attempt, and a three-sample cap permits exactly one
  restored sample, including its delivered final-turn warning. Text-only
  responses after compaction obey the same reservation. Neither path resets the
  cap or advances the root turn count.
- Verification at the approved seams: restoration-pretool-red.log and
  restoration-pretool-deny-red.log reproduce the native hook control gaps;
  restoration-child-cap-red.log and restoration-child-text-red.log reproduce
  sample-limit bypasses. All78 native engine/root/child/Goal/persistence/context/
  model/compaction tests passed (restoration-regression.log,80.48s). After the
  last text-only cap fix, focused continuation/child/context tests passed10/10
  (restoration-final.log,23.10s). Earlier continuation regression passed45/45.
  All 229 files compile with zero warnings (restoration-compile.log), followed
  by Eask cleanup. Updated architecture, sessions, agents, reminders, Goals,
  module map and ADR0115 to the implemented contract. No full suite, final
  review or commit yet.
- The user's test-seam confirmation remains recorded in the canonical PRD and
  handoff; no additional approval is needed. This closes oversized changed and
  restoration context. Remaining work: child permission while a parent waits,
  nested/capacity acceptance, full workload/model/capability callsite audits,
  exact full-system baseline evidence, real A16 programming and provisioned
  remote acceptance, final full suite/review/commit. The implementation Goal
  remains active and there is no blocking user decision.

## Waiting parents and child permission ownership (2026-10-06)

- Added an acceptance test through ordinary root send, real Agent/WaitAgent/Read
  MCP calls, retained child runtimes and the root permission view. The parent
  remains in WaitAgent while its child is blocked on an explicit Read ask rule.
  Permission approval and denial worked immediately. Interrupting the child
  exposed a real gap: its terminal result woke the parent and released capacity,
  but the permission card remained in the queue (child-permission-wait.log).
- Native child turns had relied only on their invocation and lacked the ordinary
  admitted request that owns prompt cancellers and queue identity. They now call
  the existing request admission function in their retained conversation buffer.
  The invocation continues to own external history and terminal publication;
  there is no synthetic gptel FSM. ACP startup, event ownership, cancellation and
  MCP admission check that captured request. Interruption reuses its cancellers
  and the existing request-specific queue sweep. Root turn accounting remains
  unchanged. This fixes cleanup at the shared lifecycle seam rather than adding
  a separate native permission registry or blanket root-queue abort.
- Test fixture corrections: the public retained outcome is `interrupted`, not
  the runtime's `aborted` status. Nested acceptance waits for both ancestors to
  report waiting, because the grandchild can enqueue permission before its
  parent has reached its next tool call. Neither correction weakens the effect
  or cleanup checks.
- All six direct/nested approval, denial and interruption scenarios now pass.
  The UI attributes the card to the leaf's canonical path. Only approval yields
  the evidence, late approval after settlement is ignored, and the root settles
  once. Waiting turns still consume the configured one/two child capacity slots:
  another spawn fails while full, then a replacement child starts successfully
  after settlement and can be interrupted cleanly.
- Native child/Goal/recovery/restart/MCP/ACP regression passed28/28 in28.69s
  (child-request-regression.log). The final expanded wait and ACP ownership
  tests passed7/7 in9.87s, including six scenarios in the wait case
  (child-wait-final.log). All 229 files compile with zero warnings
  (child-request-compile.log), followed by Eask cleanup. git diff --check passes.
  Updated agents.md and the canonical interaction ownership ADR0060.
- This closes the outstanding waiting-parent permission, nested wait and
  capacity acceptance gap in A10; previously tested follow-up/mailbox behavior
  remains covered by the retained native regression. No new live model call was
  needed: real ACP peers and MCP transports exercise actual lifecycle and UI
  boundaries deterministically. Remaining: full workload/model/capability
  callsite audits, full-system baseline evidence, real A16 programming,
  provisioned remote acceptance, final full suite/review/commit. The Goal is
  active with no blocking user decision.

## Real programming, remote targets and raw-send audit (2026-10-06)

### A16: completed programming task with retained evidence

The bounded Eventfold task implemented five empty Python modules from an
independent API contract, twenty JSON cases and a five-method unittest suite.
The native run used the supported existing Enterprise login, Sonnet (actual
`claude-sonnet-5-5`), low effort, built-ins disabled and only mevedel Read,
ApplyPatch and Bash. This is evidence for the supported subscription engine;
it does not claim testing with a separate Pro/Max account.

- Completed in 71.10 seconds within the 300-second / 40-sampling-turn bounds.
  One reviewed patch changed five modules, with 5,464 bytes of reviewed content.
  The model read the PNG as media and correctly reported red. It ran unittest
  successfully and reported completion. No interruption was needed in this run.
- 80 distinct tool calls: 33 successes, 46 tool errors and one permission denial.
  The fixture exposed only Read for investigation, and ordinary filesystem Read
  rejects directories. The model spent many calls guessing case filenames using
  missing-file suggestions; all twenty real cases were eventually read. This
  is a deliberately constrained roster, not a representative efficiency result.
  A requested discovery command was denied through normal UI feedback; the
  subsequent permitted unittest command was approved once.
- Available final usage: input 30,831, output 10,231, cached input 317,915 and
  cache creation 30,793. No API fallback or paid overflow was enabled.
- The live ERT driver reported a failed final assertion because it counted
  unsuccessful case-name guesses as case reads (59 unique guesses versus 20
  fixture files). It had already captured success, all tool outcomes, patch
  measurements, response, source and transcript. The driver now counts only
  successful reads. No further model call was made to paper over this defect.
- Offline `verify-programming.py` parses each successful case Read from the
  retained transcript and compares its full JSON body with the original
  independent fixture. All twenty match. Tests, cases, README and image are
  byte-identical to their originals; only the specified five source modules
  changed. The independent Python unittest run passes all five methods,
  including twenty case subtests. The verifier also checks distinct call IDs,
  successful patch/Bash outcomes, review size and media/color evidence.
- Evidence is local under `.scratch/claude-code-engine/`: live-programming5.log,
  verify-programming.py, and programming-result-bRLugv/{transcript.org,
  driver-state.el,verification.json,tests.log,eventfold,cases,tests,README.md,
  badge.png}. The original failed ERT result is retained, not relabelled green.
  Its later turn-count assertion was not reached; single-settlement evidence
  remains in the deterministic session acceptance tests.
- Earlier attempts are retained in live-programming.log through
  live-programming4.log: driver assumptions about Bash's dedicated permission
  entry shape and allowed command suffixes stopped the first three attempts;
  the fourth failed Lisp parsing before a model call. The corrected driver
  returns UI feedback for unapproved commands instead of aborting the test.

This supplies the real multi-file programming evidence required by A16,
separate from earlier synthetic Read-chain and compaction experiments.

### A05: actual SSH and Podman execution through ACP/MCP

Added a normal-session acceptance case using the actual local ACP transport,
private MCP bridge and registered Read/ApplyPatch/Bash tools. The deterministic
peer is local; its process directory contains a conflicting input file. The
session owns a provisioned remote project and requires Bubblewrap confinement.
The test proves Read returns remote evidence, patch and Bash change only the
remote files, the local decoy stays unchanged, and the turn settles once.
Both SSH and Podman run within the case. No model or login is needed for this
transport/authority test.

`MEVEDEL_TEST_REMOTE_TEST=mevedel-claude-code/remote/test
 test/run-remote-acceptance.sh` passed in 12.63 seconds (native-remote.log).
The runner now includes the new case in selected and full remote acceptance.
The final fixture also dynamically isolates TRAMP connection settings and the
storage-disclosure cache; native-remote-final.log records the repeat after
that cleanup adjustment. The runner removes its own disposable containers and
volumes. No Docker or physically separate-host claim is made.

### Request callsites and raw data-buffer dispatch

The model-callsite audit found raw `gptel-send` and viewless `/init` bypassed
engine selection. A public send regression first failed with “Subscription
send reached the API”. Raw sends now dispatch the resolved request policy
through the existing engine seam after slash/skill preparation. Paired views
still skip rescanning prepared skill text. `/init` directly uses the same
engine dispatcher. API requests retain their original gptel send implementation.

A second red test showed raw sends could reuse an already-admitted request.
Raw submission now rejects busy or read-only sessions before launch. Prefix-zero
steering explicitly reports that external engines do not support it; prefix-four
still opens the gptel menu without submitting or expanding a prompt. Tests cover
raw, paired and init native completion plus admission/steering refusal. Two old
stash-cleanup fixtures used an invalid model-selector shape; they now seed a
valid nonempty permission context because model resolution is not their subject.

The final focused native input/init/skills run passed 137/137 in 5.07 seconds
(raw-admission-green.log). All 229 production files compiled with zero warnings
(raw-send-compile.log), followed by Eask cleanup. Updated skills documentation
for dispatch and removed its stale cross-engine-transfer refusal description.

The remaining direct `gptel-request` callsites were inspected: chat and
retained-agent dispatch, directive requests, context summaries, Buddy,
consolidation estimation and consolidation execution all select their native
branch before gptel. The engine's ordinary text method is specialized on the
API backend. Naming and guardian use isolated engine text; journal digestion
uses context-summary, with no inference in the storage worker. Existing native
workload tests cover naming, guardian, digest, Buddy and consolidation. Buffer
kill hooks own cancellation for isolated native work; callers ignoring a returned
canceller do not leak the native process when they kill the request buffer.

Remaining before completion: finish A14's visible menu/direct capability audit
and A20's unavailable-ID policy-route coverage; consolidate full-system prompt
baseline evidence; final full suite, current-contract/ADR audit, code review and
commit. The Goal is active with no blocking user decision. No commit, merge,
publish or live-editor reload was performed.

### External-history command and menu acceptance

Added a public lifecycle test that first completes a real ACP fixture turn,
then exercises direct Compact, Rewind, Redo, side conversation and prefixed
Save Session, plus view/menu Conversation Fork, Worktree Fork, Rewind, Redo
and Compact. Every refusal must name the external-history restriction without
opening a picker, asking for a name or requesting confirmation. The test checks
that no fork is armed and preserves the native reference, session identity,
transcript and multiline composer draft. The view must actually render the
settled transcript before testing response-at-point actions; creating the paired
view alone only initializes its zones.

Two red failures exposed late capability checks. Redo first reported the absence
of other heads, hiding the real capability restriction and potentially offering
a picker when heads existed. It now calls the existing history guard before
querying them. Prefixed Save Session asked for a new name before the lower-level
transaction guard; it now checks before mutation authority, materialization,
parent saving or naming cancellation. Transaction-level guards remain in place
for direct transaction callers. No new capability framework was added.

The lifecycle test passes (capabilities-green2.log, 1.07s). The broader native
capability/persistence/Save As/Rewind/menu regression passes 316/316 in109.99s
(capabilities-regression.log), including existing API lifecycle behavior.
All 229 production files compile with zero warnings (capabilities-compile.log),
followed by Eask cleanup (capabilities-clean.log). The final provisioned native
SSH+Podman repeat passed in13.16s (native-remote-final.log).

The history-operation portion of A14 now has command and menu evidence. Final
model-control/bridge audit, A20's unavailable-ID policy-route coverage,
full-system baseline evidence, full suite/review/documentation audit/commit
remain. No live editor was reloaded and no work was committed.

## Final policy, controls and baseline checks (2026-10-06)

A20 now has actual admission-path coverage for an unavailable model selected
through session selection, a preset, planning workload and directive override.
The test preserves the production launcher's catalog check while substituting
only the external peer. Each route fails before prompt dispatch, names selecting
a listed model as the next action, settles its request and makes no API call.
Existing valid policy routes plus these cases pass 2/2 in 3.03s
(`unavailable-policy2.log`). The first attempt exposed only missing fixture tool
registration (`unavailable-policy.log`); no production fallback was found.

The gptel HTTP-controls menu now refuses external providers before opening its
transient or changing bridge return state. Direct bridge/menu, prefixed send and
paired view paths point to mevedel's supported model/effort and tools menus.
The new test plus bridge/menu regressions pass 120/120 in 2.24s
(`native-controls-green.log`); the red run opened the unsupported transient.

The full normal implementation-preset baseline was checked through production
launch and the installed adapter. The composed core is 11,175 characters;
with two acceptance canaries it is 11,481 characters, SHA-256
`f8675bea1573e5fd279d3c354a3bbe6b979ec23aaae00634579c74ee46a011cf`.
The full core is unchanged in the custom system-prompt option, with snapshot
false and built-in tools empty. Preflight passed without a model call.
The live run used the supported existing Enterprise login, Sonnet 5.5 / low,
a four-sample/90-second bound and completed in 5.30s. It returned the two system
canaries, workspace guidance canary and file canary, made one successful Read,
and correctly described mevedel's tool authority and patch review. There were
two SDK user receipts and successful native terminal settlement. Reported usage:
input 13,921, output 284, cached 13,780, cache 13,917; these are not allowance
measurements or Pro/Max-account-specific evidence.

The original live ERT result remains FAILED: the driver incorrectly required
more than eight direct tools. The actual normal preset exposes seven: Read,
Glob, Grep, ApplyPatch, Bash, ToolSearch and ToolCall. Specialist tools are
available through discovery/programmatic calls. Offline verification of the
retained log, full prompt and transcript passed with that exact roster, all
four canaries and one successful Read. The later turn-count assertion was not
reached and is not claimed from this run. The local driver now checks the exact
roster; no further subscription call was made to replace the failed driver
result. Artifacts: `baseline-result-EQD6sj/verification.json`, system snapshots
and transcript; log `full-baseline-live.log`. Verification records hashes of
actually loaded isolated gptel source and bytecode, without claiming equivalence
to the user's editor. Official subscription support and integration terms were
rechecked before this run; no credentials were copied and no overflow enabled.

ADR 0123 records the implemented request/engine/tool ownership boundary, with
current glossary, architecture and user setup/control documentation updated.
The final full suite began with 9,385 tests across four isolated workers.
The required two-axis review found no hard standards violations and one
nonblocking duplication concern (root/child native-history record operations).
Spec review found three actionable gaps: editing earlier raw transcript text,
unsupported steering not automatically queuing, and unguarded cooperative
session control transfer. These remain work to resolve before completion;
this review is not a release verdict. No implementation commit has been made.

### Final review fixes and failed full-suite diagnosis

Standards review reported zero hard violations and one nonblocking judgment:
root and child launchers duplicate some native-record transitions. Spec review
reported three gaps. Busy Claude composer sends now use the existing follow-up
queue with a visible next-turn message and retained FIFO identity; 59/59 input
and pending-input tests pass (`review-queue-green.log`). Cooperative transfer
now refuses native history before request/grant/release and acquisition, checks
refreshed adoption state, and rejects stale requesters in the automatic owner
poll. Command/menu plus existing control tests pass 24/24
(`review-control-green.log`); the larger persistence/durability run passed
266/266 (`review-transfer-regression.log`). An additional stale-requester
assertion was added afterward and remains part of the next run.

Earlier raw transcript edits now record durable `diverged` history and require
explicit excerpt recovery before continuation. Normal draft edits stay allowed.
The first attempted before-change refusal was unsuitable because Emacs removes
an erroring modification hook; durable divergence replaced it. Workflow coverage
passed after fixing old fixtures that inserted follow-ups before the prior
response rather than appending them. Follow-up review found a remaining
no-output case: a submitted prompt interrupted before the first answer needs
its own durable boundary. This is being covered and fixed before completion.

The first full run (`final-suite.log`, report `20261006-202403`) is FAILED, not
9,385 passing tests. Three workers completed; worker three stalled after case87
and was stopped, so the runner rejected its incomplete inventory. Diagnoses:

- The old user-turn producer test stubbed `gptel-send`; `/init` now dispatches
  through the shared request path. It accidentally entered the API transport; no successful model response was
  observed. Its later sentinel failed inside another test. Stubbing the actual API
  boundary fixes this; replaying all88 cases with full file loading and a guard
  against unexpected API dispatch passes (`reproduce-worker3-fixed.log`).
- The cold reviewer fixture had no concrete backend. It now explicitly loads
  the OpenAI provider and supplies a test backend, exercising the generic engine
  route without credentials. Cold owner/user-turn/recovery checks pass8/8
  (`review-regression-green.log`).
- A chat initialization test created an inspection view outside its view stub,
  leaving a repeating control timer. The timer probe identified that exact case;
  inspection now shares the intended stub. Further suite validation remains.
- Telemetry's pending-tool stream failure test requires upstream's corrected
  failure ordering. The frozen dependency still transitioned before recording
  the Curl error. The isolated dependency now uses the already-inspected current
  upstream `edb3fee3b5266e9060f6d121e9b3914eb7c3409d`; old source is retained under
  scratch, and `gptel-validation-snapshot.json` records every before/after hash.
  No user's editor or main-checkout dependency was changed. Package directory
  naming remains the older MELPA stamp; the source commit/hash is authoritative.

With refreshed compiled gptel, native/session/context/recovery and telemetry
checks pass96/96 (`review-gptel-current-green.log`). The earlier live evidence
continues to identify its original frozen dependency; it has not been relabelled
as a run against the refreshed source. All 229 production files compiled without
warnings (`review-final-compile.log`) before the subsequent no-output-boundary
work; Eask bytecode cleanup has run for its tests. Final full suite, review of
remaining fixes and final compile/commit remain outstanding.


### Submitted history and final validation follow-up

The no-output gap now has a persisted segment/body-offset input boundary. It
survives interrupted save/reopen, excludes mutable Org metadata, and protects
submitted input while keeping unsent drafts editable. Active abort preserves
`diverged`; explicit recovery remains required before another native dispatch.
The before-change observer preserves match data. A first publication into an
empty buffer now moves its collapsed response marker beyond the new header,
and native reasoning publication is explicitly host-owned.

Focused session/transfer validation passes 29/29 (`review-prefix-final.log`).
The final spec reviewer accepted the no-output fix and found no remaining gaps.
Production compilation passes 229 files without warnings (`final-compile.log`).
The second complete run discovers 9,387 cases. It exposed a metadata-only false
positive: gptel's before-save hook removes/replaces leading drawer properties.
The observer now excludes edits contained in that header, while edits crossing
into the body still count. A full-roster-loaded run of directive, no-output
(including repeated metadata saves), and boundary-codec cases passes 3/3
(`full-load-history.log`). The running full suite predates this final fix and
will not be reported as green; the corrected source needs final validation.

The second full run completed all 9,387 cases in 318.80 seconds: one unexpected
(the metadata-save case), 24 conditional skips. All other workers completed
successfully. After the body-only guard and its focused test, all 229 production
files again compiled without warnings (`final-compile2.log`). Final run three
uses eight isolated workers and the measured duration table from run two;
report `20261006-210840`, log `final-suite3.log`. No source edits are planned
while awaiting that result.

Run three completed all 9,387 cases in 141.80 seconds: one unexpected,
25 conditional skips. The remaining failure was the existing YouTube cleanup
test executing as its worker's first URL request. Emacs lazily created its
hourly cookie-save timer inside the measured interval; this was not a mevedel
request timer. The shared local HTTP fixture now disables cookie/history save
intervals, preserving its strict timer leak assertion. The exact cold-start
case passes (`cold-web-failure.log`). Production code is unchanged from the
warning-free compilation. Run four repeats all cases with this fixture fix.


## Final acceptance — 2026-10-06

The complete final isolated Eask run passes: **9,387 cases, zero unexpected
results, 23 conditional skips**, all eight workers exit 0, 143.71 seconds.
Report: `.scratch/test-suite-performance/20261006-211214/summary.json`;
log: `.scratch/claude-code-engine/final-suite4.log`. The runner performs bytecode
cleanup and validates that every discovered case ran exactly once. All 229
production files compiled without warnings on the final production source
(`final-compile2.log`); only the HTTP test fixture changed afterward.
`git diff --check` is clean.

The final [acceptance index](claude-code-engine-acceptance.md) maps A01–A21,
records the independent Standards and Spec reviews, and identifies all live-run
limits. Standards has zero hard violations and one nonblocking P3 duplication
judgment; Spec has no unresolved findings after follow-up review. The local
Easkfile used for offline dependency metadata is moved under scratch before
commit. Scratch execution logs, dependency copies and the local PRD remain
untracked. The feature branch contains the implementation, tests, current docs
and reviewed versioned working material; no merge, publication or editor reload
is part of this delivery.

Session sidecars require v0.5.9. Older saved records are rejected without
migration; their files remain intact. Live subscription evidence is from the
existing Enterprise login, not a separate Pro/Max account. These limits are
explicit in the acceptance record and handoff.


## First-use follow-up (2026-10-06)

The user's screenshot exposed an acknowledged native reminder as raw XML after
authored input. A focused paired-view regression reproduced it: the parser's
head-of-property-run rule rejected the reminder after the unclassified delivery
marker. Canonical receipt markers now establish the synthetic block boundary;
the view omits that marker and uses its existing expandable reminder row.
The regression covers expansion, restoration, quoted assistant prose and a
multiline composer draft. Stored receipt text remains unchanged.

Cold Claude model selection now includes Fable and documented effort choices
for Sonnet, Opus and Fable. Initial SDK launch leaves effort unset and removes
an inherited effort override. After ACP reports capabilities, startup applies
the selected level through `session/set_config_option`, waiting for the actual
acknowledgement before sending a prompt. Unsupported effort selects native
`default`, announces the fallback and resets a still-matching session selection.
Missing/wrong models and a rejected or mismatched configuration still fail
before prompt dispatch. Cancellation includes pending configuration.

The fixture peer reports only an effort it received in a real configuration
request, testing both ordering and fallback independently of model prose.
The saved session's `high` setting was changed by the user after the response;
it does not establish the first prompt's effort. No paid prompt was needed for
these changes. Adapter installation is persistent and is never repeated by chat.

Official capability reference checked on 2026-10-06:
https://code.claude.com/docs/en/model-config.

Follow-up verification: 9,391 complete ERT cases, zero unexpected results,
23 conditional skips, eight workers exit 0, 143.75 seconds. Report:
`.scratch/test-suite-performance/20261006-225855/summary.json`.
Compilation: 229 files, no warnings (`ui-followup-compile.log`).


## Explicit old-session migration (2026-10-06)

The user requested a narrow exception for selected older sessions while keeping
the no-compatibility rule. The `v0.5.6` -> `v0.5.9` metadata delta is the required
empty `:external-conversations` field; the transcript format is unchanged.
Added a standalone, explicitly invoked copy converter under `scripts/`, outside
the runtime loader. It validates the current output schema and all published
artifact hashes, converts retained history sidecars, updates manifests, and
refuses live ownership, unresolved recovery, links, corruption and older formats.
It retains the original source and cleans up an incomplete destination.

Validation: 63 migration/codec ERT cases pass, the converter compiles with
warnings treated as errors, and 229 production modules compile without warnings.
Two selected real sessions were converted: 19 sidecar files and 17 manifests;
154 other files are byte-identical. Their originals were retained as backups.
Both appear in the ordinary session picker. The current decoder was also
checked in the user's configured editor, preserving the retained agent. An
initial empty batch environment lacked the Codex backend and dropped that agent
in its temporary decoded object; no stored bytes were changed by that check.
The configured-editor check passed without warnings and preserved agent counts.
Older `v0.5.0` sessions were left untouched. No model request was made.

## Documentation consolidation — 2026-10-07

Run evidence and fix details removed from ADR 0115/0060/0063 decision histories
when the receipt contract moved to `docs/sessions.md#native-context-delivery`
and its rationale to ADR 0123. Items already recorded above (live 12,223-char
prompt echo, 22-Read compaction chains, 6.4 s and 76.19 s continuation runs,
image echo, PDF non-advertisement, child permission WaitAgent test, two-process
restart) are not repeated.

- Review fixes: wire markers and raw observation text in the transcript cost the
  view a regexp pass over every hidden segment, 3.2 ms per pass on a 2.3 MB
  transcript. The UTF-16 hook-limit check counted the encoder's byte-order mark
  and rejected output of exactly 10,000 units. A gptel directive after an engine
  switch could stage root path instructions and acknowledge them for a root that
  never received them (now moot: directives start isolated conversations).
- Selected text context: collecting only media dropped selected files and
  buffer regions; a bounded live turn per placement returned a marker supplied
  only through the selected file. This verifies use, not an exact SDK receipt
  for system placement.
- Scoped instructions: a separate-editor restart test exposed local
  `SessionStart` discarding learned native scopes; root hashes also suppressed
  guidance in fresh directives until request-local hashes fixed that scope.
- Shared reminders: the existing recovery test caught a duplicate recovery
  notice with configured reminders enabled; routing root recovery through the
  shared pending-event owner removed it.
- Observation hooks: a bounded live Claude Code 2.1.291 run accepted a changed
  memory section and discovered path instructions in one exact hook receipt and
  used both markers. Deterministic cases reject foreign-session, malformed,
  mismatched and failed receipts and oversized emoji payloads.
- Native compaction restoration: a live turn with more than 12,000 characters of
  system instructions compacted after 12 batches, accepted an exact
  SessionStart receipt with changed memory and completed 10 further reads.
- Child sample cap at continuation: retained-child tests caught a skipped sample
  charge; a two-sample cap now stops after the denied attempt, a three-sample cap
  permits exactly one restored sample with its warning, and a text-only
  post-compaction sample cannot bypass the cap.
- ADR 0060 child permissions: tests cover approval, denial and interruption for
  direct and nested children while ancestors wait, reject late approval, and
  reuse capacity after settlement.
- ADR 0063 cold tool registry: the in-process tests had already populated the
  global gptel registry and missed the unloaded `ToolCall` dependency; cold
  decoding now initializes missing built-ins through their registrar.
