# Session telemetry and profiling

mevedel writes a versioned, append-only diagnostic event stream for every
session. The stream is evidence for postmortems and performance analysis; it
is not session state and is never read during resume.

## Storage and representation

The stream lives at `SESSION_DIR/telemetry-log.el`. Each line is one readable
Emacs Lisp plist. This uses the package's existing append-only log convention,
requires no serializer dependency, and remains streamable even when a run is
interrupted. Every entry has schema version 1, an ISO wall time,
process-relative elapsed milliseconds, a process-local sequence number, the
turn, and any current preset and Goal identity. Session identity comes from
the containing session directory, not a repeated `:session-id` on every line.
Keep that directory name when copying logs for analysis. Nil-valued properties
are omitted, including inside nested plists: an absent field means nil or
unknown, never zero. Numeric zero, true flags, and nil positions in ordinary
lists and vectors are retained. Schema tags remain per-event so incomplete
or concatenated streams remain independently interpretable; preset and active
profiler/Goal identities remain per-event because they can change during a run.
Existing files are not rewritten. The offline cache analyzer derives session
identity from the containing directory for all rows, including previously
written rows with the now-redundant session field.

Sessionless background work uses the same compact envelope and privacy filter at
`WORKSPACE/.mevedel/state/diagnostics/telemetry-log.el`, through target-native control
operations on local and TRAMP workspaces. It has no session identity or Goal
and does not manufacture a conversation. It follows the same enable switch;
disabled telemetry creates no diagnostic state.

Workspace appends rotate before `telemetry-log.el` exceeds 10 MiB, keeping one
`telemetry-log.el.1` archive. Cooperating writers share a target directory lock.
The next rotation replaces the old archive. An already oversized log is trimmed
to its last complete lines within 10 MiB on rotation; a single oversized event
is rejected with the ordinary telemetry failure diagnostic. No age timer is
needed. Session telemetry keeps its existing session-owned lifetime.

On Linux, elapsed milliseconds and span durations use the kernel monotonic
clock exposed by `/proc/uptime`. Other systems fall back to process-relative
wall time and clamp emitted elapsed values so they never move backwards. Use
`:duration-ms` for latency analysis and `:time` for correlation with external
logs.

Permission queue events use a stable `:permission-id` across admission, initial
display, and settlement. `permission-displayed` occurs once per card, including
when that card is later refreshed; `permission-enqueued` alone does not mean the
user saw a prompt. `permission-resolved` counts user answers, while coalescing,
owner-request sweeps, cancellations, and aborts retain distinct event or
`:settlement-source` values. Captured admission modes are separate from Eval's
`:eval-mode`. See [permission diagnostics](permissions.md) for event semantics
and the richer resource-scope fields restricted to `permission-log.el`.

Telemetry may be disabled with `mevedel-telemetry-enabled`. Events emitted
before a new session has a directory are held in the session and flushed as
soon as it is materialized. Persistence failures warn but never fail the user
workflow. Local session events are appended synchronously as UTF-8, one write
per event; the session directory is created only when an append finds it
missing.

## Data policy

Telemetry records lifecycle metadata, sizes, classifications, hashes, and
bounded identifiers. The emitter keeps only the keys named in
`mevedel-telemetry--allowed-keys` and drops everything else, at every depth: a
nested property list is filtered by the same rule as the event's own
properties. This is a structural filter, not content classification: a string
under an allowed key is bounded but is not inspected for secrets or payload
text. Callers must classify or hash payload-derived values before emission.
The names of dropped keys -- names only -- are
recorded on the event as `:dropped-keys`, so a caller whose property was
omitted can see that instead of nothing. Adding a property to telemetry means
adding its key to that list, and classifying or hashing anything derived from
a payload first. Shell commands are correlated by a SHA-256 hash; a Buddy
scope key is hashed for the same reason; Eask test paths are extracted only when they are repository-local
`test/*.el` names. Cache identity is a hash of the relevant parent environment,
not the environment values.

Memory-consolidation terminal events have one narrow target/query exception:
`:investigation` contains at most 32 call rows within 32 KiB of serialized
UTF-8, with an explicit omitted-row count. Dedicated `:investigation-root`,
`:investigation-path`, `:investigation-pattern` and `:investigation-command`
fields retain the scoped root, normalized relative target and search query
(or Git subcommand), each capped at 256 UTF-8 bytes. `:target-truncated` marks
shortened target fields. Rows retain admission-order call identity, tool name,
Read range, status, delivered byte count, result SHA-256, and partial/truncated
flags, never delivered content or error messages. `:investigation-notes` lists
at most 32 admitted note addresses of at most 256 UTF-8 bytes each, with
`:investigation-notes-omitted` counting oversized or excess addresses. The pass ID joins
these observations to accepted proposals; it does not attribute a call to any
particular note or establish what the model understood. This exception does not
allow arbitrary tool arguments, absolute source paths, shell commands, or
results in ordinary telemetry. It follows the normal enable switch, nil-field
omission and workspace rotation lifetime; accepted bundle and public review
schemas remain unchanged.

The ordinary hook, permission, execution, and repair logs remain available for
their subsystem-specific details. Telemetry connects their lifetimes through
shared session, request, tool-use, execution, interaction, agent, Goal, and
span identifiers.

An ephemeral `/btw` conversation has no telemetry file of its own. Its
allowlisted tool, permission, repair, sandbox, and managed-execution audit
events are redacted again, tagged with `:conversation-scope btw`, and recorded
through the durable parent session. Conversational events remain transient.
Side prompts, response prose, commands, tool arguments and results, paths,
permission profiles, and justifications are not forwarded.

## Detail tiers

Normal sessions keep tool-, request-, interaction-, execution-, agent-, Goal-,
and other outcome-level events. They omit routine per-pipeline-step spans,
per-hook-handler lifecycle spans, and hook-event spans with no matching
handlers. Hook events that actually run handlers and nontrivial repair
outcomes remain visible. Per-handler `hook-handler` outcomes repeat the
session's persisted hook log, so a durable session records them only during a
profiler run or with `mevedel-hooks-persist-log` disabled; a side
conversation, whose hook log is transient, still forwards them as its
value-free audit. A compaction attempt refused before admission is not
measured at all; one rejected after measurement starts closes its span as
rejected, so every recorded start has a finish.

Valid no-op input-validation and repair events are never recorded in any
tier, detailed profiler runs included: a validation that changed nothing and
raised nothing carries no information, and a profiled session emits hundreds
of them. Input validation therefore records one settled
`tool-input-validation-repair` event carrying its own `:duration-ms` instead
of a start/finish span, and only for outcomes with repairs, issues, or
errors.

An active `mevedel-telemetry-profiler-start` run records the full detailed
stream for its owning session, minus those no-op validation and repair
events. `mevedel-session-debug` starts that same profiler, so it also enables
full telemetry. Other concurrently live sessions remain on the normal tier.

## Covered lifecycle boundaries

Request settlement `:duration-ms` remains end-to-end wall-clock latency,
including user waits. Interaction events separately identify whether active
work was paused. The request-progress display and persisted request summary use
active elapsed time instead and exclude actionable user-input waits.

A `request-settled` event with `:outcome lost` records a degraded
settlement: the turn's request slot was emptied without its settlement
ever running -- a terminal transition lost with its process -- and a
later terminal transition on the same machine settled what it still
could.  Its request identity comes from the machine itself, and it
carries no duration or token facts.  The session reports `settling` and
rejects new admission until the degraded settlement chain completes; transport
cancellation releases the fence and leaves the machine retryable.

- Goal start, continuation dispatch, root-turn settlement, accounting, retries,
  and terminal status changes;
- request queueing, provider dispatch, first response, stream end, callback
  settlement, cancellation, and teardown;
- every tool pipeline step during profiler/debug runs, plus every permission
  queue transition, interaction lifetime,
  sandbox preparation/refusal, scheduler dwell, child start/first output/end,
  `WriteStdin` requested/effective wait, and result return;
- every ToolCall script as one redacted span with outcome, budget category,
  nested-call count, and duration, never script text, arguments, or results;
- aggregate hook events with matching handlers, plus every hook handler and
  empty aggregate event during profiler/debug runs, including handler identity,
  process outcome, contributed-context size, and acquisition/release of
  slow-hook status ownership;
- agent dispatch, provider send, first response, settlement, waits, and UI
  status ownership transitions;
- `mcp-reply-lost` when an MCP tool or hook reply finds its client closed,
  correlated with the owning turn and carrying only reply kind, request ID,
  and child agent path when applicable, never the reply payload;
- queued user messages with enqueue/dequeue events and dwell time;
- `event-wake-refused` when an idle root's background-event wake for waking
  mail is held, with `:reason` naming the first refusing gate: `busy`,
  `drain-blocked-queued` (queued follow-ups or another delivery hold),
  `prompt-hook`, `recovery`, `drain-blocked-latch` (the failure pause while
  steering is queued), `interaction`, or `composer-draft`. It is recorded
  only when the reason changes; an admitted wake resets it;
- `event-loop-lag` whenever the editor's event loop ran more than
  `mevedel-telemetry-lag-threshold` (0.5 s) late. Watching runs while a
  request runs and for two minutes after it settles, since journal,
  collection and publication work follows settlement. Admitted sessionless
  memory passes also watch from registration before preparation through terminal
  completion (including publication and automatic application), followed by the
  same two-minute tail. They reuse the process heartbeat, native worker or PGTK
  fallback timer, never a separate sampler. Their lag events and summary go to
  workspace diagnostics with `:pass-id` and `:workload memory`; no session is
  manufactured. One heartbeat batches workspace rows into one locked target
  program per workspace; quiet samples do not write. Diagnostic append time is
  excluded from native lag attribution so recording a stall cannot renew itself.
  Nested remote attribution checks a bounded set of supported wait/send dispatch
  frames; missing or truncated stacks conservatively retain diagnostic ownership.
  This remains synchronous I/O: independent responsiveness and CPU measurements
  must include that observer cost, rather than treating exclusion as a speedup.
  Overlapping session/pass windows can report the same process
  stall, so deduplicate before counting editor stalls. Waiting before admission
  is not watched by the pass. Disabled telemetry and noninteractive Emacs do not
  start pass watching. Every timer delivered
  meanwhile reports how late it ran, since a stall delays every timer due
  during it. A 100 ms heartbeat samples the rest and reports the worst delay
  since the previous heartbeat, once. Busy Lisp, redisplay or collection makes
  it late. On supported Linux builds, the optional native module supplies a
  timed condition wait: a worker releases the interpreter lock without
  dispatching process sentinels, timers or redisplay. Emacs owns the mutex,
  stack roots and thread cancellation. The module guards against GC hooks,
  advice and debugger callbacks consuming the timeout before the intended
  wait. Stopping sampling signals and joins the worker. If the module, timed
  wait or worker creation is unavailable, the shared main-thread timer
  samples instead, preserving process ownership: every 100 ms, or once a
  second while a PGTK frame exists, since stock PGTK redraws the whole frame
  on each wakeup. A timer callback also counts its own run time since the
  previous heartbeat, since it blocks input while it runs; a sub-second stall
  elsewhere reaches the whole-second heartbeat only by delaying a timer. Only
  PGTK Emacs loads the module for sampling; elsewhere a wakeup that changes
  nothing does not redraw. Native presentation and native sampling are
  separate capabilities; sampling does not require a graphical frame. A
  blocking remote operation can deliver
  timers while it waits, so the outermost one on the main thread counts as
  late as it took. Each callback
  retains its own timing attribution; the shared timer dispatcher does not
  replace callback names. Disabling telemetry stops the heartbeat at its next
  sample, since requests no longer report settling to close their windows.
  Time the previous heartbeat already saw is not counted again, and idle
  timers do not count. The event carries the delay, whether input arrived
  since the previous heartbeat, whether the request
  had settled, the running command's name, and the name and duration of
  the slowest timer callback since the previous heartbeat. `:gc-count` and
  `:gc-ms` report garbage collection anywhere since the previous heartbeat;
  `:timer-gc-count` and `:timer-gc-ms` the part inside that slowest callback,
  so a pause can be split into collection and callback work. `:cpu-ms` is
  the CPU time Emacs used since the previous heartbeat: close to the delay
  for a busy editor, near zero for a suspended machine or a blocking wait on
  a child process. A closure is
  labelled `closure:` plus the function it calls, preferring a `mevedel-`
  one: that names what it does, not where it was created. One that calls
  only primitives is named `anonymous`. Each settled request adds an `event-loop-lag-summary`
  with the counts above 200, 500 and 1000 ms, the maximum, and how many had
  input pending. Wait time the CPU profiler cannot see, such as a blocking
  remote command, shows up here;
- `session-save` (`:kind full` or `agent-registry`), `session-publication`
  (`:artifact-count`, `:input-bytes`), and `control-program` (`:kind`, the
  first operation's verb, and `:operation-count`) for synchronous
  persistence work. Each is one settled event recorded when the work returns,
  with `:duration-ms`, `:gc-count` and `:gc-ms` inside it, an estimated
  `:allocated-kb` from `memory-use-counts`, and `:outcome` (a returned
  symbol such as `queued`, else `ok`, or `error`). Allocation is reported
  apart from collection because persistence defers collection past its own
  extent, into whatever runs next. A save contains its publication and
  programs, so these durations nest. A target program has no session of its
  own: it is recorded only into the sessions the lag heartbeat is watching,
  and only when it blocked for 20 ms or more. Nothing is measured while a
  measurement is being recorded, since a remote session appends telemetry
  through the same programs;
- `publication-collection` when a collection job ends, with `:outcome`
  (`completed`, `proof-failed` when a deletion proof refused and nothing was
  deleted, or `failed`), candidate and retained head counts, deleted file and
  directory counts, and duration;
- `journal-capture-queued` when a checkpoint first becomes ready, carrying its
  capture identity, checkpoint trigger, and frozen input byte count; no evidence
  body is logged;
- `journal-digest-written`, `journal-digest-omitted` (nothing noteworthy), and
  `journal-digest-failed` in workspace diagnostics,
  with capture identity, trigger, attempt generation, publication/failure outcome,
  body byte count, and available provider uncached-input, cached-input, and
  output token counts. Input and cached counts are exclusive. Recovery
  can report publication without provider usage when those counts were not
  retained. Retries and failed publication remain diagnostic events; logs are
  not used as completion or review-coverage authority;
- `memory-note-cycle-observed` in workspace diagnostics after a preparation
  response, including an empty or skipped metadata-only opportunity. Its scalar
  fields describe the last observed durable scheduling snapshot:
  - `:note-cycle-id`: opaque cycle identity;
  - `:note-cycle-count`: total frozen cohort members;
  - `:note-cycle-attempted`: members authenticated as attempted by accepted passes;
  - `:note-cycle-deferred`: members deferred without an admitted attempt;
  - `:note-cycle-remaining`: frozen members not yet attempted or deferred;
  - `:note-cycle-status`: `active`, `complete`, or `paused`.
  The same fields accompany pass terminal events and results as the last
  observed durable snapshot, not increments to sum across passes. A successful
  pass can finish before its own admission receipt has been reconciled: only
  fresh owned reconciliation can credit that accepted pass. A following
  metadata-only admission can therefore report the final complete snapshot
  without another provider request. An observation event alone proves neither
  inference, an accepted review, nor any deletion. No cohort paths, member
  bodies or parser diagnostic text enter these fields;
- `memory-consolidation-fired`, `memory-consolidation-completed`,
  `memory-consolidation-failed`, and `memory-consolidation-killed` in workspace
  diagnostics. Each pass carries its identity, ownership generation, frozen
  mode, general/focused scope, and `memory` workload. Terminal events add duration,
  available provider usage, published proposal/review counts, consumed coverage,
  and the frozen eligible backlog remaining. Focused passes consume no general
  coverage; failed or cancelled passes do not fabricate reviewed notes or
  consumed digest coverage.
  Auto completion also records `updated-file-count`, the distinct paths newly
  confirmed written by that run, without exposing those paths or claiming that
  the resulting memory is better.
  `:note-format-failure-count` counts admitted notes whose missing, invalid or
  duplicate acknowledgements were recovered as no-ops, plus unadmitted note blocks
  skipped in that pass. A completed
  pass with a nonzero count is not a clean-format review; these failures must be
  counted alongside whole-review rejections when evaluating reply quality.
  Only the scalar count is emitted, never note IDs or parser diagnostic text.
  Notes have ID-only acknowledgements with deletion implied. Retention-guard, model-keep and
  preserved-condensation counters are not emitted; acknowledgement alone is
  not proof of application or useful service.
  The daily shared-note cycle and a pass are different units. One finite cycle
  can use several ordinary passes; the private cycle record tracks the frozen
  cohort, accepted-pass-authenticated attempts, deferred versions and scheduling
  status. Its scheduling metadata contains no note bodies and grants no write
  authority. An admitted note in an accepted pass can count as attempted even
  when its acknowledgement is missing or invalid, while that pass still reports
  a format failure and no reviewed/deleted credit for the note. Deferral is not
  an attempt, and an attempt is not extraction or confirmed deletion. A cycle
  continuing after a zero-delete batch is not evidence of applied progress.
  The former catch-up-progress observation and rolling batch-cap fields are
  removed, not emitted with zero values. Provider usage, request timing, format
  failures and confirmed application counts remain per-pass observations;
  cycle scheduling does not establish provider cost or throughput.
  Phase durations accumulate in memory and join only that existing terminal
  event; phase transitions append no additional workspace events.
  `:duration-ms` starts after claim acquisition. `:queue-duration-ms`, when
  present, measures parent-local admission dwell from the automatic opportunity
  being queued or the drain batch becoming ready until claim acquisition.
  Drain batches reset this boundary after each completed batch; dwell includes
  busy-session waits and admission checks, not the preceding batch's work.
  Direct starts have no queue duration. Phase fields use monotonic milliseconds:
  - `:preparation-duration-ms`: preparation dispatch through its callback,
    including worker startup, recovery, selection and capture (or the same
    synchronous fallback).
  - `:review-request-duration-ms`: review request invocation through settlement,
    including input preparation, investigation tools and chunk summaries. This
    is **not provider time**.
  - `:provider-duration-ms` and `:provider-call-count`: sum of observed native
    gptel provider-call finish durations and number of observed dispatch starts for this
    pass, including sequential note chunk summaries and the final review.
    Tool execution, prompt packing before dispatch, application and publication
    are excluded. Failed and aborted dispatches count, with their observed
    elapsed time retained; duplicate completion/late callbacks cannot add another
    finish. Cancellation during the start-event append can abort that span before
    transport registration; a start is not proof of a provider HTTP request.
    Totals are absent when no native call was observed, including
    telemetry-disabled runs and external ACP engines whose transport boundaries
    are not observed. They are wall time, not provider CPU or billing. The same
    totals are available in the pass result and review usage snapshot.
  - `:publication-duration-ms`: asynchronous publication dispatch through reply,
    or synchronous acceptance and publication.
  - `:application-duration-ms`: automatic application invocation through actual
    completion, including yielding, pending selection and owned recovery.
  - `:retirement-duration-ms`: the unsuccessful pass's existing durable
    settlement and prepared-evidence release block. Successful publication's
    retirement remains inside publication, not counted again here.
  - `:preparation-recovery-duration-ms`: decision and store recovery measured
    within preparation's process; the child returns a duration, never a clock
    timestamp. It nests inside preparation, not additive service time.
  - `:application-recovery-round-trip-ms`: sum of owned operation recovery
    dispatches through the editor continuation immediately before application
    or pending selection. This includes worker/preflight overhead, reply
    validation and synchronous recovery fallback, and nests inside application.
    Adjacent-claim continuity checks and read-only pending selection that omit
    history recovery contribute only to application duration, not this field;
    it is not the cost of every application readiness check.
    Only yielding owned-operation application supplies this field; fully
    synchronous application leaves it absent.
  Interrupted parent-local intervals end at cancellation/failure settlement;
  unavailable child recovery durations remain absent. Unentered phases are
  absent, not zero. These diagnostic boundaries are not an additive partition
  of total latency; setup and other coordinator work remain outside them.
  Late replies neither extend timing nor emit another terminal event.
  Terminal events and pass results retain `:output-bytes` (cumulative reply,
  reasoning, and normalized tool-call bytes), `:output-estimated-tokens` (the cumulative
  client estimate), and `:result-bytes` (current-round reply bytes, final at DONE),
  separately from provider token counts. See the [accounting limits](memory.md#request-limits-and-settlement)
  for raw and in-flight tool arguments excluded from these counters.
  `:reasoning-bytes`, `:reply-bytes`, and `:tool-call-bytes` break down accounted
  output into cumulative reasoning, replies across all rounds, and normalized
  executable-name/argument JSON. `:tool-call-count` counts admitted investigation
  calls; `:rounds` counts completed HTTP rounds independently of whether provider
  token metadata was reported. These are numeric diagnostics, not retained
  payloads. Budget failures carry
  `:failure-class output-limit`, `:budget-kind` (`output-bytes`,
  `output-estimated-tokens`, `output-tokens`, or `proposal-bytes`), and the numeric
  failing threshold in `:output-limit`; the latter two fields are nil when no
  budget failed. The review freezes its configurable cumulative limits at request
  start (64,000 tokens and 262,144 accounted bytes by default). Investigation
  permits 64 calls but still only 64 KiB of aggregate tool results, capped at
  8 KiB each. The deadline remains 180 seconds. The final proposal parser has
  an independent 32 KiB cap, diagnosed as `proposal-bytes` at DONE.
  Cancellation retains diagnostics already observed, and late callbacks do not
  emit another terminal event. Storage failures retain those diagnostics too,
  with failure class `publication`. Failure classes are categorical; focus text,
  evidence, proposal bodies, credentials,
  and arbitrary error messages are excluded. Unreported provider usage remains
  unknown, so these events do not establish a billing ceiling;
- compaction threshold inputs, hook work, segment-save stages, publication,
  and total duration, plus context-summary purpose, provider/model/effort,
  outcome, and token usage without raw evidence, focus data, or generated text;
- one terminal `session-naming` event per background title operation, with
  `:trigger` `automatic` or `retry`: `renamed` on
  success, otherwise `provider-error`, `aborted`, `invalid-response`, `timeout`,
  `cancelled`, or `error`, each with a categorical `:error-class`. `cancelled`
  covers an attempt the transport refused to schedule, and an attempt the user
  ends by renaming the session, retrying, or closing its root buffer records
  nothing. A positively unavailable provider has error class `unavailable`; a
  one-time fallback to the session's own provider stays inside the same event,
  marked `:fallback t`, whose outcome is the last provider's. Failures carry
  their `:stage` (`start`, `startup`, or `response`).
  Provider failures also carry `:provider-status` and, when the provider
  structured its error, `:provider-error-type` and `:provider-error-code`;
  provider message text is never recorded, so a refusal is attributable to its
  class and code but not quotable from telemetry;
- Agent summary preparation uses that same context-summary span; the parent
  handle stores only provider/model/effort metadata and never summary content;
- skill-roster advertisement and model/user skill invocation outcomes; and
- profiler environment snapshots, prompt failures, and saved artifacts.

## Reproducing a Goal run

Start from the root data or view buffer immediately before creating the Goal.
The session does not have to have hit disk yet: profiler start materializes a
cold session itself, so a run can begin without sending a throwaway turn
first.

1. Run `M-x mevedel-telemetry-profiler-start`. Combined CPU and memory
   profiling is the default. With a prefix argument, choose a single mode.
2. Create the Goal with the preset and objective being investigated. For a
   comparison, repeat the same interaction sequence, including any queued input,
   and avoid unrelated commands.
3. Let the Goal reach a terminal state or a clearly stranded state.
4. Run `M-x mevedel-telemetry-profiler-stop`.

For a session-level reproduction, `M-x mevedel-session-debug` starts the same
profiler while enabling gptel debug logging and the existing view-render trace.
Run it again to stop and save all three captures. The view trace includes
buffer point, selected-window point/start, composer-relative offsets, and
managed-fragment coordinates around interaction registration, full rerenders,
and zone reconciliation.

During `mevedel-session-debug`, `gptel--log` appends raw entries without
JSON pretty-printing. The debug log remains an opt-in raw-data artifact; ordinary
telemetry still follows the bounded metadata policy. See
[ADR 0118](adr/0118-keep-diagnostics-observational-and-bounded.md) for the
instrumentation tradeoffs.

From the repository checkout, `python3 scripts/analyze-gptel-log.py LOG_FILE`
analyzes raw or pretty-printed gptel captures. Add `--json` for structured events.
Requests, responses and streaming completions retain source line numbers and
appear in log order.

Each profiler run gets a directory containing:

```text
profiler-cpu-profile.el       native readable Emacs CPU profile, when enabled
profiler-cpu-report.txt       rendered CPU report, when enabled
profiler-memory-profile.el    native readable Emacs memory profile, when enabled
profiler-memory-report.txt    rendered memory report, when enabled
full-suite-time.txt           GNU time report, when a full Eask suite ran
gptel-debug.log               gptel log captured by mevedel-session-debug
view-render-debug.log         view trace captured by mevedel-session-debug
```

For a session saved locally, the directory is
`SESSION_DIR/diagnostics/run-TIMESTAMP-ID/`. For a remote session, it is a fresh
client-local directory under `temporary-file-directory`. Remote-session
profiler artifacts are not portable: another client resuming the session does
not receive them. `profiler-stopped` records the absolute client-side
`:artifacts-directory`, and the stop command prints it.

Current diagnostic limitation: `:artifacts-local` is false for remote sessions
even though their profiler artifacts are client-local. Use the explicit
directory and the storage rules above to locate the files; the flag does not
reliably describe artifact locality.

Native profile files hold `profiler-fixup-profile` output, which normalizes
sampled runtime objects before serialization.  Open them with
`M-x profiler-find-profile`.  A run is recorded as `profiler-stopped` only
after every expected profile and report exists and is nonempty; otherwise it
records `profiler-stop-failed` and signals the save error.

Both ends of a run are atomic about what they leave behind. Stop halts the
native profiler before it takes its closing environment snapshot, so a
snapshot that fails cannot leave Emacs profiling after the run has released
the handle for stopping it -- and the snapshot's own Git and hashing work
stays out of the profile it describes. If any part of start fails after the
native profiler is running, start stops it, removes the prompt guard, and
clears run ownership before re-signalling, so a reported failure to start
never leaves a run sampling in the background. A run raises
`profiler-max-stack-depth` globally, because the C log fixes its backtrace
width when profiling begins; whichever way the run ends restores the previous
value.

The two debug logs are explicit opt-in artifacts and may contain raw prompts,
responses, request headers, connection settings, and short rendered text
previews. Before persistence, mevedel replaces `Authorization`,
`ChatGPT-Account-Id`, and `Session-Id` header values in the gptel log with
`<redacted>`. Other diagnostic content remains raw. The logs are written with
owner-only permissions (`0600`); still treat the diagnostics directory as
sensitive session data.

While profiling is active, the first full Eask ERT suite is transparently run
under `/usr/bin/time -v` when GNU time is installed. Focused test files and
subsequent full-suite attempts are not wrapped. The corresponding execution
events identify the report and include scheduler dwell, overlap count, cache
identity, timeout state, and report size. Classification uses the original Bash
text. `mevedel-execution-telemetry.el` owns that recognition and prepends GNU
time directly to the already-tokenized argv; it does not add another shell
layer or inspect the live process record.

At profiler start and stop, telemetry records Git HEAD, dirty-file count,
status hash, an exact dirty-content hash (tracked diff plus untracked
file content hashes), loaded gptel artifact hash and checkout commit,
Emacs and system versions, configured sandbox mode, and Bubblewrap
availability. Library identity comes from Emacs's loaded-feature history, not
the first matching library on the current `load-path`. The hash and snapshot
byte count describe the loaded artifact's **current disk bytes**, including
bytecode when an `.elc` was loaded; they do not reconstruct bytes from load time.
For Git provenance only, an existing sibling `.el` of an `.elc` is preferred,
then symlinks are resolved before searching for a repository. This can identify
the source checkout behind a compiled build tree. Its commit is checkout
provenance, **not proof that the source and compiled artifact are equivalent**.
Without sibling source, Git lookup starts at the resolved artifact directory.
Unloaded, unknown, or missing loaded artifacts have unavailable identity and
provenance; another copy on `load-path` is never substituted. Missing Git or a
repository leaves the commit unavailable without losing readable artifact
identity. File contents are not written to telemetry, and neither are
arbitrary source paths. The explicit path exceptions are repository-local
Eask test names used for workload classification and `:artifacts-directory`,
which locates the client-side profiler output.

## Comparing session instrumentation modes

The maintained
[controlled session performance workload](https://github.com/FrauH0lle/mevedel/blob/master/benchmark/session-performance-workload.md)
exercises the native ApplyPatch tool, a child-agent permission request, retained
agent coordination, focused Bash tests, and an ignored-file-safe Elisp Xref
search.  It defines normal, profiler-only, and full-debug runs from equivalent
repository state.  Use profiler-only results for performance comparisons;
full-debug capture deliberately pays the cost of gptel request logging and the
view-render trace.

## Prompt-cache efficiency

Ordinary telemetry records `provider-call` events with `:stage start` and at
most one `:stage finish` per actual gptel provider dispatch. A `:call-id` identifies
the HTTP call, unlike `:request-id`, which can span a whole tool-loop turn.
Both events retain the dispatch-time session turn, request identity, agent
path (when applicable), backend name and concrete `:backend-type`, model,
and workload (`root`, `agent`, `naming`, or `context-summary`). Context
summaries also retain their categorical purpose. Helpers write into the owning
session; sessionless journal/memory work keeps its separate workspace stream.
Native memory review and its chunk-summary calls emit `provider-call` start/finish
events there with `:pass-id` and `:workload memory`, using the same dispatch,
parser, completion and abort boundaries and normalized usage fields. Chunk
summaries additionally retain their summary `:purpose`. Correlation is internal
request context, not model input, and never emits the workspace object or payload.
Transient `/btw` conversational calls do not create per-call events.
Only gptel dispatch is observed. Claude Code requests run through ACP and emit
no `provider-call` events: root and child turns, and isolated text and workload
requests such as naming, context summaries, permission review, buddy and
memory review, are absent from prompt-cache analysis. Root and child turn usage
still reaches Goal accounting.

Normal completion observes gptel's per-call `:tokens` at streaming transport
cleanup or non-streaming response parsing, before callbacks can tear down a
buffer, run tools, or dispatch another call. Transport completion metadata
keeps an older cleanup's error from contaminating a replacement call. Streaming
parsing retains only a boolean for provider errors, with no per-chunk events.
Abort callbacks and explicit `ABRT` transitions cover cancellation, including
retained-agent interruption with a muted callback. It never uses cumulative
`:tokens-full` for per-call accounting. Successful tool-only responses that
have no text callback also settle before tool or terminal handling. Finish adds `:outcome`
(`success`, `error`, or `aborted`) and monotonic `:duration-ms`, which ends
before subsequent tool execution and user waits. An interrupted process may
leave an unmatched start. A failed/aborted call may retain partial usage; it
does not imply a zero bill.

Token fields have one normalized meaning across providers:

| Field | Meaning |
| --- | --- |
| `:input-tokens` | Input not read from cache, including cache-write tokens under gptel's normalized accounting |
| `:cached-tokens` | Input read from cache |
| `:cache-write-tokens` | Cache-write subset of input, when reported; do not add it to input again |
| `:output-tokens` | Provider-reported output |

Counts are nonnegative integers or nil (unknown). `:usage-status` is `complete`
when input, cached input, and output are known, `partial` when only some are
known, `missing` when none are known, and `invalid` when a reported value is
invalid. Cache writes are optional, so their absence does not make otherwise
complete usage partial. Invalid counts are retained as unknown. gptel can
normalize omitted provider fields to zero; telemetry cannot reconstruct that
lost distinction. Token-baseline events normalize nested `:provider-context-usage`
and `:cumulative-usage` into these same allowlisted names without broadening
the privacy filter to generic `:input`/`:output` keys.

Analyze logs from a repository checkout, without loading mevedel or making
provider calls:

```bash
# Text summary of every session with a log in this workspace.
emacs -Q --batch -l scripts/analyze-session-cache.el \
  -f mevedel-analyze-session-cache-main -- .mevedel/sessions

# Structured report for explicit logs or session directories.
emacs -Q --batch -l scripts/analyze-session-cache.el \
  -f mevedel-analyze-session-cache-main -- --json /path/to/session/telemetry-log.el
```

The analyzer accepts telemetry files, session directories, or sessions
directories. It reads Lisp data without evaluating it and validates records.
It reports source files and line numbers, duplicate/conflicting records,
malformed lines, incomplete calls, outcomes, usage coverage, and breakdowns
by session, backend/model, and workload. Chronological call details let you
locate reuse drops without recording prompt content.

The token-weighted cached-input share is
`sum(cached) / sum(input + cached)` over calls with both fields known. It is
not the average of call percentages. The cache-positive call fraction uses
calls with known cached-input usage; coverage is reported separately. Zero
denominators are unavailable, not 0%. Field totals sum known values and cannot
establish complete usage when coverage is partial.

Only per-call events enter these totals. Existing `request-settled` events
remain cumulative lifecycle summaries and are never added. Older logs have
insufficient per-call evidence; mixed logs disclose limited coverage rather
than backfilling call counts from settlements. Historical logs are not changed.

Observed reuse is not theoretical cacheability, monetary savings, or evidence
that caching caused a latency improvement. Cold starts, compaction, model or
schema changes, routing, and provider retention can legitimately reduce reuse.
The report locates drops; it does not automatically attribute their cause.

## Prompt guard

Profiler runs temporarily advise `ask-user-about-supersession-threat`,
`yes-or-no-p`, and `y-or-n-p`. Each invocation records the function, prompt
length, and prompt hash. By default,
`mevedel-telemetry-profiler-fail-on-prompt` then raises a `user-error`, making
an unexpected compaction, file-conflict, or edit question a visible failed
reproduction rather than unclassified user-wait time. Set the option to nil
only when the reproduction intentionally includes synchronous prompts.

## Reading the stream

The file can be read incrementally with ordinary Lisp `read`:

```elisp
(with-temp-buffer
  (insert-file-contents "/path/to/session/telemetry-log.el")
  (let (events)
    (condition-case nil
        (while t (push (read (current-buffer)) events))
      (end-of-file))
    (nreverse events)))
```

Build the critical path from paired `:stage start` and `:stage finish` entries
sharing `:span-id`, then use queue dwell events and provider, child-process,
interaction, hook, and compaction spans to classify every interval longer than
five seconds. `status-transition` events identify the subsystem that owned a
long-lived spinner independently of whether background work was still live.
