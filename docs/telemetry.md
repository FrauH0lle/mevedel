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
session and turn, and any current preset and Goal identity.

Sessionless background work uses the same envelope and privacy filter at
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
outcomes remain visible. A compaction attempt refused before admission is not
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
- queued user messages with enqueue/dequeue events and dwell time;
- `event-loop-lag` whenever the editor's event loop ran more than
  `mevedel-telemetry-lag-threshold` (0.5 s) late. A 100 ms heartbeat takes
  the measurement while a request runs and for two minutes after it settles,
  since journal, collection and publication work follows settlement. The
  event carries the delay, whether input was pending, whether the request
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
- `memory-consolidation-fired`, `memory-consolidation-completed`,
  `memory-consolidation-failed`, and `memory-consolidation-killed` in workspace
  diagnostics. Each pass carries its identity, ownership generation, frozen
  mode, general/focused scope, and `memory` workload. Terminal events add duration,
  available provider usage, published proposal/review counts, consumed coverage,
  and the frozen eligible backlog remaining. Focused passes consume no general
  coverage; failed or cancelled passes leave the selected batch in the backlog.
  Auto completion also records `updated-file-count`, the distinct paths newly
  confirmed written by that run, without exposing those paths or claiming that
  the resulting memory is better.
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
- one terminal `session-naming` event per background title attempt: `renamed` on
  success, otherwise `provider-error`, `aborted`, `invalid-response`, `timeout`,
  `cancelled`, or `error`, each with a categorical `:error-class`. `cancelled`
  covers an attempt the transport refused to schedule, and an attempt the user
  ends by renaming the session or closing its root buffer records nothing.
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
