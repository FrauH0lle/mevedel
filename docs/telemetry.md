# Session Telemetry and Profiler Runs

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

On Linux, elapsed milliseconds and span durations use the kernel monotonic
clock exposed by `/proc/uptime`. Other systems fall back to process-relative
wall time and clamp emitted elapsed values so they never move backwards. Use
`:duration-ms` for latency analysis and `:time` for correlation with external
logs.

Telemetry may be disabled with `mevedel-telemetry-enabled`. Events emitted
before a new session has a directory are held in the session and flushed as
soon as it is materialized. Persistence failures warn but never fail the user
workflow.

## Data policy

Telemetry records lifecycle metadata, sizes, classifications, hashes, and
bounded identifiers. The emitter keeps only the keys named in
`mevedel-telemetry--allowed-keys` and drops everything else, at every depth: a
nested property list is filtered by the same rule as the event's own
properties, so an aggregate field cannot carry a prompt, a command, a path, or
a tool result past the boundary. The list is an allowlist because a denylist
has to name every field that might leak, which makes silence the default for
any field a caller invents. The names of dropped keys -- names only -- are
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
per-hook-handler lifecycle spans, hook-event spans with no matching handlers,
valid input-validation spans, and no-op valid repair events. Hook events that
actually run handlers and nontrivial repair outcomes remain visible.

An active `mevedel-telemetry-profiler-start` run records the full detailed
stream for its owning session. `mevedel-session-debug` starts that same
profiler, so it also enables full telemetry. Other concurrently live sessions
remain on the normal tier.

## Covered lifecycle boundaries

The event stream covers:

Request settlement `:duration-ms` remains end-to-end wall-clock latency,
including user waits. Interaction events separately identify whether active
work was paused. The request-progress display and persisted request summary use
active elapsed time instead and exclude actionable user-input waits.

- Goal start, continuation dispatch, root-turn settlement, accounting, retries,
  and terminal status changes;
- request queueing, provider dispatch, first response, stream end, callback
  settlement, cancellation, and teardown;
- every tool pipeline step during profiler/debug runs, plus every permission
  queue transition, interaction lifetime,
  sandbox preparation/fallback, scheduler dwell, child start/first output/end,
  `WriteStdin` requested/effective wait, and result return;
- aggregate hook events with matching handlers, plus every hook handler and
  empty aggregate event during profiler/debug runs, including handler identity,
  process outcome, contributed-context size, and acquisition/release of
  slow-hook status ownership;
- agent dispatch, provider send, first response, settlement, waits, and UI
  status ownership transitions;
- queued user messages with enqueue/dequeue events and dwell time;
- compaction threshold inputs, hook work, segment-save stages, publication,
  and total duration, plus context-summary purpose, provider/model/effort,
  outcome, and token usage without raw evidence, focus data, or generated text;
- Agent summary preparation uses that same context-summary span; the parent
  handle stores only provider/model/effort metadata and never summary content;
- skill-roster advertisement and model/user skill invocation outcomes; and
- profiler environment snapshots, prompt failures, and saved artifacts.

## Reproducing a Goal run

Start from the materialized root data or view buffer immediately before
creating the Goal:

1. Run `M-x mevedel-telemetry-profiler-start`. Combined CPU and memory
   profiling is the default. With a prefix argument, choose a single mode.
2. Create the Goal with the same preset, objective, and interaction sequence.
   Queue the same deliberate mid-implementation message and avoid unrelated
   commands.
3. Let the Goal reach a terminal state or a clearly stranded state.
4. Run `M-x mevedel-telemetry-profiler-stop`.

For a session-level reproduction, `M-x mevedel-session-debug` starts the same
profiler while enabling gptel debug logging and the existing view-render trace.
Run it again to stop and save all three captures. The view trace includes
buffer point, selected-window point/start, composer-relative offsets, and
managed-fragment coordinates around interaction registration, full rerenders,
and zone reconciliation.

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

For a session saved locally that directory is
`SESSION_DIR/diagnostics/run-TIMESTAMP-ID/`. For a session saved on a target it
is a fresh local temporary directory instead, created per run under
`temporary-file-directory`. A profile measures the client Emacs, and the `ssh`
method has no out-of-band copy at any size, so writing 8 MB of profile to the
target means 8 MB of base64 through the shell for an artifact no resume
consults. The cost is the reason; the consequence is that diagnostics for a
remote session are **not portable** — another client resuming it finds no
`diagnostics/` for a run profiled elsewhere. The `profiler-stopped` event
therefore records `:artifacts-directory` as an absolute client-side path plus
`:artifacts-local`, and `M-x mevedel-telemetry-profiler-stop` prints the
directory, which is the only way to find a remote-session run.

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
file content hashes), loaded gptel and gptel-agent file hashes and repository
commits, Emacs and system versions, configured sandbox mode, and Bubblewrap
availability. File contents are not written to telemetry, and neither are
paths, with one exception: `:artifacts-directory` records the absolute
client-side directory holding the profiler artifacts, because a reader who
cannot find those files cannot use the run.

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
