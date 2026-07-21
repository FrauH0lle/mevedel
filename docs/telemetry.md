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
bounded identifiers. The emitter rejects fields that can contain raw prompts,
commands, tool arguments or results, hook/process output, expressions,
justifications, and environment contents. Shell commands are correlated by a
SHA-256 hash; Eask test paths are extracted only when they are repository-local
`test/*.el` names. Cache identity is a hash of the relevant parent environment,
not the environment values.

The ordinary hook, permission, execution, and repair logs remain available for
their subsystem-specific details. Telemetry connects their lifetimes through
shared session, request, tool-use, execution, interaction, agent, Goal, and
span identifiers.

## Covered lifecycle boundaries

The event stream covers:

- Goal start, phase dispatch and settlement, guardian calls, review verdict
  persistence, cycle changes, retries, and terminal settlement;
- request queueing, provider dispatch, first response, stream end, callback
  settlement, cancellation, and teardown;
- every tool pipeline step, permission queue transition, interaction lifetime,
  sandbox preparation/fallback, scheduler dwell, child start/first output/end,
  and result return;
- every hook handler and aggregate hook event, including handler identity,
  process outcome, contributed-context size, and acquisition/release of
  slow-hook status ownership;
- agent dispatch, provider send, first response, settlement, waits, and UI
  status ownership transitions;
- queued user messages with enqueue/dequeue Goal phases and dwell time;
- compaction threshold inputs, provider summary work, hook work, segment-save
  stages, publication, and total duration;
- skill-roster advertisement and model/user skill invocation outcomes; and
- profiler environment snapshots, prompt failures, and saved artifacts.

## Reproducing a Goal run

Start from the materialized root data or view buffer immediately before
creating the Goal:

1. Run `M-x mevedel-telemetry-profiler-start`. CPU profiling is the default.
   With a prefix argument, choose `mem` or `cpu+mem`.
2. Create the Goal with the same preset, objective, and interaction sequence.
   Queue the same deliberate mid-implementation message and avoid unrelated
   commands.
3. Let the Goal reach a terminal state or a clearly stranded state.
4. Run `M-x mevedel-telemetry-profiler-stop`.

Each profiler run gets
`SESSION_DIR/diagnostics/run-TIMESTAMP-ID/`, containing:

```text
profiler-cpu-profile.el       native readable Emacs CPU profile, when enabled
profiler-cpu-report.txt       rendered CPU report, when enabled
profiler-memory-profile.el    native readable Emacs memory profile, when enabled
profiler-memory-report.txt    rendered memory report, when enabled
full-suite-time.txt           GNU time report, when a full Eask suite ran
```

Native profile files are written through `profiler-write-profile`, which
normalizes sampled runtime objects before serialization.  Open them with
`M-x profiler-find-profile`.  A run is recorded as `profiler-stopped` only
after every expected profile and report exists and is nonempty; otherwise it
records `profiler-stop-failed` and signals the save error.

While profiling is active, the first full Eask ERT suite is transparently run
under `/usr/bin/time -v` when GNU time is installed. Focused test files and
subsequent full-suite attempts are not wrapped. The corresponding execution
events identify the report and include scheduler dwell, overlap count, cache
identity, timeout state, and report size.

At profiler start and stop, telemetry records Git HEAD, dirty-file count,
status hash, an exact dirty-content hash (tracked diff plus untracked
file content hashes), loaded gptel and gptel-agent file hashes and repository
commits, Emacs and system versions, configured sandbox mode, and Bubblewrap
availability. Paths and file contents are not written to telemetry.

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
