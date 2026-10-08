# Remaining editor wakeups: results and review handoff

This follow-up was measured as `219b4974` on `fix/cpu-wakeups`, directly on
`9b6a6f5e`. It was then replayed onto the review fixes on `review/cpu-wakeups`;
the source hashes and the 9,789-case suite below describe `219b4974`. The
combined tree passed 9,797 cases with zero unexpected results and measured
8.25% and 9.92% for the running-tool workload (see "Combined tree" below).
A real request waiting for a managed Bash process now uses **8.58–10.08% editor
CPU**, compared with **18.17–19.33%** at the starting revision: about **50% lower
on average**. Native status animation remains at 30 fps. These improvements are
entirely in mevedel's Lisp; the native renderer is unchanged.

The full isolated suite passed: **9,789 cases, zero unexpected results,
31 conditional skips**. All 242 source files compiled without warnings.
All 19 graphical checks passed, including draft preservation and native fallback.
Request completion and separate background execution completion release their
respective timers and native surfaces. Foreground measurements are finished.

## Final measurements

CPU is process user+system time divided by wall time; 100% means one core.
These are Emacs 31.1 PGTK/Wayland results on KDE, at 1545x864 logical pixels and
2x scale, using compiled mevedel and a local mock provider. The tool is a real
managed sleeping Bash process, not synthetic tool events. Samples cover the
quiet running interval, not request startup, streaming or total request cost.
Each sample follows a three-second settling period. Both native surfaces remain
active; focus/activity is checked at both boundaries and a focus-loss counter
covers the entire sample. Every final sample recorded zero focus losses.

| Revision / workload | Editor CPU samples | Compositor CPU samples | Sample length |
| --- | --- | --- | --- |
| Original `9b6a6f5e`, running Bash | 18.17%, 19.33% | 17.08%, 16.58% | 12 s |
| Completed implementation, running Bash | 8.58%, 10.08%, 9.58% | 14.25%, 14.83%, 14.92% | 12 s |
| Completed implementation, acceptance workload | 9.00% | 13.75% | 8 s |

The ordinary-tool means are 18.75% and 9.41%, a 49.8% reduction in **editor**
CPU. Compositor CPU remains substantial; this is not a 50% whole-machine or
battery-life claim. The provisional `--max-editor-cpu 10` gate exited 1 because
one final sample reached 10.08%. That strict gate did **not** pass. The user's
objective was substantial reduction, not a fixed cutoff; no threshold was
relaxed to manufacture success.

The separate provider-waiting workload measured 11.00%, 12.00% before and
7.17%, 8.17% after coalescing and layout changes. Those latter samples predate
the final tool-source recovery guard, which was not exercised by that workload.
Running-tool samples at that intermediate revision were 11.75%, 13.17%.
Files named `final-after-*` preserve these intermediate results; the completed
implementation is recorded in `completed-tool` and `completed-acceptance`.

## What changed and why

Local Emacs source inspection shows `pgtk_frame_up_to_date` queues a whole-widget
GTK draw after redisplay when buffer flipping is allowed. Cheap callbacks can
therefore have expensive downstream effects. The bounded callback/redisplay
trace in `results/align-trace-focused-timeline.el` showed that aligning deadlines
alone still permitted redisplay between callbacks.

1. **Share eligible callbacks.** `mevedel-utilities.el` owns a queue of real timer
   handles with one host timer dispatching due callbacks. GC maintenance, the
   process exit watchdog, telemetry heartbeat, quiet progress and metadata-only
   view updates use this queue. Existing UI timer cancellation handles ownership,
   including TRAMP's dynamically bound timer lists. Repeating observations skip
   missed ticks. Callback errors are isolated, telemetry attributes individual
   callbacks, and the dispatcher yields between callbacks for input or after
   25 ms. An individual callback can still take longer than that limit.
2. **Preserve meaningful cadence.** Quiet progress moves to whole-second ticks
   at least 250 ms after the preceding event (a 0.25–1.25 s transition). New
   process output can still hasten progress, bounded at four updates per second.
   Initial delay and process settlement behavior are unchanged. Ordinary Lisp
   decorative frames retain their own scheduler; native animation stays smooth.
3. **Avoid idle layout work when already current.** The window-change hook checks
   image width and visible stale tables before scheduling its idle pass. The
   callback checks again because queued work may become stale. Real resize and
   newly visible stale tables still schedule layout.
4. **Recover rows only when source exists.** Execution progress previously armed
   an incremental recovery render every second while its source row was absent.
   The new guard checks authoritative gptel source bounds before scheduling.
   Progress still enters its cache immediately; normal stream/tool-boundary
   projection handles source arrival. Nested tool IDs search their source-owning
   ancestor, and lookup widens without changing the caller's restriction.

No callback behavior was deleted to obtain the final figures. Diagnostic
ablations below deliberately remove work and are not the production solution.

## Validation and lifecycle

- The final full suite completed in 240.32 seconds through the isolated Eask
  runner: 9,789 cases, zero unexpected results, 31 skips. Local detailed report:
  `.scratch/test-suite-performance/20261008-141727/summary.json`.
- All 242 files compiled without warnings. Local log:
  `.scratch/wakeup-lab/final-compile.log`. Bounded validation results are retained
  in `results/validation.json`.
- Focused regressions failed before the relevant fixes: unnecessary Markdown
  timer scheduling, shared callback lifecycle, and source-less row recovery.
  The final integration subset passed 302 cases before the final complete suite.
- Nineteen graphical checks cover freeze/resume, ordinary fallback, tool styles,
  theme/font changes, split windows, clipping, typing and a multiline draft.
  See `results/completed-acceptance-acceptance.json`.
- Every ordinary final request ends with zero executions, native surfaces and
  tool indicators; only the intended telemetry/GC tail callbacks remain.
- The acceptance Bash command lasts beyond the request's 30-second yield.
  At request settlement its execution is still live, so progress/watchdog
  callbacks correctly remain. The runner separately waits for that execution
  to finish and verifies both callbacks disappear. See the `stopped` and
  `execution_stopped` snapshots in `results/completed-acceptance.json`.

The first full-suite attempt exposed a test race: the remote stdin readiness
case checked a hook log immediately after process death, before deferred output
settlement. The hooks suite passed unchanged on rerun. Its test now awaits both
process death and the recorded result, within the same deadline. Product
settlement was not changed. A focused suite also exposed a missing explicit
`mevedel-reminders` require in the native-context test fixture; that fixture now
loads its own dependency. Both fixes are covered by the final full suite.

## Measurement correction and reproducibility

The old mock provider reused `call_mock_0` across HTTP requests. A repeated
request could find the preceding tool's source-backed row and avoid the missing
source behavior of an ordinary fresh tool. Earlier second-request figures
therefore describe that flawed fixture. First-request samples have no such
collision. The mock now generates fresh response and tool-call IDs per request.
**All final old/new comparisons use the corrected mock.** Earlier evidence is
preserved with this limitation rather than relabelled.

The baseline is an isolated `git archive 9b6a6f5e` checkout at
`.scratch/wakeup-lab/before-9b6a6f5e`, compiled through Eask with frozen copies of
the same dependencies. `results/dependencies.json` records byte-for-byte matching
compiled gptel libraries. Results record runtime library paths, product hashes,
harness hashes and unique-ID fixture use. This establishes comparison parity,
not equivalence to the user's configured live Emacs. No provider account or
live user session was used.

From this worktree, with compiled current sources and the prepared baseline:

```sh
python3 .mevedel/shared/editor-cpu/renderer-lab/request-run.py \
  --source-root .scratch/wakeup-lab/before-9b6a6f5e \
  --seconds 12 --tool --modes native native --focus-kwin \
  --output .scratch/wakeup-lab/reproduce-before

python3 .mevedel/shared/editor-cpu/renderer-lab/request-run.py \
  --seconds 12 --tool --modes native native native --focus-kwin \
  --max-editor-cpu 10 --output .scratch/wakeup-lab/reproduce-after

python3 .mevedel/shared/editor-cpu/renderer-lab/request-run.py \
  --seconds 8 --tool --modes native --focus-kwin --acceptance \
  --output .scratch/wakeup-lab/reproduce-acceptance
```

The optional KWin activation script targets only the disposable editor's PID and
is unloaded immediately after activation. Keep that editor focused; parallel
measurement editors previously stole focus and invalidated runs. CPU experiments
must run serially. The strict 10% gate may fail as it did in the recorded run.
The runner records results before reporting that failure.

## Exploratory attribution

These eight-second samples predate the corrected mock and final implementation.
Their repeated-request columns are affected by ID reuse as described above.
They guided hypotheses; use the final table for the performance conclusion.
Percentages are not additive because scheduling affects separate redisplays.

| Diagnostic | First request CPU | Repeated request CPU |
| --- | ---: | ---: |
| Initial baseline | 18.25% | 20.37% |
| Align periodic deadlines | 13.50% | 16.00% |
| Suppress telemetry, GC maintenance, watchdog, collection | 8.62% | 11.50% |
| Suppress GC maintenance | 14.37% | 17.50% |
| Suppress watchdog | 14.50% | 17.12% |
| Suppress progress | 14.00% | 16.87% |
| Suppress Markdown layout checks | 17.50% | 16.87% |
| Layout fix alone, exclusive foreground | 17.62% | 18.37% |
| Layout fix + shared housekeeping prototype | 13.37% | 14.50% |
| Layout fix + shared housekeeping/progress/metadata prototype | 11.12% | 9.75% |

Suppressing telemetry alone produced first-request values of 12.0% and 13.75%,
but focus loss invalidated their repeated requests; those incomplete comparisons
were excluded. Under the corrected fixture, a later diagnostic suppressing
missing-row recovery measured 8.58%, motivating the narrower production source
check. `probe.el` is disposable diagnostic code and must not be installed;
its timer overrides belong with the original compiled baseline, not layered
over the completed production scheduler.

## Remaining limits

The useful 2-Hz telemetry heartbeat still wakes Lisp, and Emacs PGTK still pays
parent presentation costs. Real output, streaming, editing, larger transcripts,
other frame sizes and other display backends can change the figures materially.
The work does not promise all requests stay below 10%. Native animation already
avoids per-frame Lisp wakeups; further reductions should be evaluated against
lag detection, progress latency and Emacs's presentation behavior.

## Combined tree (after replay onto the review fixes)

Disposable editor, same machine, `request-run.py --focus-kwin`, single runs:

| Workload | Static label | Native 30-fps bounce |
| --- | ---: | ---: |
| Running sleeping Bash (12 s) | — | 8.25%, 9.92% |
| Silent request (8 s) | 5.75% | 7.75% |
| Prose streaming at 8 words/s (8 s) | 35.62% | 41.50% |

Before this follow-up, with the review fixes, the silent request measured
7.75% / 12.0% and streaming 38.0% / 44.25%
(`renderer-lab/results/streaming/`).
