# Spinner completed-run analysis and telemetry — 2026-09-28

Task: analyze the completed spinner Goal's logs without changing source or
session state. Parent synthesis and telemetry analysis, with separate detailed
CPU-profile and validation reports. Session directory (**S** below):
`.mevedel/sessions/2026-09-27T16-24-5ca69305960c/`.

## Integrated findings and priorities

1. **The large collapsed timer entry is misleading as an optimization target.**
   The largest exact retained primary stack goes through C-source lookup into
   a directory-completion prompt: **51.22% of displayed counts**. That matches
   the known stalled tool, already fixed and recovered. The recursive prompt
   also hosted other editor activity; these counts do not prove continuous
   blocking lookup computation. Reconstructed timer ancestry is heuristic.
2. **The best spinner-specific lead is repeated view-state/anchor scanning.**
   Raw spinner-inclusive counts are **5.03%** of the primary displayed total;
   **60.70% of those** include anchor lookup, and **40.97%** end in property-change
   scanning or `min`. The early capture independently shows this same cost
   family. This is evidence about captured code, not a regression claim about
   the final implementation. If further performance work is authorized, first
   check whether the final path still incurs these scans under representative
   transcript sizes; do not optimize blindly from this historical aggregate.
3. **Separate CPU share from tail latency.** Rendering flushes and execution
   settlement callbacks have concerning hundreds-of-milliseconds observed
   durations, detailed below. Rendering can matter to responsiveness without
   dominating aggregate CPU. Conversely, the longest heartbeat gaps came from
   suspend and diagnostic recovery, not demonstrated animation freezes.
4. **Completion is supported, but the repository is not all green.** The final
   focused roster has **1,088 expected / zero unexpected / one skip**;
   compilation covers 210 files without warnings. The full suite has **eight
   unexpected failures / 30 skips in 8,760 tests**. Baseline comparisons support
   separating those failures from animation, not calling them passes. Physical
   battery-policy operation was verified; battery-life savings were not.
5. **Verification workflow is another actionable cost.** Isolate cleanup/build
   caches from concurrent test workers and group boundary tests by lifecycle,
   projection and visibility invariants. Keep required final full-suite checks.
   This addresses observed interference and repeated discovery without weakening
   the acceptance criteria.

CPU percentages use **21,255,047 displayed counts**, not all 32,835,081 native
counts: the renderer omits 11,580,034 empty-stack counts. Units are recorded
interrupt/sample counts, not milliseconds. GC is 10.20% and discarded counts
10.29% of the displayed total; inclusive redisplay is 12.98%. These categories
overlap where applicable and must not be summed. Discarded, empty and truncated
stacks limit attribution. No memory profile was collected.

Detailed supporting reports:
- `work://shared/spinner-cpu-analysis-20260928.md` — native format, denominators,
  expanded timer tree, raw stack counts and reconstruction limitations.
- `work://shared/spinner-validation-analysis-20260928.md` — final artifacts,
  baseline comparisons, native-focus/battery limitations and verification churn.

The parent independently recomputed both native denominators, spinner/anchor
counts and the largest exact primary stack from the decoded native data; checked
the profiler C counter implementation; recomputed lag counts and named timer
statistics from all exported records; and directly checked the final roster and
full-suite summary. Agent PASS verdicts concern analysis completeness, not a new
green product test run. Profiling remains stopped as requested. A controlled
final-version comparison would be a separate future experiment, not something
this capture can establish or that was started during this analysis.

## Reconciled state and method

- The persisted Goal is complete, updated **2026-09-28 18:58:12 +0200**.
  It began at 17:41:16 on September 27: about 25 hours 17 minutes elapsed.
  The sidecar records 34 Goal turns and current segment 20. These are lifetime
  counters, not CPU measurements.
- Live inspection at 21:00 found neither CPU nor memory profiling active,
  no profiler owner, and no live buffers for this session. Process inspection
  found the host Emacs, not leftover Eask/Emacs test subprocesses.
- Read all **200,248** records in `S/telemetry-log.el`, **110,711,255 bytes**.
  A separate `emacs --batch -Q` read them as data and exported JSONL; no session
  records were evaluated or mutated. Original line count equals parsed record
  count, so the `source-record` values below also identify original log lines.
- Disposable scripts and aggregates are in
  `.scratch/spinner-log-analysis-20260928/`: `export.el`, `analyze.py`,
  `details.py`, `summary.json`, and `details.json`. The provisional provider
  latency pairing in `details.py` encountered overlapping same-request events;
  its provider-duration aggregates are **not accepted findings** and are not
  used here. Tool-ID pairing and scalar event counts are independent of that
  failed correlation assumption.
- Percentiles below describe recorded events, not every heartbeat or frame.
  Nested asynchronous durations must not be added together as CPU time.

## Capture boundary

Two CPU-only captures exist:

| Run | Started | Stop recorded |
|---|---|---|
| `run-20260927T174115-f917e96e` | Sep 27 17:41:16 | Sep 27 18:03:12 |
| `run-20260927T181246-1b68ad79` | Sep 27 18:12:46 | Sep 28 09:35:45 |

The second run contains **138,549** tagged telemetry records and spans about
15 hours 23 minutes. Its stop event follows artifact saving; native profiling
stops earlier in that command. The final Goal completion is about **9 hours
22 minutes after capture stopped**. This is an investigation capture across
changing code, concurrent work, interruptions, and repairs, not a benchmark of
the final implementation.

## Long runtime: multiple distinct causes

### An actual tool stall

Request `request-20260928T003432-79b9de8daf0c` settled as aborted at
07:03:35 with recorded duration **23,342,955 ms** (6 h 29 m).
Its outer ToolCall span lasted **23,005,510 ms** (6 h 23 m), after the nested
`function_source` call began at 00:39:47 (`telemetry-log.el:99288`). The nested
call has no `tool-finished` event. This matches the directly diagnosed
interactive C-source prompt and normal abort/resume recovery described in
`introspection-source-recovery-20260928.md`, fixed by `1df976e9`.

The other unmatched `tool-received` is the ToolCall at 21:05:27 on September 27
(`telemetry-log.el:52212`), consistent with the separately investigated pipeline
stack incident. Neither missing completion is evidence that work is still
running now; these are incomplete historical telemetry spans.

Eight root requests settled with `outcome=error` and `provider-status="Curl
failure"`; one was aborted during recovery and 32 settled successfully.
An error request's entire duration is not network downtime: it includes work
before failure. Provider failures, tool stalls, and computational latency must
not be conflated.

### System sleep, not a 38-minute editor freeze

The worst primary-capture heartbeat delay is **2,315,015 ms**, recorded at
20:39:20 (`telemetry-log.el:41933`). System journals independently show:

- Sep 27 20:00:45: lid closed; user processes frozen; system suspended.
- Sep 27 20:39:20: lid opened and system resumed.
- Sep 28 06:27:34–06:30:59: another suspend/resume, matching the **205,490 ms**
  delay at `telemetry-log.el:99363`.
- Sep 28 06:31:30–06:31:33: a further brief suspension near the 3,950-ms event.

Confirmed with scoped `journalctl -u systemd-suspend.service -u
systemd-logind.service` queries; the suspend-unit excerpt is retained in
`.scratch/spinner-log-analysis-20260928/suspend.log`. These delays must not be
attributed to whichever short timer happened to run next. The 38-minute event's
reported slowest timer took only 7 ms.

## Responsiveness findings

During the primary capture:

| Observation | Count |
|---|---:|
| Recorded lag events | 5,604 |
| Delay >500 ms | 1,675 |
| Delay >1,000 ms | 203 |
| Delay >5,000 ms | 5 |
| Input pending at lag observation | 220 |

The five delays above five seconds are the two large system sleeps and three
events during recovery at 07:03:27/43/50. That recovery included an accidental
large settlement-object serialization from this diagnostic session; it is not
representative animation work. Some historical events are below today's
configured 500-ms logging threshold; comparisons use explicit stored delays,
not assumptions that configuration remained constant throughout the run.

Among captured lag observations, the slowest recorded timer was:

| Callback | Observations | Median timer time | p95 | Maximum |
|---|---:|---:|---:|---:|
| `mevedel-view--flush-scheduled-render` | 909 | 439 ms | 958 ms | 1,346 ms |
| `mevedel-execution-process--settle-main-exit` | 793 | 426 ms | 972 ms | 3,455 ms |

These are meaningful leads for interactive-latency investigation, not independent
samples of every callback invocation, nor proof that the named callback caused
the whole delay. The heartbeat reports only the slowest timer since its previous
tick (`mevedel-telemetry.el:676–707`). Anonymous callbacks also occur frequently.
Long callback wall time can include nested minibuffer event loops: an anonymous
timer interval in the stalled request is measured in hours while heartbeat
events continue. That is not a continuous CPU blockage.

## Workload and verification churn

Across the entire session, telemetry records:

- 4,978 provider dispatch events, 111 agent dispatches, and 7,250 tool finishes.
- 1,626 managed execution finishes. Their summed durations are about **8.5
  hours**, but overlap, subprocess wait, and system sleep make this neither
  serial critical-path time nor CPU use.
- 198 executions classified as Eask. Their durations sum to about **5.55
  hours**, with the same caveats. Do not interpret 62 `test-scope=full` tags as
  62 complete suites: the current classifier calls any recognized Eask command
  without explicit `test/*.el` targets `full`, including cleanup/compilation
  (`mevedel-execution-telemetry.el:246–287`).
- 28 completed compaction spans total **19 m 40 s**, median 42 s; 19 segment
  rotation spans total **11.95 s**, maximum 940 ms. Compaction includes summary
  provider work. This does not point to segment rotation as the dominant
  end-to-end cost; it also does not assess repeated background persistence.
- 445 tool error outcomes, including 17 `UpdateGoal` errors. These counts include
  expected failures, verification refusals, and nested ToolCall reporting; they
  are not 445 distinct product bugs.

The validation report separately confirms repeated full-suite retries, real
newly discovered boundary defects, and interference from concurrent build/test
activity. Its final full run takes 721 s while the final focused roster takes
22.7 s. Preserve required final checks, but consolidate tests around lifecycle
and visibility invariants and isolate build caches/runners to reduce avoidable
repetition.

## Diagnostic limits worth addressing later

1. Native CPU samples have no per-sample time/session labels; read the CPU
   report before selecting a hotspot. This session's telemetry cannot turn a
   process-wide aggregate into a final-source or spinner-only benchmark.
2. Provider dispatches lack a unique HTTP-round correlation identity, and agent
   dispatch records lack the corresponding agent path. Root stream cleanup can
   overlap the next dispatch under the same request ID. Naive latency pairing
   is unreliable; do not publish its percentiles as measured provider latency.
3. Lag reports should distinguish system-sleep gaps from editor work. Callback
   wall time alone also cannot separate nested interactive waiting from CPU.
4. Eask workload tags need distinct clean/compile/focused/full classifications
   before being used to count full-suite runs.

No product changes or tests were made for this analysis.
