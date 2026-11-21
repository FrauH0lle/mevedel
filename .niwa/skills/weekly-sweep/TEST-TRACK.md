# Test track

Goal: keep the suite fast and reliable as it grows. Find what got slower or
flaky since the last sweep and fix it; remove tests whose behaviour other
tests already cover.

Timing data: `.scratch/test-suite-performance/latest-durations.csv` (slowest
first) and each run's `durations.csv` and `summary.json` under
`.scratch/test-suite-performance/`. The baseline is the closing full-suite
report the previous sweep recorded in its ledger. For the 2026-10-31 sweep,
whose closing report lived in a removed worktree, use
`20261101-175241-562286` (first green full suite after it: 520 files,
460 s wall). For the 2026-11-07 sweep the baseline is
`20261107-182932-573537` (533 files, 14292 results, 268 s wall,
8 workers). For the 2026-11-13 sweep the baseline is
`20261114-110608-424227` (548 files, 14550 results, 281 s wall,
8 workers). After the 2026-11-14 continuation (lease-only sessions) the
baseline is `20261114-222430-799374` (550 files, 14525 results, 576 s wall,
4 workers). After the 2026-11-21 sweep the baseline is
`20261121-161327-957582` (landing tree on master 9e09dbef: 572 files,
14862 results, 853 s wall, 4 workers; summed test-seconds 2574, inflated by
intermittent +10/+20 s memory-test waits; integration run
`20261121-151526-909697`: 617 s, 2260 test-seconds). The close copies its report into the main checkout's
`.scratch/test-suite-performance/`. Commands and tiers:
`docs/development.md`, checks
table and testing section. The runner needs `eask` on PATH; without network,
use the cached copy under `~/.npm/_npx/*/node_modules/.bin/` (its `npx`
fallback fails offline).

## Workstreams

1. **Regressions.** Compare current per-file test-seconds with the baseline.
   Assign each case of 1 s or more that is new or slower one cause: fixed
   sleep, polling interval, real subprocess, repeated fixture setup, real
   filesystem or Git work, timeout used as the pass condition, or redundant
   scenario. Write `test-triage.md` beside the ledger.
   Done when every new or slower case of 1 s or more belongs to a cluster.
2. **Cluster fixes**, one worker per cluster, slowest first. Wait on the event
   or predicate instead of sleeping; let-bind intervals and timeouts down;
   share expensive setup; stub a subprocess the test does not check; merge
   variants that exercise the same path. Each change keeps every assertion's
   strength; a deleted test names the tests that still cover its behaviour.
   Done when the touched files pass three consecutive focused runs and the
   ledger records per-file test-seconds before and after.
3. **Flakes.** Diagnose and fix the backlog's "Tests and transport" entries
   and failures seen in the sweep's own runs.
   Done when each is fixed and removed from the backlog, or its blocker is
   recorded with evidence.

## Measurements

Compare before and after under the same load and worker count. Report each
measurement as its own labelled item: scenario, metric, value, unit, sample
count.
