# Profiler optimization results

Run benchmarks from a clean checkout with the project and test directories on
the load path:

```bash
npx @emacs-eask/cli emacs --batch -L . -L test \
  -l benchmark/mevedel-profiler-optimization.el -- SCENARIO OUTPUT_DIRECTORY
```

Raw profiles live under
`.scratch/profiler-optimization/results/SCENARIO/COMMIT/`.  This ledger records
the reproducible workload, commits, median measurements, and optimization
decision.  A candidate qualifies only when five measured rounds show at least
a 10% median improvement plus either 10 ms saved or 1 MiB less sampled
allocation per normalized iteration.

`Commit` identifies the package source under test; `Harness` identifies the
benchmark source.  Current metrics also record the harness SHA-256, tracked
worktree-diff SHA-256, and dirty-file list.  The two historical baselines were
run from the listed source revisions with the not-yet-committed harness later
recorded by the listed harness commits.

## Retry-gap bounded search

Workload: 50 classifications of a 4 MiB tool-property gap whose first
non-whitespace character is the final character.

| Revision | Commit | Harness | Median wall time | Sampled allocation |
| --- | --- | --- | ---: | ---: |
| Baseline | `c553513` | `8005a4d` | 393.42 ms | 211,284,391 bytes |
| Bounded search | `67aa18e` | `8005a4d` | 283.17 ms | 1,561,011 bytes |

Decision: ship.  The bounded search reduced median wall time by 28.0%, removed
all benchmark GC cycles, and reduced sampled allocation by 99.3%.

## Marker-first prompt lookup

Workload: 500 prompt-position lookups over a 5 MiB rendered transcript,
followed by a real status/interaction redraw with a selected multiline
composer draft beginning with `>`.

| Revision | Commit | Harness | Median wall time | Sampled allocation | Prompt scans |
| --- | --- | --- | ---: | ---: | ---: |
| Baseline | `67aa18e` | `0fe2b3d` | 0.226 ms | 1,560,835 bytes | 500 |
| Marker first | `c527b7b` | `c527b7b` | 0.262 ms | 1,560,835 bytes | 0 |

Decision: ship.  The live-marker path removed all 500 full-range scan
requests.  The 0.036 ms aggregate timing difference is below the materiality
gate, so no wall-time claim is made.

## Tool-segment copying

Workload: one selected-window full redraw containing two 64 KiB persisted Bash
blocks, one with accurate and one with drifted gptel bounds.  The redraw
preserves a multiline `>` composer draft and renders the Bash rows.

| Commit | Median wall time | Sampled allocation | Segment calls | Substrings | Bytes copied |
| --- | ---: | ---: | ---: | ---: | ---: |
| `c527b7b` | 4.90 ms | 3,046,017 bytes | 2 | 2 | 131,256 |

Decision: defer optimization.  The real render pass performs exactly one
segment extraction per tool block and one substring per extraction.  It does
not reproduce same-range copies that would justify a persistent cache.

## Telemetry perturbation

Workload: 250 identical sanitized events with persistence enabled and disabled.

| Mode | Commit | Median wall time | Sampled allocation | Emitted bytes |
| --- | --- | ---: | ---: | ---: |
| Enabled | `c527b7b` | 41.64 ms | 12,591,049 bytes | 47,890 |
| Disabled | `c527b7b` | 0.078 ms | 1,560,835 bytes | 0 |

Decision: defer batching.  This isolated persistence microbenchmark adds about
0.166 ms and 44 KiB sampled allocation per event, below both absolute
per-iteration gates.  It does not substitute for the pending interactive
redraw/tool-execution comparison, so no broader perturbation claim is made.

## Status and history scans

Workload: one status refresh and one detached-marker history fallback over a
1 MiB rendered transcript while preserving a selected multiline composer
draft beginning with `>`.

Finalized harness SHA-256:
`64b2ef3aeee8eba2ef496b6765fe6a730125f56ac7d9c991a46a2d377270c526`.

| Scenario | Revision | Median wall time | Sampled allocation | History checks |
| --- | --- | ---: | ---: | ---: |
| Status refresh | `5bb0965` baseline | 309.86 ms | 27,172,267 bytes | 1,048,609 |
| Status refresh | candidate | 0.064 ms | 1,560,835 bytes | 5 |
| History fallback | `5bb0965` baseline | 304.47 ms | 27,008,307 bytes | 1,048,608 |
| History fallback | candidate | 0.008 ms | 1,560,835 bytes | 4 |

Decision: ship. Reverse property-run traversal removes the per-character
fallback without adding cache state. Status refresh saves 309.79 ms median and
94.3% sampled allocation in this fixture; direct fallback saves 304.46 ms and
94.2%.

## Pending-tool spinner scope

Workload: 100 pending-tool spinner ticks over a 1 MiB rendered transcript,
with one live pending row and a selected multiline composer draft beginning
with `>`. The same finalized harness above measured both revisions.

| Revision | Median wall time | Sampled allocation | Inline scan range |
| --- | ---: | ---: | ---: |
| `5bb0965` baseline | 3.842 ms | 1,577,379 bytes | 104,864,500 chars |
| Candidate | 3.825 ms | 1,577,379 bytes | 2,900 chars |

Decision: ship for ownership correctness and bounded work, but make no timing
or allocation claim. Restricting updates to the managed `history-live` zone
removed 99.997% of the requested scan range and prevents unrelated matching
properties elsewhere in the view from being mutated; the 0.4% timing change is
below the materiality gate.

## Running tool calls in the activity run

Workload (`tool-burst`): one in-flight turn after a 1 MiB rendered transcript,
with a selected multiline composer draft, running three parallel batches of
four calls in gptel's hook order: every pre-tool hook, a boundary render, the
results, every post-tool hook, and a boundary render.  Each round stops and
removes its turn.  Both revisions ran the same harness, alternating, three
times each; the table gives the range of the five-round medians.

| Revision | Median wall time | Sampled allocation | Zone reconciles | View modification ticks | Live renders |
| --- | ---: | ---: | ---: | ---: | ---: |
| `a5210c4` baseline | 20.0–20.7 ms | 2,104,006 bytes | 74 | 518 | 6 |
| Running calls in the run | 11.8–12.6 ms | 1,969,662–1,977,846 bytes | 10 | 158 | 6 |

`spinner-ticks` (ported to draw its row with the live render's functions) and
`tool-segments` stayed at parity: 1.30–1.50 ms against 1.31–1.39 ms, and
4.36–4.48 ms against 4.19–4.72 ms.

Decision: ship.  The change removes the per-hook fragment reconciliation, so a
burst does 39% less work at an unchanged number of renders.  It falls short
of the 10 ms materiality gate in absolute terms, but it was made for
presentation and had to show no regression, which all three scenarios meet.

## Running tool names keep their color

Workload: a graphical PGTK frame in a nested KWin, 20,000 lines of history,
three running shimmer rows.  Measured: color-bank preparation from a cold
cache, and 100 frame ticks across one full sweep.  Base (grey `ephemeral`
face) and the change (each span's own face) ran alternately, six runs each.

| Revision | Bank preparation | 100-tick sweep |
| --- | ---: | ---: |
| `9f3d28c` baseline | 2.54–2.90 ms | 21.6–30.4 ms |
| Span-owned face | 2.54–2.85 ms | 28.7–29.4 ms |

Banks were already keyed per label and face, so the change alters which face
a label's single bank uses, not how many exist.  Batch `spinner-ticks`
(1.35 ms) and `tool-burst` (11.9 ms) were unchanged.

Decision: ship; parity on every measure.

## Interactive rerun

The supplied real-session diagnostics close the interactive rerun:

- 1,344 composer zone-reconcile before/after pairs had zero offset or draft
  mismatches.
- The current profiler run recorded 18 composer interaction-registration
  pairs with zero mismatches and no render fallbacks.
- No sustained heap growth indicated a session memory leak.

The remaining large full rerenders measured 191 ms median, 443 ms p95, and
556 ms maximum for the 347 KiB worker transcript. That path is real but
infrequent in this run, so broader incremental-render changes remain deferred
until a reproducible interaction shows they dominate user-visible latency.

## Compiled hot-path allocation gate (2026-10-23)

Command: `npx @emacs-eask/cli run script bench-gate`.
Package baseline: `6d6f4a95`, with measurement-only harness changes. Emacs
31.1, x86_64 Linux. The runner compiles into a temporary directory and loads
the result in a fresh batch subprocess with private HOME/XDG/native caches.
The three scenarios use the profiler's shared view fixture, 1 MiB synthetic
rendered response history, a selected 80-column/25-line frame and a multiline
draft starting with `>`. Five warmups precede each fresh-fixture sample.

The stream sample renders one appended ` fixed stream chunk.` on an in-flight
response. The composer sample inserts `x` and runs the view's actual
`post-command-hook` after priming its window geometry. The static-spinner tick
crosses a bound clock from 1000.25 to 1001.25 seconds, rewriting the progress
rail elapsed label from `12s` to `13s`. Guards check modified ticks, the actual
chunk/elapsed label and composer text/point. Setup and assertions are not
measured; clock binding and reconciliation counting are measured consistently.

Each sample starts after full GC and subtracts the componentwise minimum of
three no-op samples, clamped at zero. GC settings: 16 MiB threshold, 0.1 heap
percentage. Three independent command invocations produced identical counters:

| Scenario | Net conses (all three runs) | String chars | Vector cells | GCs | Cons budget |
| --- | ---: | ---: | ---: | ---: | ---: |
| Stream chunk | 864 | 7,592 | 1,740 | 0 | 964 |
| Composer keystroke | 50 | 22 | 62 | 0 | 150 |
| Zone tick | 1,732 | 721 | 637 | 0 | 1,832 |

Cons range and population variance are **0** for every scenario. Budgets are
baseline + **100 conses** (fixed absolute margin, not a percentage); only conses
gate acceptance. These are allocation counts, not live-heap sizes, profiler
byte samples, graphical redisplay cost or real-session latency.

## Minimal contiguous-edit twin (2026-10-23)

Command: `npx @emacs-eask/cli run script bench-twin`. Same package baseline,
fixture, Emacs and GC settings as the gate above. Each timing is a median of
five fresh-fixture samples after five warmups. All replays matched the live
buffer with `equal-including-properties`.

| Scenario | Observed real chars deleted / inserted | Floor chars deleted / inserted | Observed property calls / floor runs | Whole update | Floor replay |
| --- | ---: | ---: | ---: | ---: | ---: |
| Stream chunk | 21 / 41 | 0 / 20 | 9 / 5 | 0.609 ms | 0.00564 ms |
| Zone tick (`12s` -> `13s`) | 27 / 27 | 1 / 1 | 0 / 0 | 0.141 ms | 0.00334 ms |

Timing uses ordinary bytecode without observation advice. Counts use a separate
build where `insert` and `delete-region` compile as generic calls, because
their normal bytecode opcodes bypass advice. The source and scenario remain
the same; only the count build is instrumentable, and it is never timed.
Observed property counts are successful nonempty calls to
`set-text-properties`, `add-text-properties` and `put-text-property`, including
redundant calls; properties carried by inserted strings are not explicit
property writes. Other primitives and internal C mutations are not counted.

The floor keeps the common character prefix and nonoverlapping suffix,
inserts the changed middle including its final properties, then assigns only
differing property runs. It is a single-span text/property floor, not a
globally minimal multi-edit script. Snapshot copying, derivation and equality
checks are outside both timings; replay has no product hooks, overlays, marker
state or graphical redisplay. Timing ratios are diagnostic, not a claimed
achievable renderer speedup. This report does not gate acceptance.

## Full projection attribution

`full-projection` measures a deterministic forty-exchange source history
(312,920 characters: prose, Markdown/code panels, 120 tools and 40 reasoning
blocks), or restores a session directory through the production loader. The
restored path always makes a private temporary copy, relocates its workspace,
forces read-only acquisition, and deletes the working copy afterward. The
fixture itself must remain local, not committed.

Compile the package and harness before running:

```sh
npx @emacs-eask/cli compile
npx @emacs-eask/cli emacs --batch -L . \
  --eval '(setq mevedel-benchmark--no-main t)' \
  -f batch-byte-compile benchmark/mevedel-profiler-optimization.el
npx @emacs-eask/cli emacs --batch -L . \
  -l benchmark/mevedel-profiler-optimization.elc -- \
  full-projection OUTPUT-DIRECTORY [SESSION-DIRECTORY]
```

Set `treesit-extra-load-path` before loading the harness if Markdown grammars
are installed outside Emacs's default search path. Metrics record the resolved
mode; a nil mode measures the unfontified fallback, not Markdown fontification.
Bind `mevedel-benchmark--projection-gc-threshold` before loading to compare a
tuned heap; the default is 800,000 bytes with `gc-cons-percentage` 0.1.

Both synchronous projection and scheduled projection driven deterministically
to completion get two warmups, seven uninstrumented timing rounds, a separate
CPU+memory profile, and seven nested exclusive stage-timing rounds. The latter
record restore, scan, grouping/annotation, fontification, tool preparation,
reasoning cleanup, turn work, insertion helpers, and remaining publication work.
GC is included where it occurs. Inline bytecode insertion stays inside turn
time; named tool insertion and display-property helpers are reported separately.
The cached-turn partition includes calls to the existing response, reasoning,
and tool cache owners, including their lookups and misses; uncached-turn time
includes uncached prompt/code-panel fontification and insertion. This is not a
cache-hit rate or a predicted speedup. Sync/batch history text and the multiline
`>` composer draft are checked.

2026-10-23, base package `6d6f4a95`, Emacs 31.1, 80-column batch window,
installed Markdown grammars: representative root session
`2026-10-17T08-06-5eea23ca6f19`, current `segment-0001.chat.org`, 3,199,884
bytes / 3,185,854 restored characters, SHA-256
`8347f883ad7839abb37f669acf1f22894ea0c3d4c53829f6c49c204c464cd16a`.
Its canonical scan contains 642 tools, 104 reasoning and 99 mailbox segments.

| Fixture / path | Default-GC median | GC time / collections | Uncached turn share | 64 MiB median | Uncached turn share |
| --- | ---: | ---: | ---: | ---: | ---: |
| Synthetic synchronous | 433.2 ms | 186.5 ms / 7 | 51.7% | 264.4 ms | 27.1% |
| Synthetic batched | 428.8 ms | 143.7 ms / 7 | 37.4% | 289.2 ms | 24.9% |
| Restored synchronous | 745.0 ms | 342.4 ms / 9 | 58.7% | 401.5 ms | 54.3% |
| Restored batched | 996.6 ms | 472.4 ms / 11 | 39.4% | 508.7 ms | 41.9% |

Shares are medians of each instrumented round's uncached-turn/total ratio,
not ratios mixing separate timing populations. All 64 MiB timing rounds had
zero collections. These are repeated full projections with current caches,
not cold session-load timings, timer-wait latency, or graphical redisplay.
Restoration during projection measures normalization of already restored
properties; initial disk loading/hydration is outside the measured workload.

Decision: proceed to the turn-cache design, rather than close the backlog
entry as immaterial. The real transcript remains well above the approximately
25% threshold under both GC settings. It does not establish a cache's eventual
speedup: insertion, source mapping and position-dependent affordances still
need recreation, and substantial publication/disclosure work remains outside
the turn renderer. No product cache is implemented by this measurement change.

## Targeted full-projection fixes (2026-10-24)

Controlled comparison: `dcb5e5af` versus product revision `fa1e8101`.
The independently revertible changes are `09c6be7a` (cold harness compilation),
`d472a3c5` (monotone disclosure traversal and removal of redundant capture),
`1bfb893a` (projection-scoped Markdown availability), `ef1e844b` (fork-point
index), and `fa1e8101` (canonical tool preparation and completed Markdown
promotion). This is targeted preparation reuse, not a persistent turn cache.

Both revisions use the same fixed harness, compiled into private directories,
with frozen runtime assets and identical installed dependencies. The baseline
is a detached worktree under `.scratch/projection-fixes-measurements/base`.
The harness overlay fixes only its compile-time sibling-fixture path; its
scenarios, stage owners and gate arithmetic are unchanged. Harness source SHA-256
`001c0009add2a3166cdd8dd81f1dd6e342ef27aefbf761668ca403b22046b4ab`;
compiled harness SHA-256
`cd6f9abc4d75bd791ea4978305d87d33936b4ec69d0028bdafdea75c58596b09`.
Both source worktrees were clean at freeze.

The fixtures and definitions are those above: forty synthetic exchanges and
the frozen 3,199,884-byte restored root. Emacs 31.1 runs in a separate batch
process with an 80-column, 25-line selected window and `markdown-ts-mode` with
both installed grammars. Each fixture/GC cell has **seven alternating
baseline/change process pairs**. Each process keeps the existing two warmups,
seven uninstrumented timing rounds, separate native CPU/memory profiling, and
seven exclusive stage rounds per synchronous/batched path. Timings below are
medians of seven process timing medians; stages and shares use the 49 separate
instrumented rounds per revision/cell/path. GC percentage is 0.1, with explicit
thresholds 800,000 or 67,108,864 bytes. One advisory lock excludes competing
tests, compilation and timing runs. No code is loaded into the running editor.

Raw evidence, launch arguments, source/bytecode/asset/dependency hashes and
native profiles remain local under `.scratch/projection-fixes-measurements/`;
`final/summary.json` and `final/cold-summary.json` summarize the raw metrics.
All synchronous/batched and baseline/change history hashes match. Fixture,
grammar, build and dependency postchecks pass. Independent raw-metric
recomputation confirms wall medians, the cached/uncached partition and same-round
share medians, rather than trusting the summary alone.

### Complete workload: baseline -> targeted fixes

All times are milliseconds. GC columns give time / collection count.

| Fixture / path | Default wall | Default GC | 64 MiB wall | 64 MiB GC |
| --- | ---: | ---: | ---: | ---: |
| Synthetic sync | 430.5 -> 275.4 | 186.4/7 -> 185.6/7 | 246.2 -> 86.9 | 0/0 -> 0/0 |
| Synthetic batch | 480.6 -> 292.7 | 211.9/8 -> 183.2/7 | 269.8 -> 104.3 | 0/0 -> 0/0 |
| Restored sync | 712.9 -> 560.8 | 324.0/10 -> 290.5/9 | 375.2 -> 265.3 | 0/0 -> 0/0 |
| Restored batch | 930.9 -> 638.3 | 442.6/13 -> 301.5/9 | 475.3 -> 325.5 | 0/0 -> 0/0 |

The restored workload improves **21.3% sync / 31.4% batch** at default GC and
**29.3% / 31.5%** at 64 MiB. Synthetic improvements are 36.0% / 39.1% and
64.7% / 61.4%, respectively. These comparisons use the new controlled baseline,
not the earlier independent attribution run's timing population.

### Exclusive stages: default GC

Each cell is baseline -> change, milliseconds. Individual stage medians need
not sum to the median total: collections can move between stages.

| Stage | Synthetic sync | Synthetic batch | Restored sync | Restored batch |
| --- | ---: | ---: | ---: | ---: |
| Source property restore/normalization | .043 -> .039 | .035 -> .036 | 10.820 -> 10.808 | 10.751 -> 10.747 |
| Canonical scan/segmentation | 2.179 -> 2.185 | 2.188 -> 2.188 | 74.205 -> 75.363 | 81.218 -> 80.911 |
| Grouping/filter/annotation | .298 -> .297 | .934 -> .929 | 1.929 -> 1.915 | 4.377 -> 4.366 |
| Markdown/generic/code fontification helpers | 40.040 -> 38.954 | 42.835 -> 13.421 | 10.432 -> 46.471 | 10.323 -> 10.214 |
| Tool preparation/render-cache owner | 1.192 -> 1.174 | 30.508 -> 5.355 | 9.657 -> 9.545 | 38.732 -> 80.355 |
| Reasoning cleanup/cache owner | .052 -> .053 | .057 -> .057 | 1.205 -> 1.164 | 1.199 -> 1.157 |
| Response preparation/cache owner, excluding inner fontification | 6.301 -> .842 | 88.259 -> 2.150 | 53.938 -> 2.746 | 197.169 -> 7.920 |
| Remaining per-turn rendering | 155.385 -> 95.482 | 93.713 -> 66.257 | 336.476 -> 253.223 | 248.140 -> 228.148 |
| Named insertion/display-property helpers | 36.831 -> 96.174 | 98.041 -> 155.941 | 110.734 -> 109.879 | 108.861 -> 104.125 |
| Remaining publication/preservation/bookkeeping | 186.066 -> 39.170 | 148.152 -> 53.911 | 106.685 -> 60.050 | 214.327 -> 121.962 |
| Median instrumented total | 429.586 -> 277.102 | 486.686 -> 300.527 | 719.047 -> 569.989 | 908.715 -> 649.963 |

### Exclusive stages: 64 MiB GC

| Stage | Synthetic sync | Synthetic batch | Restored sync | Restored batch |
| --- | ---: | ---: | ---: | ---: |
| Source property restore/normalization | .038 -> .034 | .034 -> .032 | 10.737 -> 10.679 | 10.959 -> 10.768 |
| Canonical scan/segmentation | 2.278 -> 2.243 | 2.277 -> 2.235 | 44.228 -> 44.000 | 44.835 -> 44.815 |
| Grouping/filter/annotation | .333 -> .321 | .935 -> .931 | 1.978 -> 1.976 | 4.278 -> 4.282 |
| Markdown/generic/code fontification helpers | 12.534 -> 12.257 | 12.670 -> 12.347 | 9.808 -> 9.663 | 9.810 -> 9.721 |
| Tool preparation/render-cache owner | 1.130 -> 1.129 | 4.642 -> 5.245 | 9.283 -> 9.140 | 36.857 -> 44.789 |
| Reasoning cleanup/cache owner | .050 -> .050 | .054 -> .054 | 1.154 -> 1.120 | 1.171 -> 1.133 |
| Response preparation/cache owner, excluding inner fontification | 5.879 -> .817 | 92.539 -> 2.099 | 24.998 -> 2.705 | 94.686 -> 8.012 |
| Remaining per-turn rendering | 43.434 -> 43.108 | 42.216 -> 41.695 | 165.085 -> 121.158 | 155.704 -> 112.044 |
| Named insertion/display-property helpers | 11.561 -> 11.527 | 11.666 -> 11.601 | 33.059 -> 32.698 | 35.102 -> 34.602 |
| Remaining publication/preservation/bookkeeping | 172.746 -> 16.834 | 102.858 -> 28.814 | 79.451 -> 34.123 | 85.055 -> 58.680 |
| Median instrumented total | 250.451 -> 88.216 | 269.423 -> 104.877 | 380.029 -> 267.563 | 478.679 -> 329.380 |

Not every stage improves. Tuned restored batch tool preparation rises from
36.857 to 44.789 ms, while the complete batch falls by 149.8 ms. The row includes
canonical normalization/classification and scheduling, not only persistent tool
cache misses. Default-GC collections also move to different stage owners; the
tuned table isolates these shifts from the underlying work. No exhaustive
primitive-insertion or per-fix isolated speedup is claimed.

### Re-run of the 25% uncached-turn gate

| Fixture / path | Default share | 64 MiB share |
| --- | ---: | ---: |
| Synthetic sync | 54.09% -> 84.11% | 27.09% -> 75.76% |
| Synthetic batch | 48.28% -> 78.41% | 24.60% -> 62.49% |
| Restored sync | 63.99% -> 71.55% | 54.74% -> 61.20% |
| Restored batch | 39.11% -> 52.70% | 41.91% -> 47.50% |

The gate remains above 25% after the fixes. Its share can rise while absolute
uncached work falls because these fixes also remove substantial preparation and
preservation outside the numerator. For example, tuned restored uncached turn
medians fall from 208.0 to 163.6 ms sync and 200.5 to 156.6 ms batch. The gate
still justifies investigating pure turn preparation reuse; it is not a promised
cache speedup, because the numerator includes insertion and fresh source mapping.
This step does not close the turn-reuse backlog item or add a final-text cache.

### Separate cold-response-cache batch-only check

Clear only the persistent response cache immediately before the first batch,
then drain a second batch with no synchronous warming in between. Repeat seven
alternating process pairs per fixture/GC cell. Setup may already have initialized
the view, so this is response-cache cold, not process/disk/session-load cold.
Counter runs are a separate population from the uninstrumented timing pairs.

| Fixture / GC | Base first / second | Change first / second |
| --- | ---: | ---: |
| Synthetic / default | 743.8 / 933.3 ms | 704.4 / 261.4 ms |
| Restored / default | 2195.4 / 1907.4 ms | 1943.4 / 637.1 ms |
| Synthetic / 64 MiB | 369.8 / 451.0 ms | 351.1 / 103.7 ms |
| Restored / 64 MiB | 1280.7 / 1119.7 ms | 1119.5 / 325.6 ms |

The baseline starts 40 synthetic / 190 restored Markdown jobs on **both** passes
and retains zero response entries. The change starts 40 / 190 jobs on the first
pass, **zero** on the second, and retains 40 / 190 entries. Actual grammar
availability-owner calls fall from 160 / 760 per projection to **one**; these
counts instrument `markdown-grammars-ready-p`, not cheap snapshot lookups.
Focused tests additionally cover bounded eviction, incomplete-job cancellation,
canonical merged tuple equality, interrupted per-entry progress, and full tool
bodies after publication newly remembers an expanded fold.

### Verification and limits

- Required Eask bytecode cleanup, then full suite: **12,520/12,520 results,
  zero unexpected, 38 conditional skips**, eight workers exit 0; 222.91 s.
- The first full run had one native Wayland idle/configure assertion fail.
  No code or test was changed: isolated native tests pass 3/3 on both revisions,
  and the identical eight-worker full rerun passes. The initial failure remains
  recorded in `.scratch/test-suite-performance/20261023-234157/`; final report
  `.scratch/test-suite-performance/20261023-234948/`. No checks were weakened.
- Final package/harness private builds compile without warnings. Full-suite
  worker logs contain no unexpected diagnostic noise. Static review has no
  unresolved actionable findings; `git diff --check` passes.
- `bench-gate` passes with **unchanged** net conses/budgets: stream **864/964**,
  composer **50/150**, zone **1732/1832**, zero GCs. `bench-twin` passes with
  unchanged observed edit counts: stream 21 deleted / 41 inserted / 9 property
  calls, zone 27 / 27 / 0. The zone implementation is untouched.
- Measurements preserve the multiline leading-`>` draft and point and exact
  sync/batch history text. Regression suites cover reader anchors, undo sanitation
  and mutation ownership. This is not a graphical redisplay, maximum callback
  delay, retained-memory, or provider-transport measurement. The conditional
  suite skips remain unverified; no new skip was added.
