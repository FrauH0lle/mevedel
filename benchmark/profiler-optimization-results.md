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
