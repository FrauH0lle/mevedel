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
