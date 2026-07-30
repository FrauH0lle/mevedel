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

| Scenario | Commit | Median wall time | Sampled allocation | History scans |
| --- | --- | ---: | ---: | ---: |
| Status refresh | `c527b7b` | 312.01 ms | 27,139,475 bytes | 1 / 1,048,608 chars |
| History fallback | `c527b7b` | 322.00 ms | 27,008,307 bytes | 1 / 1,048,576 chars |

Decision: investigate in a separate optimization change.  Every status refresh
still invokes the linear history fallback despite live zone markers, and its
absolute cost exceeds the gate.  This evidence commit deliberately adds no
cache or new state.

## Interactive rerun

Pending the user's equivalent real-session rerun with
`M-x mevedel-session-debug`; batch benchmarks cannot validate redisplay,
permission-overlay timing, or the reported intermittent cursor jump.
