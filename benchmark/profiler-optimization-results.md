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

## Retry-gap bounded search

Workload: 50 classifications of a 4 MiB tool-property gap whose first
non-whitespace character is the final character.

| Revision | Commit | Median wall time | Sampled allocation |
| --- | --- | ---: | ---: |
| Baseline | `c553513` | 393.42 ms | 211,284,391 bytes |
| Bounded search | `67aa18e` | 283.17 ms | 1,561,011 bytes |

Decision: ship.  The bounded search reduced median wall time by 28.0%, removed
all benchmark GC cycles, and reduced sampled allocation by 99.3%.

## Marker-first prompt lookup

Workload: 500 prompt-position lookups over a 5 MiB rendered transcript,
followed by a real status/interaction redraw with a selected multiline
composer draft beginning with `>`.

| Revision | Commit | Median wall time | Sampled allocation | Prompt scans |
| --- | --- | ---: | ---: | ---: |
| Baseline | `67aa18e` | 0.226 ms | 1,560,835 bytes | 500 |
| Marker first | `6605bb8` | 0.292 ms | 1,560,835 bytes | 0 |

Decision: ship.  The live-marker path removed all 500 full-range scan
requests.  The 0.066 ms aggregate timing difference is below the materiality
gate, so no wall-time claim is made.

## Tool-segment copying

Workload: one accurate and one drifted lookup for each of two 64 KiB persisted
tool blocks.

| Commit | Median wall time | Sampled allocation | Substrings | Bytes copied |
| --- | ---: | ---: | ---: | ---: |
| `ea479c0` | 43.60 ms | 2,480,947 bytes | 6 | 262,684 |

Decision: defer optimization.  Four requested segment strings account for
approximately four full block copies; drift recovery adds two small initial
slices, not repeated copies of the recovered full ranges.  A persistent cache
is not justified by this workload.

## Telemetry perturbation

Workload: 250 identical sanitized events with persistence enabled and disabled.

| Mode | Commit | Median wall time | Sampled allocation | Emitted bytes |
| --- | --- | ---: | ---: | ---: |
| Enabled | `ea479c0` | 40.88 ms | 12,586,905 bytes | 47,890 |
| Disabled | `ea479c0` | 0.076 ms | 1,560,835 bytes | 0 |

Decision: defer batching.  Persistence adds about 0.163 ms and 44 KiB sampled
allocation per event, below both absolute per-iteration gates.  The current
append durability remains unchanged.

## Status and history scans

Workload: one status refresh and one detached-marker history fallback over a
1 MiB rendered transcript while preserving a selected multiline composer
draft beginning with `>`.

| Scenario | Commit | Median wall time | Sampled allocation | History scans |
| --- | --- | ---: | ---: | ---: |
| Status refresh | `514ddff` | 305.95 ms | 27,139,475 bytes | 1 / 1,048,608 chars |
| History fallback | `514ddff` | 305.52 ms | 27,008,307 bytes | 1 / 1,048,576 chars |

Decision: investigate in a separate optimization change.  Every status refresh
still invokes the linear history fallback despite live zone markers, and its
absolute cost exceeds the gate.  This evidence commit deliberately adds no
cache or new state.

## Interactive rerun

Pending the user's equivalent real-session rerun with
`M-x mevedel-session-debug`; batch benchmarks cannot validate redisplay,
permission-overlay timing, or the reported intermittent cursor jump.
