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
| Bounded search | pending | pending | pending |

Decision: pending.

## Marker-first prompt lookup

Workload and results pending.

## Tool-segment copying

Workload and results pending.

## Telemetry perturbation

Workload and results pending.

## Status and history scans

Workload and results pending.
