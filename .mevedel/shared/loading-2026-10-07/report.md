# Feature-boundary loading — 2026-10-07

Implemented in `.scratch/worktrees/claude-code-engine`, based on
`108de8d0e81e16fa5c5585c9bc53c09fff1c6f75`. Existing loading guidelines in
`AGENTS.md` and `docs/development.md` were retained. The completed loading-scan
backlog entry was removed. No compatibility layer or new configuration was added.

## Result

The entry point loads foundational data and exposes existing commands with
ordinary autoloads. Installation retains full discovery metadata and integration
hooks. Execution, patch, and web catalogs have registration-only owners;
filesystem and shared-editing entry points autoload their implementations.
Claude provider identity is independent of ACP/MCP/usage execution. Saved-lobby
inspection starts browser dependencies only when a recorded lobby starts.

Instruction restoration remains installed with its foundational owner. Journal
scheduling is available before session startup; execution completion callbacks
remain callable; pending Plan/directive restoration depends on durable state
rather than whether its implementation was previously loaded. Uninstall and
unrelated focus/theme activity do not activate deferred features. Interactive
autoload declarations retain commands in `M-x` when another owner loads.

## Measurements

Five samples per configuration, alternating baseline/current and source/compiled
in fresh `emacs --batch -Q` children launched through Eask. The baseline is an
archive of the commit above. All seven dependency paths and artifact hashes match
the earlier scan. GC uses threshold 800000 and percentage 1.0. Timing includes GC,
excludes process startup, prerequisite `cl-lib`/`json`, and shutdown. No paid calls.

Median milliseconds, with gptel initially unloaded:

| Phase | Compiled baseline | Compiled current | Source baseline | Source current |
| --- | ---: | ---: | ---: | ---: |
| Require | 185.70 | 32.14 | 369.09 | 54.66 |
| Install | 38.30 | 75.90 | 43.26 | 155.06 |
| First public view | 259.05 | 336.27 | 307.48 | 437.76 |
| Repeated view | 0.125 | 0.110 | 0.153 | 0.143 |
| First Read after view | 1.70 | 2.03 | 4.68 | 5.76 |
| Repeated Read | 1.19 | 1.13 | 3.74 | 3.84 |
| First ListExecutions without a view | 0.67 | 17.99 | 2.34 | 34.45 |
| Repeated ListExecutions | 0.205 | 0.213 | 1.400 | 1.567 |

Cumulative medians are calculated per process, not by adding phase medians:

| Milestone | Compiled baseline → current | Source baseline → current |
| --- | ---: | ---: |
| Required and installed | 226.05 → 107.80 ms (52% lower) | 412.35 → 210.08 ms (49% lower) |
| First view ready, including startup | 483.04 → 443.16 ms (8% lower) | 719.73 → 646.37 ms (10% lower) |

Installation itself gets slower because it now loads registration owners that
were formerly loaded by `require`. First view also absorbs deferred session and
view work. The cumulative savings show that this is more than transferring the
entire cost to the first interaction. The execution-family first call pays about
17 ms compiled / 32 ms source for its deferred implementation and pipeline.

The new compiled entry loads 14 mevedel features; installation reaches 63
(source reaches 64), versus 159 and 172 previously. No repeated measured operation
loads another module. Sub-millisecond differences are not evidence of a durable
steady-state speedup or regression.

The first-view fixture starts the existing journal lifecycle, whose memory helpers
load Read/search/patch implementations. Read after that view therefore measures
full-pipeline use, **not cold Read implementation loading**. ListExecutions is a
separate process scenario with no view. Repeated Read uses the ordinary
unchanged-file cache. All fixtures are headless and local; these results do not
measure GUI painting, remote targets, real providers, or an OS-cold disk cache.

The catalog digest is identical in all 60 processes:
`7e6378dd2cb3cff73198c7f43941e0a2bf2bf59504da125b85791cab0397fa6f`.
It covers category/name, descriptions, prompts, argument schemas, groups,
read-only/snapshot/destructive/async metadata, result limits, and gptel schemas.
Callback behavior is verified separately by cold permission, path, renderer,
handler, and pipeline tests.

[results.json](results.json) retains every sample, range, GC cost, feature list,
dependency hash, and the corresponding gptel-preloaded matrix.
[measure-test.el](measure-test.el) and [measure-worker.el](measure-worker.el) retain
the measurement harness. To repeat, archive the baseline into
`.scratch/loading-measurement/baseline`, then run:

```sh
npx @emacs-eask/cli clean elc
npx @emacs-eask/cli test ert .mevedel/shared/loading-2026-10-07/measure-test.el
```

## Verification

- Warning-free `npx @emacs-eask/cli compile` (241 root files).
- Required Eask bytecode cleanup, then full isolated parallel suite:
  **9,614 passed, 23 skipped, 0 unexpected** out of 9,637; 150.52 seconds.
  Report: `.scratch/test-suite-performance/20261007-115540/summary.json`.
- `test/test-mevedel-loading.el` independently compiles every module in a fresh
  process, then runs seven scenarios in both source and compiled fresh processes:
  installation/uninstall/reinstall and focus/theme isolation; native gptel dry-run;
  first public view; persisted tool lookup, permission/path callbacks and tool
  dispatch; cold renderers; real ACP text/workload fixture exchanges; absent,
  empty, malformed, failed, valid, repeated, and explicitly stopped lobby intent.
- Existing two-process Claude restart, directive planning, and cold tool/Goal
  ownership tests pass. The capacity fixture explicitly sets three slots; the
  separate struct test retains the ten-agent application default.
- Viewer protocol, lobby controller, and artifact-comment checks pass.
- The final worker logs contain no unexpected diagnostic noise. The Markdown-link
  fixture now uses a real temporary file instead of replacing `file-exists-p`;
  journal hook ownership remains unchanged across uninstall.
- No live editor reload, deployment, or provider call was performed.
