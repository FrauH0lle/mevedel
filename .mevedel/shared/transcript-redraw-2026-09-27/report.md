# Recent-session transcript redraw measurements

## Implementation replay (2026-09-27)

After staging canonical scanning, grouping, Markdown and source-backed tool
preparation in the existing view batch, 27 new serial trials were compared with
27 untouched-code trials from `impl-before-20260927T083057` using the same
frozen fixture and harness hashes. All runs passed and matched the corresponding
baseline's history text and source-map hashes across modes; the source and
multiline typed draft were unchanged. Raw results remain private under
`.scratch/transcript-redraw-2026-09-27/results/`; the separately generated
`after-results.json` records exact paths, hashes, and numeric results without
altering the baseline `results.json` or the captures.

| Input / path | Median maximum terminal keyboard-to-command delay, before → after | Median completion, before → after |
|---|---:|---:|
| Root / cold scheduled | 180.9 → **77.8 ms** | 557.5 → 1026.0 ms |
| Agent / cold scheduled | 180.5 → **39.4 ms** | 440.8 → 789.7 ms |
| Root / warm scheduled | 127.2 → **101.6 ms** | 505.0 → 966.9 ms |
| Agent / warm scheduled | 66.8 → **56.8 ms** | 317.6 → 589.1 ms |
| Root / synchronous cold | 297.1 → 264.5 ms | 295.6 → 270.2 ms |
| Agent / synchronous cold | 258.4 → 246.9 ms | 265.0 → 248.5 ms |

The 100 ms median-maximum input goal is met for both cold-scheduled cases and
warm agent, **not** for warm root (101.6 ms, range 101.0–109.2). The ordinary
150 ms refresh debounce is included in scheduled completion. A separately
profiled warm-root replay reached 119.1 ms maximum input delay and 1017 ms
completion; one turn took 117.9 ms, including 46.8 ms of shared GC. A second
turn took 100.0 ms, including 43.9 ms GC. These inclusive times overlap other
phases and are not additive. Some group rendering and parser repair remain
indivisible. The chosen smaller preparation slices trade almost twice the
settlement time for more responsive terminal input; they do not establish a
10 ms hard callback bound.

These probes use isolated terminal Emacs; they do not measure graphical
painted-input latency, full-session loading, or all real-time streaming paths.
The original pre-implementation measurements and protocol follow unchanged.

Measured 2026-09-27 against `51a2ffe` (`fix(view): Place status above the pinned
prompt`). User-authorized investigation only; no production changes or original
session resume. Main agent executed the measurements; `/root/measurement_review`
reviewed the protocol and harness independently.

## Conclusion

**Keep the backlog item. Recent, ordinary saved transcripts still produce
noticeable input stalls.** The problem is not restricted to the old multi-megabyte
hidden ToolCall metadata capture.

Existing scheduled projection helps, but is not a complete fix. With equally
cold caches, selecting that existing path reduced the median maximum input delay
from 294 to 168 ms for the large root and from 263 to 184 ms for the agent.
Warm scheduled refresh still reached about 131 ms on the large root.

Prioritize the synchronous work before the first batch yield, its allocation/GC,
and expensive individual turns. Expanding use of the scheduler is worth a
focused implementation experiment, but merely changing callers will not establish
a tight input-latency bound. These results do not justify a new persisted summary
format or subprocess architecture.

## Frozen inputs

These are transcript sizes, not whole-session disk usage or displayed text size.
All bytes came from the authoritative published manifest and passed SHA-256
checks before copying. No serialization or rewriting was used.

| Label | Session and logical artifact | Raw bytes | Canonical segment/turn diagnostic | Rendered history characters |
|---|---|---:|---|---:|
| Control | `2026-09-25T20-48-9f6729a8d04c`, `segment-0001.chat.org` | 783,101 | 81 spans, 8 turns | 12,249 |
| Large root | `2026-09-25T20-50-e6b129234a36`, `segment-0005.chat.org` | 2,449,700 | 351 spans, 17 turns | 27,876 |
| Large agent | Same large session, `agents/verifier--2026-09-26T08-46-02--d409da12.chat.org` | 1,841,820 | 194 spans, 6 turns | 20,553 |

The control grew from the previously inspected 573,759 bytes to 783,101 before
capture. Every reported run uses the latter frozen version. Largest source turn
diagnostics were 532,127, 1,756,237 and 820,846 characters respectively. These count
source text including hidden content, not visible prose. Diagnostic grouping was
performed after the timing window.

Exact publication paths and hashes are in `results.json` and the private capture
manifest `.scratch/transcript-redraw-2026-09-27/manifest.json`.

## Primary results

Three serial, fresh-process repetitions per condition. "Maximum input delay" is
the median of each run's maximum keyboard-to-command-entry latency. Parentheses
give the range of the three maxima. It is not keystroke-to-screen latency.
Completion is projection settlement, not final painted redisplay.

| Input | Path | Median completion | Maximum input delay, median (range) | Median GC time during work |
|---|---|---:|---:|---:|
| Control | Cold synchronous projection | 75.6 ms | 77.8 ms (76.6-79.4) | 0 ms |
| Control | Warm scheduled refresh | 193.5 ms | 13.4 ms (11.6-14.4) | 0 ms |
| Large root | Cold synchronous projection | 293.8 ms | 294.2 ms (268.6-295.2) | 92.4 ms |
| Large root | Warm scheduled refresh | 507.6 ms | 130.8 ms (116.1-131.3) | 100.0 ms |
| Large agent | Cold synchronous projection | 263.7 ms | 263.2 ms (255.9-265.4) | 43.0 ms |
| Large agent | Warm scheduled refresh | 313.0 ms | 61.2 ms (60.4-65.2) | 39.3 ms |

Scheduled completion includes the ordinary **150 ms debounce**, plus callback
gaps. A slower total completion can therefore coexist with better input response.
The primary cold/warm comparison changes both cache warmth and scheduling; it is
not proof that scheduling alone accounts for the entire latency improvement.

### Controlled cold-cache scheduling comparison

An additional three runs per large input selected the existing scheduled full
refresh on a fresh view, without a warming full render. This is an experimental
caller choice, not current cold-opening behavior; no production code changed.

| Input | Cold scheduled median completion | Maximum input delay, median (range) |
|---|---:|---:|
| Large root | 518.3 ms | 167.8 ms (165.8-167.9) |
| Large agent | 443.8 ms | 183.8 ms (181.0-184.9) |

This isolates a useful but incomplete scheduling benefit. It also shows why the
61 ms warm-agent result should not be advertised as achievable simply by changing
the cold-opening caller.

## Attribution

Six additional instrumented replays (one per primary condition) were excluded
from the latency medians. Advice recorded phase start/duration and inclusive GC
time, and a post-GC hook correlated collection completion with key delays.
**Nested phase times overlap and must not be added together.** Single attribution
runs locate work; they are not precise stable percentage estimates.

| Instrumented case | Segmentation | Turn grouping | Full source plan | Largest turn render | Total fontification |
|---|---:|---:|---:|---:|---:|
| Large root, cold | 28.9 ms | 80.0 ms, including 39.6 GC | 110.5 ms | 100.7 ms, including 40.9 GC | 3.0 ms |
| Large root, warm scheduled | 28.0 ms | 22.9 ms | 51.0 ms | 55.7 ms | 3.9 ms |
| Large agent, cold | 16.0 ms | 25.4 ms | 41.5 ms | 152.8 ms, including 40.3 GC | 89.2 ms |
| Large agent, warm scheduled | 16.0 ms | 14.0 ms | 30.1 ms | 61.9 ms, including 38.7 GC | 7.1 ms |

In the warm-root attribution replay:

- The initial batch setup ran from roughly 150.2 to 225.8 ms after scheduling:
  **75.6 ms**, of which 51.0 ms was source planning.
- A collection completed at about 268.1 ms, after setup and before input resumed.
  Its duration was approximately 42 ms.
- The worst key was sent at 153.7 ms and handled at 273.1 ms: **119.4 ms delay**.
- A further 4.2 ms batch callback ran immediately before that key was dispatched.

Thus, the remaining warm-root stall is not simply one slow later turn callback.
The initial synchronous preparation and work around its GC boundary already
consume the input budget. The current scheduler dynamically raises GC thresholds
inside its flush; this replay preserves that behavior rather than normalizing the
two paths to identical internal GC bindings.

The cold-agent profile also exposes a substantial individual-turn/fontification
cost. Additional batching only between turns cannot divide that operation.

## Method and validity

- Emacs 31.1, source-loaded Mevedel; actual gptel request library was
  `.eask/31.1/elpa/gptel-20260906.334/gptel-request.elc`. Loaded library paths and
  hashes are recorded in every raw result and the shareable primary results.
- Eask bytecode cleanup ran before testing. The ERT launcher supplied temporary
  HOME/XDG roots; each terminal child used `-Q`, inherited Eask dependencies,
  and an isolated default directory. No user init or real session registry was
  loaded. Network creation in the child was explicitly rejected.
- Markdown and Markdown-inline tree-sitter grammars were available from copied
  local grammar libraries. Both were active in every primary run.
- Baseline GC settings were 32 MiB / 0.1; the native scheduled-render GC guard was
  left intact. An explicit GC occurred before readiness, not during timing.
  This is a fresh-process baseline, not a multi-hour editor heap experiment.
- Real PTY `x` keys were sent every approximately 10 ms, beginning 250 ms before
  work and continuing through completion and a recovery interval. All key
  timestamps are retained. Maximum sender lateness in the 18 primary runs was
  1.07 ms. No other benchmark ran concurrently.
- Root keys entered the real multiline composer. The agent view was read-only;
  keys entered a separate visible text editor buffer. No fake editable agent
  composer was introduced.
- Cold timing excludes file loading and canonical property restoration. Restore
  time was recorded separately: median across primary runs was about 9.0 ms for
  control, 38.9 ms for root, and 21.1 ms for agent. These are not full resume costs.
- Warm timing uses the normal positive-delay full-refresh scheduler. The selected
  archived root bytes are the replay's current display source, so the test does
  not accidentally measure the historical-view chrome-only refresh path.
- Settlement checks include pending render intent, batch work, preparation jobs
  and timer, and active/queued projection ownership. Unfinished placeholders,
  render warnings/errors, abnormal child exit, and missing input samples fail.
- Correctness checks run after timing/GC counters freeze. Every reported run
  preserved every typed character, point-at-end, source text, and history/source
  mappings across a reference synchronous rerender. Cross-process cold, warm,
  cold-scheduled and instrumented history and source-map hashes also agree per
  fixture. This is stronger than same-view rerender idempotence alone.
- The result records contain p50/p95/max and >50/>100 ms counts for all keys,
  keys sent during work, and latency intervals overlapping work. In all 18
  primary runs, the overall maximum was also a work-overlapping maximum.
- Final integrity checks matched all captured production source hashes and all
  three original immutable transcript hashes. No measurement child remained.

### Pilot issue

The first large-root pilot finished rendering but timed out during finalization:
the inherited control-G end marker could be consumed as Emacs quit instead of
dispatching the local command. Its owned child was terminated and reaped. The
runner now uses an ordinary `z` command, requires clean child exit, and verifies
all input counts. That failed pilot and all successful pilots are excluded from
the reported statistics; the complete primary matrix was run afterward.

## Verification

- 18 primary replays, 6 controlled cold-scheduled replays, and 6 instrumented
  replays passed their Eask-owned checks.
- `summarize.py` revalidated the reported matrix and cross-mode history/source
  map equivalence, producing `results.json` with raw-result paths and hashes.
- Independent read-only review recalculated the tables, input statistics, GC
  timeline, raw-file hashes, population separation, and cross-mode equivalence.
  It confirmed the scoped report; its minor sender-lateness rounding correction
  is incorporated above.
- After another `npx @emacs-eask/cli clean elc`, the existing
  `test/test-mevedel-view-render-batch.el` and `test/test-mevedel-view-prepare.el`
  suites passed **52/52** tests, with no unexpected diagnostics.
- Production files and backlog are unchanged. Only this working-material
  directory is new in Git; private fixtures/raw results remain under `.scratch/`.

## Limits and next decision

This round establishes current-source, settled-transcript input stalls. It does
not measure graphical keystroke-to-screen latency, full session resume, live
streaming reconciliation, source changes during rendering, expansion of large
tools, a large accumulated editor heap, external media hydration, or the old
pathological capture. The existing tests cover some relevant preservation
contracts, but are not performance evidence for those omitted workloads.

Before implementation, pick a response-latency target and confirm a consequential
case in the user's graphical configuration. The measured first targets are:

1. Reduce or yield within initial source preparation and its allocation, retaining
   canonical ownership/trust classification.
2. Evaluate using existing scheduling for cold root/agent opening where caller
   semantics permit it; the cold-cache comparison shows benefit but not a bound.
3. Split or defer expensive work within a large turn, especially cold response
   fontification, while preserving drafts, live text, source maps and disclosures.

Keep legacy summary/payload storage and subprocess offload as evidence-dependent
options, not prerequisites for this work.

## Reproduction

`capture.py` creates a new private snapshot directory and intentionally refuses
to overwrite it. Published heads can change; the saved manifest identifies the
exact inputs used here. For the existing frozen fixtures:

```bash
npx @emacs-eask/cli clean elc
PROBE_LABEL=root PROBE_MODE=cold PROBE_TRIAL=new-trial \
  npx @emacs-eask/cli test ert \
  .mevedel/shared/transcript-redraw-2026-09-27/launch-test.el
```

Labels: `control`, `root`, `agent`. Modes: `cold`, `warm`, `cold-scheduled`.
Use a fresh trial name; output directories cannot be overwritten. Set
`PROBE_PROFILE=1` for an instrumented run. Run conditions serially. Reports and
harness are shareable; fixture bytes and raw terminal recordings are local-only.
