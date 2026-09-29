# Spinner native CPU analysis — 2026-09-28

Task: `spinner_cpu_analysis`; CPU analysis only. Main owns telemetry correlation; another agent owns validation. No live profiler changes, product edits, tests, or Goal operations were performed.

## Result

The long capture is not a clean spinner benchmark. Its largest retained exact stack is a **C-source directory-completion prompt**, not animation: 10,887,623 counts (51.22% of the displayed-report denominator). Spinner view-state/anchor work is a separately supported hotspot in both runs. Publication is present but much smaller in retained CPU counts. Neither the collapsed timer entry nor the long prompt's inclusive ancestry establishes a continuously blocking timer callback.

## Inputs, format, units, and denominators

Inputs are the two `profiler-cpu-profile.el` and sibling `profiler-cpu-report.txt` files beneath `.mevedel/sessions/2026-09-27T16-24-5ca69305960c/diagnostics/`. Below, **early** means `run-20260927T174115-f917e96e`; **primary** means `run-20260927T181246-1b68ad79`.

Read `docs/index.md` and `docs/telemetry.md`, especially `docs/telemetry.md:244-284,303-321`. Parsed profiles with `read`, not evaluation/loading, in isolated `emacs -Q --batch`; validated profile shape, stack/count types, and absence of trailing forms. Installed Emacs is 31.1, `/usr/share/emacs/31.1/lisp/profiler.el.gz`. Its profile-format version is **28.1**, not the capturing Emacs version. The native object has type, aggregate backtrace hash table, one retrieval timestamp, and diff flag. **There are no per-sample timestamps, sequence, run-start timestamp, sampling interval, or selected timer clock in this format.** Retrieval timestamps are early `2026-09-27T18:03:02+0200`, primary `2026-09-28T09:35:11+0200`.

Units require care: installed `profiler.el:117-122` describes weighted counts using the sampling interval, but the corresponding Emacs 31.1 C implementation explicitly records **interrupt counts**, with `1 + timer_getoverrun`, not milliseconds or nanoseconds. Verified against upstream tag `emacs-31.1`, `src/profiler.c:266-268,368-380` (https://raw.githubusercontent.com/emacs-mirror/emacs/emacs-31.1/src/profiler.c). Call them **recorded sample/interrupt counts**, not exact independent samples or duration. Installed default interval is 1,000,000 ns (`profiler.el:39`), but the actual capture interval/clock is not retained, so this analysis does not convert counts to CPU seconds. Matching upstream source does not attest to every distro C patch. Timer setup prefers CPU clocks, with fallbacks (`profiler.c:414-449`); elapsed wall time, system suspension, subprocess wall time, and CPU counters must not be equated.

| Quantity | Early | Primary |
|---|---:|---:|
| Distinct native stack keys | 6,448 | 8,261 |
| All native counts, **T** | 797,497 | 32,835,081 |
| All-nil stack counts (omitted by rendered report) | 514,461 (64.51% T) | 11,580,034 (35.27% T) |
| Displayed-report denominator, **R = T − empty** | **283,036** | **21,255,047** |
| Automatic GC | 64,952 (22.95% R) | 2,168,562 (10.20% R) |
| Discarded Samples | 27,748 (9.80% R) | 2,187,391 (10.29% R) |
| Remaining named-stack counts, R − GC − discarded | 190,336 | 16,899,094 |
| Full 64-frame stacks, lacking nil terminator | 20,679 (7.31% R) | 13,414,482 (63.11% R) |

All percentages below use **R**, unless marked “% timer.” Empty stacks are unattributed, not proven idle and not assigned to printing or recovery. GC is exported without caller context; it cannot be charged to a particular allocator. Discarded counts are evictions of lower-count backtraces on table overflow, not idle time or a product function (`profiler.c:204-260,580-587`). Retained stack attribution therefore preferentially preserves repeated/heavy stacks. No memory profile is available here.

## Why the saved timer total is not an actionable hotspot

Installed `profiler.el:285-385` heuristically attaches a full/truncated stack to one “best matching” parent, explicitly acknowledging that it cannot determine the correct distribution among possible callers. This is particularly consequential with 63.11% R in full-depth primary stacks. Direct/raw inclusive counts below require the frame to occur in the saved vector; they do not invent ancestors and count each frame at most once per stack. “Exclusive” means the first retained frame, not source-level instruction timing.

The saved reports show early timer **74,680 (26.39% R)** and primary **14,512,492 (68.28% R)**. Rebuilding with the installed profiler from the normalized data gives **70,084** and **14,397,509** respectively, while preserving both total denominators exactly. These are **not exact reproductions of the saved tree**. The save implementation renders the runtime-object profile before serializing `profiler-fixup-profile` (`mevedel-telemetry.el:988-998`); fixup replaces runtime functions with names (`profiler.el:106-141`). Runtime identity/parent-selection differences are a plausible explanation, not a proven reconstruction of the original choices. Do not silently substitute one tree for the other.

For a quantitative expansion, the following **disjoint, ordered partition of the rebuilt primary root-timer branch** sums exactly to 14,397,509. It describes that reconstruction, not extra directly observed ancestry. Priority follows table categories: completion leaf, spinner, control/lease, progress, publication, redisplay, other view, heartbeat, other introspection, remainder.

| Rebuilt timer work category | Counts | % timer |
|---|---:|---:|
| C-source prompt, native `completing-read-default` leaf | 10,887,623 | 75.62% |
| `mevedel-view--spinner-tick` subtree | 1,074,377 | 7.46% |
| Redisplay, excluding preceding categories | 812,403 | 5.64% |
| Control-transfer refresh / lease renewal | 558,712 | 3.88% |
| Other timer/wrapper work | 279,953 | 1.94% |
| Other work under source introspection | 218,568 | 1.52% |
| Other view/render work | 214,118 | 1.49% |
| Execution progress | 204,986 | 1.42% |
| Lag heartbeat | 109,352 | 0.76% |
| Publication-prefix subtree | 37,417 | 0.26% |

In the early rebuilt timer branch, spinner is 36,639/70,084 (52.28%), execution progress 14,520 (20.72%), control/lease 8,879 (12.67%), other view 3,193 (4.56%); the source-introspection branch is absent. Primary raw vectors actually containing a timer symbol or native timer marker total **2,545,445 (11.98% R)**; most extra reconstructed timer ancestry is inferred through truncation. Advice `mevedel-telemetry--lag-time-callback` wraps callbacks: its inclusive cost is not telemetry overhead.

## Direct stack evidence: overlapping inclusive categories

These rows are **not additive**. Counts are raw-vector membership, with no parent stitching.

| Frame/category | Early count (% R) | Primary count (% R) |
|---|---:|---:|
| Source introspection `mevedel-tool-introspect--source` | 0 | 12,630,837 (59.43%) |
| Redisplay C frame, including descendants | 80,444 (28.42%) | 2,759,186 (12.98%) |
| Spinner tick, including descendants | 40,165 (14.19%) | 1,069,940 (5.03%) |
| Control-transfer refresh | 5,828 (2.06%) | 525,376 (2.47%) |
| Execution progress emission | 14,638 (5.17%) | 203,687 (0.96%) |
| Lag heartbeat | 925 (0.33%) | 117,802 (0.55%) |
| Any `mevedel-session-publication-` frame | 1,945 (0.69%) | 68,958 (0.32%) |
| Publication OR artifacts prefix OR agent-conversation write | 5,195 (1.84%) | 159,409 (0.75%) |

**Introspection:** heaviest exact stack, shortened root → leaf: pipeline capture-coverage → pipeline run → handler → `mevedel-tool-introspect--source` → `find-definition-noselect` → `find-function-noselect` → `find-function-C-source` → `read-directory-name` → `read-file-name` → Vertico-advised `completing-read-default`. The native completion leaf accounts for 10,887,623 on this exact stack. Another 723,925 exact-stack counts end in redisplay above that prompt. Timer/spinner/lease activity also runs inside its recursive command loop, so inclusive introspection is not exclusively lookup computation. The existing recovery account independently describes this prompt and hotload: `.mevedel/shared/introspection-source-recovery-20260928.md:8-17,35-53`.

**Spinner/anchors:** primary spinner vectors include window-state preservation for **899,356 (84.06% of spinner)** and either `position-render-anchor` or `render-anchor-position` for **649,487 (60.70%)**. Leaf `next-single-property-change` contributes **256,433**, `min` **181,898**: together **438,331 (40.97% of spinner)**. Early equivalents are 39,039 window preservation, 35,202 anchor lookup, and 24,481 combined property/min leaves (60.95% of spinner). Representative path: spinner-tick → preserve-user-view-state → preserve-window-state → position-render-anchor → render-anchor-run-end → min → next-single-property-change. This supports investigating repeated anchor/property scans rather than blaming timer dispatch itself. It does not establish cost per tick, before/after improvement, or current-code regressions across hotloads.

**Other rendering:** primary raw inclusive `insert-rendered-tool` 121,320; `render-expanded-body` 120,776; `render-assistant-turn` 91,183; `refresh-tool-row-now` 72,164; `flush-scheduled-render` 36,116. Rebuilding raises the last to **136,933** and tool-row refresh to **115,455**, illustrating sensitivity to inferred outer frames in deep rendering stacks, not proving the inferred attribution. Sticky-prompt-line has 185,751 inclusive, with representative leaf `vertical-motion`; rendering also includes ordinary editor/modeline redisplay, not only mevedel. These totals do not measure callback tail latency; main's timer telemetry is needed for that.

**Publication:** primary publication-prefix leaves include `write-region` 17,720, `secure-hash` 13,877, `file-relative-name` 8,111, `insert` 5,540, `call-process` 5,054. `publication-publish` is 54,266 inclusive, critical batches 44,013, drain 42,937; these overlap. Small CPU shares do not rule out publication wall latency, remote I/O, or discarded/truncated attribution.

**Heavy exclusive frames across the whole primary profile:** native completion 10,982,725; redisplay C 2,027,601; `call-process` 506,028; `next-single-property-change` 295,497; `save-current-buffer` 227,774; `min` 183,001; `file-truename` 129,169; `cconv-make-interpreted-closure` 95,323. GC/discarded/empty are separate buckets, not functions. Early: redisplay C 56,640, property change 17,866, native completion 11,791, file-truename 8,204, min 7,302, call-process 6,256. For context, primary execution artifact-address construction has 163,061 inclusive; control/lease stacks visibly descend into transport/process-file/call-process. These are CPU-stack observations, not measurements of child-process CPU or wait time.

## Contamination, verification, and handoff

- The editor was simultaneously used for normal work, debugging, hotloads, and other sessions. Profiles are global, not session-filtered or version-segmented. Anonymous function labels do not identify stable callbacks across reloads/runs.
- User/main identify the huge recovery Eval settlement-object print around 07:03 Sep28; the existing recovery report confirms it at lines 49-53. Native data cannot isolate that interval or subtract its printing/GC/rendering costs. Retained `prin1`/`prin1-to-string` leaves (3,308/1,828 primary) do **not** bound total recovery overhead: attribution may be missing, discarded, in empty stacks, or in downstream work. Likewise, no CPU count can be assigned to an individual lag or suspension using this aggregate alone.
- Isolated batch parsing and installed-profiler reconstruction completed for both files. Independent arithmetic checks confirmed **native total minus empty = saved report row sum = rebuilt tree sum**, and one nonzero self-node per nonempty saved stack (6,447 / 8,260). No product tests were run or requested. Rebuilding saved top-level attribution is explicitly inexact, not a verification pass claim.
- Disposable reproducibility material: `.scratch/spinner-cpu-analysis-20260928/{parse.el,tree.el,summarize.py,detail.py}`, decoded JSON, `*-summary.txt`, `*-mevedel.txt`, `*-tree-detail.txt`, and `partition.txt`. Installed profiler source was decompressed there as `installed-profiler.el`; matching upstream C source is `profiler-31.1.c`. Native profiles remain untouched.
- Analysis is complete for the available aggregate data. Exact event-time attribution, current-version improvement, interval-specific recovery overhead, and the original runtime-object call tree remain unknowable from these files alone.
