# Session performance capture

Capture completed and saved at 10:58 on 2026-10-03. Both native profilers
are stopped, and `mevedel-telemetry-profiler-fail-on-prompt` is restored to t.
The original capture was observational. The implementation and controlled replay
results below were added after the capture; unrelated diagnostic advice was not changed.

## Findings

There are measurable responsiveness problems during active work, independent
of the user's absence and machine sleep. The strongest optimization lead is
animation visibility checking; five multi-second stalls are directly located
in scheduled view rendering. The capture establishes the stalls and CPU
hotspot, but does not prove that the visibility checker caused each stall.

All times below are local, UTC+02:00, on 2026-10-03.

| Interval | Classification | Evidence |
| --- | --- | --- |
| 07:27:14.590–07:31:19.282 | First profiled request: 4m 04.692s | Successful; zero recorded delays above 200 ms |
| 07:31:19.280–09:37:47.607 | Waiting for plan approval: 2h 06m 28.327s | `interaction-opened` / `interaction-closed`, kind `plan`; excluded from request latency |
| 09:37:47.840–10:46:52.374 | Second profiled request: 1h 09m 04.535s | Successful; 70 delays above 200 ms, 12 above 500 ms, five above 1 s |
| After 10:46:52 | Post-request work and time before capture stop | Not added to request duration |

The user reported a long absence and sleep within the capture. The telemetry
does not identify the exact OS suspend/resume times. The long plan wait is
not treated as a freeze. All 12 individually logged stalls occurred in the
second request and showed CPU activity consistent with active work, not a
suspend gap. Six of the 70 counted delays had input pending; four of the five
multi-second stalls did.

### Confirmed multi-second rendering stalls

| Time | Event-loop delay | CPU since previous heartbeat | GC in that interval | Input pending | Telemetry sequence |
| --- | ---: | ---: | ---: | --- | ---: |
| 09:52:49.613 | 2.571 s | 2.646 s | 0.126 s | Yes | 8006 |
| 09:52:57.743 | 6.430 s | 6.406 s | 1.076 s | Yes | 8061 |
| 09:53:26.191 | 4.072 s | 4.153 s | 0.648 s | No | 8522 |
| 09:54:25.759 | 3.095 s | 3.180 s | 0.537 s | Yes | 9167 |
| 09:55:05.034 | 5.684 s | 5.759 s | 0.498 s | Yes | 9498 |

All five name `mevedel-view--flush-scheduled-render` as the slowest timer,
with callback durations 2.657, 6.432, 4.170, 3.194 and 5.676 seconds,
respectively. The five delays total 21.852 seconds. GC contributes but does
not explain most of their duration. CPU covers the entire heartbeat interval,
so it can exceed the delay beyond the scheduled heartbeat time.

### CPU and allocation evidence

The native CPU report contains 1,037,275 attributed samples. The following are
inclusive shares unless stated otherwise; overlapping rows must not be added.

| Path | Share of attributed CPU samples |
| --- | ---: |
| `mevedel-view--animation-span-in-window-p` | 52.3% inclusive; 39.4% self |
| `mevedel-view--resume-on-horizontal-redisplay` | 40.0% |
| `mevedel-view--spinner-tick` | 8.6% |
| `mevedel-view--sticky-prompt-line` | 6.7% |
| Automatic GC | 15.9% |
| `mevedel-telemetry-record` | 0.19% |
| `mevedel-session-artifacts-save` | 0.11% |

Animation visibility checking is a substantial CPU hotspot even though the
sampled implementation was byte-compiled. The implementation follow-up below
removes its redisplay and scroll probes. Aggregate profiles have no timestamps,
so these shares cannot be restricted retrospectively to either request or
matched to individual stalls.

The memory profile reports approximately 64.56 GB of cumulative allocation,
not retained memory or evidence of a leak. About 53.3% passes through
`cl-print-object`; backtrace printing appears in roughly 14.2%. Temporary
`my/crumb-trace` advice appears in sampled render stacks, accounting for 4.54%
of allocation and 0.26% of CPU where that name remains in the captured stack.
It was absent when inspected after capture. Server evaluation also contributes
13.1% of the memory report. These diagnostic activities were not installed by
this monitoring setup and make allocation totals unsuitable as a clean normal
workload baseline. Truncated stacks can omit the originating caller.

### Other measured costs

Across both profiled requests and their recorded tails:

| Operation | Count | Median | p95 | Maximum |
| --- | ---: | ---: | ---: | ---: |
| Session save | 78 | 69 ms | 138 ms | 168 ms |
| Session publication | 322 | 5 ms | 53 ms | 110 ms |
| Logged control programs | 172 | 26 ms | 62 ms | 73 ms |

Control-program telemetry only records operations at least 20 ms long;
these are statistics of the logged subset. Persistence durations can nest.
They do not explain the observed multi-second rendering freezes.

Three successful compactions took 95.82, 86.24 and 154.25 seconds, including
continuation-model requests of 94.81, 85.97 and 153.12 seconds. Two were root
compactions and one belonged to an agent. These are model-wait costs, not
evidence of an editor freeze; concurrent durations must not be added to
estimate the root request's critical path.

### Capture limits

This was an observational capture, not a controlled regression benchmark.
The CPU/allocation profiles cover the entire Emacs process, including the
waiting interval and unrelated actions. Both profiling and detailed telemetry
add overhead. Git HEAD and dirty state changed during the monitored coding
work; loaded gptel artifact hashes matched at both boundaries. A clean replay
of scrolling and streamed rendering on this large session, without transient
backtrace/debug advice, was used for the implementation follow-up below.
The original capture alone cannot prove that each recorded freeze is gone.

Analysis read saved Lisp records without evaluation in a separate batch Emacs.
Native profile stacks were summed once per function per stack for inclusive
shares. Empty CPU stacks were excluded, matching the native report's total.
Checks confirmed both CPU and allocation sums equal the saved native report
totals, and the individual lag events match the request summary's count,
maximum and >1-second count. p95 uses the sorted value at floor(0.95*(n-1)).

## Implementation follow-up

The implementation plan was carried out in this order:

1. Replace glyph/pixel visibility probes with overlap against each attended
   window's last redisplayed buffer range. Remove the self-sustaining
   pre-redisplay hook, global horizontal/pixel scroll advice, and unused
   changing-prefix metadata. Keep vertical visibility, focus, power policy,
   independent elapsed-time updates and existing spinner themes.
2. Bound the stored, filtered prompt preview to 512 characters including the
   ellipsis. The complete prompt remains available in its transcript source.
3. Inspect the temporary diagnostic advice. `my/crumb-trace` was already absent
   from the running Emacs and its configuration source; there was nothing left
   to remove. Its allocation share cannot establish that it caused most of the
   cumulative 64.56 GB, contrary to the earlier quoted interpretation.
4. Replay the saved transcript in a separate graphical Emacs through scheduled
   live rendering, with actual queued keyboard input. Profile remaining costs.
   This exposed repeated whole-turn reconstruction when restoring any expanded
   disclosure. Preserve an intact final render unit across restoration of an
   earlier section; retain the conservative rebuild for a rewritten unit.
5. Add regression coverage, run isolated Eask tests and warning-free compilation,
   review the changes against the task and repository contracts, and commit the
   complete change together.

The retained-tail regression first failed on an earlier expanded thinking block.
The final unit is now captured before restoration, including a witness for its
first character. Restoration must leave that character and one uninterrupted
source-property run intact. This matters because an advancing start marker alone
can accept the trailing fragment of a rewritten unit. Failure or a rewritten
prefix/interior invalidates retention. Temporary markers are released even when
restoration fails.

The visibility tradeoff is deliberate: a spinner clipped sideways can continue
animating while its buffer range overlaps the window. This costs a decorative
display-property write, without expensive glyph-position queries on every frame.
ADR 0119 records the reversal and its measurements; `docs/view.md` describes the
current behavior.

### Controlled replay protocol

The baseline is the original byte-compiled view modules from commit
`be23fd1`; the treatment is the changed, byte-compiled modules. Both use the same
isolated Eask dependencies and a separate graphical Emacs with `-Q`. No model
request or network replay is involved, and no user session files are modified.

The replay alternates three before/after pairs:

- 2,000 visibility checks on a horizontally clipped label and elapsed row,
  with a 20,000-character prompt preview in an evaluated header line. It counts
  real `posn-at-point` calls against a real graphical glyph matrix.
- Load a copy of the saved 1,624,875-byte transcript segment, restore its source
  properties, locate the final user-turn boundary, and initialize an active
  streamed turn. Expand eight disclosures, then append 12 response chunks.
  Each scheduled render queues a real `self-insert-command`; a post-command hook
  measures input handling latency. Each update asserts the expected multiline
  composer text (beginning with `>`) and cursor position.

Foreground attendance is forced for the replay so window-manager focus does not
silently skip work. Modelines, the user's complete init configuration and the
temporary backtrace advice are intentionally absent. Full initial transcript
reconstruction is setup, not a streamed-update timing. The first update after
expansion is included and must perform a complete turn rebuild; later updates
can reuse the intact response tail. Measurements are unprofiled except for the
separate diagnostic CPU run. p95 uses floor(0.95*(n-1)).

Local disposable harness and results are in
`.scratch/session-responsiveness/`: `gui-test.el`, `gui-replay.el`,
`gui-result.json`, and the baseline/compiled modules. Invoke with
`npx --offline @emacs-eask/cli test ert .scratch/session-responsiveness/gui-test.el`
after copying warning-free bytecode into its `compiled/` directory and cleaning
the source tree through Eask. These local execution artifacts are not committed.

### Replay results

| Measure | Before | After |
| --- | ---: | ---: |
| 2,000 visibility checks, median of 3 runs | 976.91 ms | 0.256 ms |
| Glyph-position queries per visibility run | 5,000 | 0 |
| Scheduled render + redisplay, median of 36 updates | 104.49 ms | 5.86 ms |
| Scheduled render + redisplay, p95 | 113.87 ms | 97.34 ms |
| Scheduled render + redisplay, maximum | 125.38 ms | 98.26 ms |
| Queued-input latency, median of 36 updates | 104.57 ms | 5.92 ms |
| Queued-input latency, p95 | 115.19 ms | 97.42 ms |
| Queued-input latency, maximum | 125.48 ms | 98.34 ms |

The slower after-change samples are the three initial complete-turn rebuilds
after expanding disclosures, included in all statistics. Every subsequent update
can retain the stable prefix. The first visibility-only fixes reduced repeated
rendering to about 96 ms; preserving the intact final unit accounts for most of
the further improvement. The 2,000-check loop magnifies the old visibility cost
and is not a claim that each ordinary frame previously took one second.

A separate CPU capture over 100 after-change streamed updates attributed zero
of 1,365 nonempty samples to the visibility checker (629 samples passed through
the scheduled render). Its sampled share was below the proposed 1% limit;
sampling cannot resolve the check's now very small cost precisely. Profiled
timings are excluded from the table.

All replayed scheduled renders were below the proposed one-second limit;
all queued input was handled within 100 ms. The replay did **not** reproduce
the original 2.57–6.43-second freezes under the user's complete configuration,
so these results establish the removed hotspot and improved controlled latency,
not proof that every possible live-session stall is eliminated. No unexplained
replay stall remains. Session persistence was not changed because its measured
costs did not support it as a cause of those freezes.

Regression tests cover glyph-free visibility, attended windows, sideways-clipped
animation across themes, bounded mailbox-filtered prompt previews, unchanged
earlier disclosures across appends, conservative handling of rewritten tails,
prefix-replacement marker safety, and multiline composer/cursor preservation.
The original global scroll advice and pixel-policy tests were removed with
their superseded implementation. Compilation completed without warnings.
The final full isolated Eask suite ran 9,123 cases in 248.35 seconds with zero
unexpected results and 22 conditional skips (9,101 passed). Its report is
`.scratch/test-suite-performance/20261003-122354/summary.json`.
Standards and specification review findings were addressed before committing.

The running user Emacs was not hot-reloaded. A fresh Emacs load applies the
removed advice/hooks cleanly; the saved diagnostic capture remains available.

## Session-backed follow-up

The transcript replay above used detached test buffers, so the agent
transcripts, archived segments and publications it references were
unreachable. A second replay restored a copy of the real session (read-only;
lease held by the live Emacs), loaded segment 1 -- the live turn of the stalled
request, before its 09:58 rotation -- as the chat transcript, expanded the live
turn's collapsed sections and streamed its last 71 KB back in 74 property-run
chunks. The stalls happened while three subagents ran in parallel, just before
that rotation, in a 700 KB live turn.

Profiled cause, after the visibility fix:

| Cause | Effect per streamed update |
| --- | --- |
| Expanded hook audits rendered collapsed, then restoration re-expanded them | Retained tail invalidated: the whole 700 KB turn rebuilt |
| Breadcrumb deduplication re-read every older archive per fresh breadcrumb | 2-3 archive reads with Org setup; 57% of render samples |
| Each Bash row reparsed every audit record of the 1.6 MB transcript | 22% of render samples |
| A tool block followed by its merged hook audit failed the exact-span cache test | Nearly every tool reparsed |
| In-flight boundary recovery walked the view per character, result unused when retained | A scan from the top of the view on every update |

All five are fixed, with regressions that fail without the change. Two smaller
costs were also removed: restoration's quadratic state lookup and image
decoration resolving every bare path (it now checks the extension first);
one projection resolves each path link once.

Graphical replay, alternating committed and changed modules (render plus
redisplay; queued input matched within 5 ms):

| Folds | Before median / p95 | After median / p95 |
| --- | ---: | ---: |
| Every live section expanded (73) | 0.360 / 0.397 s | 0.071-0.072 / 0.098-0.108 s |
| Default | 0.097 / 0.128 s | 0.028 / 0.062 s |

Full isolated suite: 9,134 cases, 0 unexpected, 22 skipped; compilation clean.

Not changed: the remaining expanded-case cost is re-inserting the growing
activity group's rows, below the 0.15 s render debounce. The single 1.2 s
`mevedel-transport-run-when-idle` pause at 07:12:03 (76 ms CPU, so blocking
I/O in one deferred turn step) appears once across every saved session's
telemetry and predates the profiler; it was not reproduced or attributed.

## Original setup record

Session: `2026-10-03T07-07-aa7dca1083e9`.
Live Emacs PID: 14897.
Capture started: 2026-10-03 07:25:59 +0200.
Run: `run-20261003T072558-7160b1be`.

Built-in CPU and allocation profiling and detailed session telemetry are
enabled. CPU sampling interval is 1 ms; stack depth is 64. Existing event-loop
probes run during requests and for 120 seconds afterward, count stalls above
200/500/1000 ms, and emit individual events above 500 ms, with CPU, GC and
slowest-timer measurements. Raw gptel debug logging remains disabled.
The native profiles cover the whole Emacs process; unrelated editor activity
can contribute. Profiling adds overhead; a later unprofiled comparison may be
needed to quantify borderline findings.

`mevedel-telemetry-profiler-fail-on-prompt` was changed from t to nil to permit
normal user interaction. Restore it to t after stopping capture. No product
source was changed. The inspected transcript parser and telemetry functions
were compiled. At capture start: 81 GCs, 5.415269929 cumulative GC seconds.

Telemetry is in the session's `telemetry-log.el`. Native profiles and reports
are saved on stop under `diagnostics/run-20261003T072558-7160b1be/` in that session.
To stop, save, and restore the prompt setting in the running Emacs:

```elisp
(unwind-protect
    (mevedel-telemetry-profiler-stop)
  (setq mevedel-telemetry-profiler-fail-on-prompt t))
```

Pre-capture evidence: the previous request's lag summary at 07:11:58 reported
zero stalls above 200 ms. Its post-settlement tail at 07:12:03 recorded one
1247 ms delay, 76 ms CPU, no GC, and a 1228 ms callback labelled
`closure:mevedel-transport-run-when-idle`. This is an observed pause, not a
root-cause finding. No user input was pending at that measurement.
