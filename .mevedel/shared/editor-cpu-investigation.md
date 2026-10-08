# Editor CPU during requests (2026-10-08)

## Root cause

On the user's Emacs 31.1 pgtk build (KDE Wayland, 2x scale, 1536x888 frame)
every wakeup ends in a redisplay that presents the **whole frame surface**,
even when nothing changed. A do-nothing 10 Hz timer in `emacs -Q`:

| frame          | emacs CPU | kwin CPU |
|----------------|-----------|----------|
| 1536x888 (2x)  | 23-36%    | ~21%     |
| 400x300        | 3%        | —        |

Rule of thumb on this frame: **~2% editor CPU + ~1% compositor per wakeup
per second.** mevedel's cost was dominated by how often it woke Emacs.

## Before / after (user's live Emacs, frame visible, 10 s windows)

Request held open by a local mock server; focus check bypassed; same view.

| scenario                                         | before          | after           |
|--------------------------------------------------|-----------------|-----------------|
| idle, no request                                 | 4% / 5% kwin    | 1% / 2%         |
| request waiting, static label                    | 8% / 7%         | 7% / 5%         |
| shimmer label (old default 60 fps / new cadence) | **70% / 34%**   | **29% / 15%**   |
| shimmer label on battery (30 fps / cadence 15)   | 62% / 31%       | 23% / 11%       |
| running `Bash` (sleep), everything static        | **42% / 11%**   | **21% / 10%**   |
| telemetry heartbeat + static label (10 Hz / 2 Hz) | 30% / 12%      | 18% / 9%        |
| **default config, watched, Bash running**        | **73% / 21%**   | **39% / 18%**   |
| same, on battery                                 | —               | 34% / 16%       |

At this first commit, continuous styles were unchanged and remained
expensive: breathe/bounce at 30 fps 64% / 30%, braille/ascii ~30% / 13%,
dots 21%, ellipsis 17%. The follow-up below lowered their cadences, and the
native presenter (see the end) removes per-frame redraws on PGTK/Wayland.

## Claude Code backend

Adapter + `claude` CLI: ~230% CPU for ~2 s at start, 5-17% while streaming.
`acp.el` drains each output chunk via a zero-delay timer (~30/s), comparable
to a fast gptel stream. A header-line `vertical-motion` walk cost 13% of
editor CPU in its requests (fixed).

## Fixes (branch `fix/cpu-wakeups`)

- Telemetry heartbeat 100 ms -> 500 ms; timers report their own lateness.
- Shimmer cadenced (Codex-style 1 s sweep every 4 s); ceilings 30/15 fps.
- Tool rows default to shimmer, sweeping "Calling TOOL" with the label.
- Spinner arming fixed after new spans and scrolling (was frozen).
- Indicators hold still while waiting for user input.
- Bash: missed-sentinel watch 10/s -> 1/s; progress 4/s only while output
  flows, else 1/s; elapsed times shown in whole seconds everywhere.
- Unattended (incl. undisplayed) views arm no render timers.
- Collection: slices 1 s apart, exponential backoff while blocked.
- Control-transfer polling skipped for non-portable sessions.
- Header/status-line per-redisplay costs reduced; decorative frames no
  longer invalidate header caches; stale animation markers detached.
- Markdown realign skips passes with nothing to lay out.
- Stream insert batch 0.04 s -> 0.2 s.

Open items are listed in `docs/backlog.md` under "Editor CPU".

## Independent review and follow-up (2026-10-08)

Reviewed `339c8702` independently in the `fix/cpu-wakeups` worktree. The
original CPU reduction is reproducible, including the spinner startup defect:
with `REARM=0`, the old view/animation modules from `3df91e1d` delivered no
shimmer callbacks during the sample; the branch animated without a focus event.
That old-spinner comparison intentionally retains the current execution and
telemetry modules; it is not a whole-package before/after benchmark.

### Standards review

The review found incomplete per-tool bank preparation and missing direct tests
for the combined scheduler, root abbreviation memo and duration formatter. Those
paths now have coverage. No additional concrete maintainability refactor was
justified. Follow-up review found no remaining concrete defect in the final diff.

### Behavior review

Reproduced and fixed these failures with tests that failed before the fix:

- A frozen shimmer overflow row kept its old count under the new underlying
  text. Restoration now regenerates changed labels at the retained phase.
- Only the first visible tool bank was prepared; four pinned banks could not
  cover distinct tool labels plus overflow and main labels. The scheduler now
  prepares every eligible label and reserves capacity for the registered set.
  Semantic preparation promotes current keys so a moved row cannot evict a
  label already visited in that pass. Frame callbacks retain cheap cache hits.
- Input pause stopped timers but did not freeze phase during visibility/theme
  refresh or an already-delivered tick. All presentation paths now share the
  freeze predicate. Actual Ask registration/removal stops/rearms scheduling.

Mixed color/glyph tool displays retain both cadences. Frozen tool-only views
adapt their destination palette/glyph fallback without advancing phase.
Native graphical checks passed fresh `display-buffer` windows, tab restoration,
a second frame, preserved multiline drafts, and offscreen timer/probe quiescence.
These used actual window hooks with focus eligibility supplied for the temporary
verification frames; they did not exercise persp-mode's own commands.

### Accepted continuous styles

The user compared current motion, 12-fps colors/half-speed glyphs, and 8-fps
colors/third-speed glyphs in their theme. They preferred the original appearance
but accepted 8 fps for breathe/bounce and half-speed glyphs for CPU savings.
Third-speed glyphs looked laggy. The implementation follows that choice: color
cycles stay 3.6 seconds, while glyph steps become 240/480/960 ms for
braille+ASCII/dots/ellipsis. Shimmer keeps its existing smooth sweep cadence.

| Scenario | Reviewed branch | Follow-up |
|---|---:|---:|
| Static label | 6.1% | 9.4% |
| Shimmer, 30-fps ceiling | 25.8% | 27.1% |
| Breathe | 63.2% | 25.7% |
| Bounce | 60.7% | 25.9% |
| Braille | 28.3% | 17.8% |
| ASCII | 25.7% | 15.0% |
| Dots | 17.8% | 12.4% |
| Ellipsis | 12.8% | 9.6% |
| Default shimmer + Bash + heartbeat | 33.8% | 34.7% |
| Same, 15-fps battery ceiling | 24.7% | 27.6% |

Percentages are editor CPU relative to one core, over one 10-second sample per
scenario. These are directional observations, not confidence intervals or
battery-life estimates. Both runs used the same live 31.1 pgtk editor, visible
1536x888 logical frame at 2x scale, local mock server and focus bypass, with no
manual rearm. The full matrix was run during isolated test work on four of the
machine's sixteen available CPUs; scheduling/load effects and lifecycle tails
were not eliminated. Background callbacks include Jinx, GC maintenance,
collection and portable-session lease/transfer work. The compositor measures the
whole desktop and had a changing baseline. Shimmer/default differences are not
claimed as improvements. Settings and windows were restored after measurements;
compiled follow-up view code remains loaded for this editor session.

The original measurement script assumed 100 clock ticks/second and exactly ten
seconds of elapsed time. It now reads `CLK_TCK`, accounts for actual elapsed
wall time, rejects an unsettled scenario, and preserves cleanup after a failed
mock process. All final matrix samples completed, but editing that running
shell script caused a trailing command error after its last sample; cleanup
completed. A subsequent clean smoke run validates the finalized runner.

### PGTK and native/Canvas alternatives

An independent separate `emacs -Q` process measured a do-nothing 10-Hz timer:
22.5–23.2% CPU at actual 1568x884 geometry, versus 3.4% at 432x289. No-op baselines
were 4.1% and 0.1%, respectively. Actual geometry differs from the requested
size because of frame sizing. This supports the expensive-presentation finding
without loading mevedel or the user's configuration.

[Canvas/native-module research](editor-cpu/canvas-research.md) confirms Canvas
is in Emacs 32 development sources, but current Canvas refresh still reaches
PGTK's whole-widget invalidation/presentation. A Rust/C renderer could reduce
pixel-generation work; it does not by itself address the measured no-op wakeup
cost. A disposable Canvas versus PGTK-damage experiment is the next informative
upstream investigation, not a justified native spinner rewrite yet. No Emacs
build was replaced and no upstream report was sent.

Raw counters, source identities and graphical check results are in
[the review measurements](editor-cpu/measurements/review-2026-10-08/).
The repeatable isolated PGTK and live view-lifecycle probes are in
`editor-cpu/tools/review-pgtk.py` and `editor-cpu/tools/review-lifecycle.el`.
Mock request bodies and user transcripts are not retained in these artifacts.


### Final validation

- Full isolated Eask suite: 9,760 cases, zero unexpected results, 31 conditional
  environment-dependent skips; 409.98 seconds with four workers. The earlier
  full run also passed, before the final cadence and paused-refresh changes.
- `npx @emacs-eask/cli compile`: 241 files, no warnings; bytecode cleaned by
  the final full-suite runner before testing.
- Final runner smoke: exit 0, static 5.8% and shimmer 26.3%, no manual rearm.
- Native graphical lifecycle probe: all eleven checks passed. The temporary
  frames/buffers, demo timers, focus advice, mock server and measurement session
  were removed. The editor returned to its original single frame and settings
  (60-fps configured ceiling, braille tool preference); the new natural style
  cadences apply beneath that ceiling. Final animation/view code is compiled.
- `git diff --check` passed. Product changes and evidence are collected on
  `fix/cpu-wakeups`; the main checkout was not edited.


### Visual contrast follow-up

After the measurements, the user confirmed that shimmer should keep normal
foreground text with a band fading toward the background, then requested the
same treatment for the other color styles. Breathe now starts at normal
foreground and fades out and back; bounce moves a faded band over normal text.
Glyph styles already retain normal text. Cadences and frame counts are unchanged.
The CPU measurements above predate this contrast-only adjustment.

The final contrast changes pass all 220 isolated animation/stream tests,
including light and dark themes, and all 241 package files compile without
warnings. Compiled animation code is loaded into the live editor and its preview.

## Later work

The upstream-investigation suggestion above was superseded: a mevedel-side
native presenter now draws animation on Wayland subsurfaces. Its experiments
and measurements are in [the renderer lab](editor-cpu/renderer-lab/README.md);
the current state, the 2026-10-08 review and its fixes are summarized in
[the handoff](editor-cpu/handoff.md).
