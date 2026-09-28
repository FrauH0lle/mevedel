# Working animation implementation: validation handoff (2026-09-27)

Source/task: accepted plan `work://plans/accepted-20260927-174116.md` and
commit `3a9e950a` on `master`. This is a dated observation, not an instruction
or a claim about later checkouts.

- Focused Eask view tests: 900/901 expected, zero unexpected, one optional
  Markdown-mode skip (`.scratch/spinner-required-focused-final.log`). Eask
  compilation: 210 files, no warnings (`.scratch/spinner-final-compile.log`).
- Full runner: four unexpected, 30 skipped
  (`.scratch/test-suite-performance/20260927-204344/`). The four test names
  also fail from unchanged HEAD with
  the same Eask dependencies (`.scratch/spinner-baseline-replay.log`), so the
  full suite is not green but the failures predate the animation change.
- Graphical production runs measured ~16.67 ms full and ~33.33 ms save frame
  intervals, 120 ms for Braille, zero callbacks for a hidden view, and no
  decorative ticks in a static view (`.scratch/spinner-final-*.stats`). The
  original Braille at unchanged HEAD took ~0.445 ms median callback in the
  same graphical environment (`.scratch/spinner-baseline.stats`); new Braille
  took ~0.394 ms median in its short run. These are not battery-life estimates.
- A separate graphical interaction run retained a typed draft across scrolling,
  a theme switch, and a full-to-save policy change; offscreen cadence was nil
  and resumed at 33.33 ms (`.scratch/spinner-interaction-events.log`).
- With two production views on the real host, `auto` made one shared
  `battery-upower` query, classified external, and switched both view timers
  to 16.67 ms (`.scratch/spinner-auto-power.stats`). At the time, AC/online
  was `1` and BAT0 was `Full`.
- After the user unplugged the laptop on 2026-09-27, five sequential fresh
  graphical runs used the same shimmer view with five visible tools and fixed
  backlight brightness 39321/65535. `/sys/class/power_supply/AC/online` stayed
  `0` and BAT0 reported `Discharging` throughout the runs. Automatic policy
  queried `battery-upower` once per run, classified `battery`, and selected
  a 33.33-ms actual view timer. It delivered about 50–51 rendered frame
  changes and 60 callbacks per 3.5–3.6-second run, versus 93 changes and
  120 callbacks in a forced-full 16.67-ms run. With `save` at zero battery
  fps, decorative callbacks stopped; the two callbacks 1 second apart were
  semantic maintenance. The reference backlight, 0.2-second BAT0 power and
  energy samples, GUI logs, and renderer statistics are in
  `.scratch/spinner-physical-battery-20260927/`; raw execution is
  `artifact://executions/execution-hP6BbA.log`. Median whole-system power
  readings during the successive runs ranged from 28.145 to 28.343 W,
  varying across repeated auto runs; their coarse update rate and short run
  length do not support attribution or any battery-life estimate. The user
  was told they can reconnect AC after this sampling.
- The running host Emacs had loaded an older `mevedel-view` (the new spinner
  style symbol was absent), whereas GUI tests loaded the committed sources
  into a fresh Emacs. Avoid hot-reloading the user's active request solely for
  visual verification. A fresh graphical Emacs subsequently exercised the
  production view and `mevedel-view-stream-schedule` with five real
  gptel-property response chunks, plus a typed composer draft, scrolling, a
  full-to-save power-policy switch, and a theme change. Rendered screenshots
  show the response progressively arriving without losing the draft; event
  records show offscreen animation suspended and an onscreen 33.33-ms timer
  afterward (`.scratch/spinner-live-stream-graphical-check.el`,
  `.scratch/spinner-live-stream-graphical-events.log`, and
  `.scratch/spinner-live-stream-{early,middle,theme-save}.png`;
  `artifact://executions/execution-aMPguH.log`). The concurrent full Eask
  runner makes this smoke run unsuitable for isolated callback-cost claims.
  A sequence of static screenshots and timer statistics cannot establish a
  person's perception of smoothness during a live request in the host Emacs;
  that subjective check needed a safe fresh session or user observation.
- At the user's request, a separate fresh Emacs displayed the production
  renderer with simulated response chunks for 18 seconds on 2026-09-27.
  The local preview script `.scratch/spinner-production-user-preview.el`
  switched from automatic to saving policy at 7 seconds and changed theme
  at 11 seconds; it exited successfully with an empty error log
  (`artifact://executions/execution-61YyDx.log`). The user reported,
  "Yes, it looked good". This is direct visual feedback about the preview,
  not evidence that they typed or scrolled, nor a live request in their older
  already-loaded host Emacs. The separate graphical smoke run above covered
  synthetic typing and scrolling without changing the host Emacs.

Follow-up validation, 2026-09-27 after the dots fallback and live-setting/
timer-recovery regressions (source: this task's focused Eask run, independent
review, and full-suite runner):

- After `eask clean elc`, the complete view roster plus power tests passed
  906 of 907 results, zero unexpected, one optional Markdown-mode skip
  (`.scratch/spinner-last-focused.log`). Eask compiled all 210 files without
  warnings (`.scratch/spinner-last-compile.log`). The follow-up review found
  no remaining bug in the dots target-frame filtering and one-second cache.
- The complete Eask script discovered 8721 tests, finished with **8 unexpected
  and 30 skipped**, and is **not green** (`.scratch/spinner-final-full-suite/`,
  `.scratch/spinner-final-full-run.log`). The four previously reproduced
  baseline failures remain: gptel bridge install, preset transitions@2,
  message inject@2, steering inject@9. The skills-ui `/clear` layout@8
  missing-segment failure also reproduces in a separate unchanged-`bc31bb12`
  worktree (`.scratch/spinner-current-baseline-focused.log`). Three additional
  plan-handoff dispatch cases (@3, @8, @10) fail in both the full run and a
  focused rerun on the concurrent plan-handoff working tree
  (`.scratch/spinner-suite-failures-focused.log`); these cases passed in the
  same focused roster on clean `bc31bb12`, before the concurrent plan-handoff
  working-tree edits (`.scratch/spinner-current-baseline-focused.log`).

Final animation completion follow-up, 2026-09-27 (source: scoped working-tree
changes after `378963ae`, independent read-only reviews and fresh Eask runs):

- A verifier reproduced a frozen ASCII label changing on an elapsed redraw
  after a non-rendering scroll rearm, and repeating timer callbacks catching
  up in a burst after a stall. The implementation now records only actually
  displayed main samples, clears the frozen latch on resumption, and rearms
  one owned non-repeating timer from the present. Pending-tool rebuilds
  restore surviving calls' glyphs by stable zone ID. New tests cover freeze
  boundaries, repeated freeze/resume, structural tool rows, and overdue timer
  delivery. Both independent follow-up reviewers returned PASS; the runtime
  verifier exercised real-view AC/BAT notifications and 12 pending-tool
  permutations (`artifact://executions/execution-PdULZ9.log`,
  `artifact://executions/execution-daSNIx.log`).
- After Eask cleanup the named view roster plus power tests passed **910/911
  expected, zero unexpected, one optional Markdown-mode skip**
  (`.scratch/spinner-stall-final-focused.log`). Compilation completed all 210
  files without warnings (`.scratch/spinner-stall-final-compile.log`). The
  full Eask runner discovered 8725 tests and ended with **8 unexpected, 30
  skipped** (`.scratch/spinner-stall-final-full-suite/`,
  `.scratch/spinner-stall-final-full-run.log`); its unexpected test names match
  the earlier eight recorded above, so the full suite remains not green. The
  unrelated plan-handoff working-tree changes still belong to another task.
- Fresh graphical Emacs production-view smoke runs after the one-shot timer
  change measured median delivered/rendered intervals near 17.3 ms under
  `full` and 34.0 ms under `save` (30 fps), with five visible tool rows;
  callback medians were 0.376/0.387 ms respectively. Saving at zero fps
  delivered only two elapsed-metadata callbacks and no changed frames over
  ~3.3 seconds; a hidden view delivered no callbacks. These are short-run
  overhead and cadence checks, not battery-life measurements or a guarantee
  of 60 displayed frames per second (`.scratch/spinner-final-one-shot-*.stats`,
  `artifact://executions/execution-GU1igq.log`).

Low-color terminal completion follow-up, 2026-09-28 (source: Goal verifier,
fresh real-terminal probe, independent review, and Eask runs):

- The Goal verifier reproduced color-capable eight-color terminals where all
  216 prepared color samples displayed identically, despite a 60-Hz timer.
  The color-bank entry now rejects terminal palettes below 256 colors and
  selects the existing glyph fallback and slower timer cadence. A read-only
  independent `TERM=linux`, `emacs -Q -nw` production-view probe observed all
  three color styles moving glyphs at 120 ms with no color bank; a 256-color
  terminal retained the color bank and 60-Hz ceiling
  (`artifact://executions/execution-whlZ49.log`). This threshold is a
  conservative heuristic for useful shade animation, not a claim that every
  256-color terminal has 64 distinguishable shades.
- After Eask cleanup, the named view roster plus power tests passed **912/913
  expected, zero unexpected, one optional Markdown-mode skip**
  (`.scratch/spinner-terminal-final-focused.log`). All 210 files compiled
  without warning matches (`.scratch/spinner-terminal-final-compile.log`).
  An eight-worker full run overlapping another task's full runner ended with
  one unfinished worker and missing inventory, so it did not yield a valid
  summary (`.scratch/spinner-terminal-full-run.log`). A complete four-worker
  retry discovered 8727 tests and ended with **8 unexpected, 30 skipped**;
  its eight names match the earlier unrelated failures recorded above
  (`.scratch/spinner-terminal-full-suite-4/`,
  `.scratch/spinner-terminal-full-run-4.log`). The full suite is not green.

Horizontal-scroll completion follow-up, 2026-09-28 (source: production
probes, independent review, clean Eask runs, and full-suite runner):

- A completion verifier found that a label entirely hidden by horizontal
  scrolling still received 60-Hz display writes. The visibility gate now
  checks the bounded animated span's actual display position in hscrolled
  windows, retaining the cheap row-boundary check in ordinary unscrolled
  windows. Explicit `set-window-hscroll` calls rearm through owned advice;
  automatic Emacs panning is observed through a temporary buffer-local
  redisplay hook while horizontally suspended, with one owned deferred
  recheck. A two-window regression prevents unrelated redisplay callbacks
  from scheduling needless rechecks. The independent re-review returned
  PASS after an Emacs 31.1 terminal probe of that case.
- Fresh production-view probes exercised explicit scrolling, automatic pan
  (hscroll 260 to 0), and partial visibility: fully hidden labels had no
  frame writes or active high-frequency timer, returning to view restored
  frames and the 16.67-ms timer, and a partly visible label kept animating
  (`.scratch/spinner-hscroll-production-probe.log`,
  `.scratch/spinner-hscroll-auto-production.log`,
  `.scratch/spinner-hscroll-partial-final.log`). The final two-window
  refinement is separately covered by the independent terminal probe and
  regression; the earlier graphical probes did not rerun after that change.
- Following `eask clean elc`, the named view test roster plus chat tests
  passed **967/968 expected, zero unexpected, one optional Markdown-mode
  skip** (`.scratch/spinner-hscroll-v2-focused.log`). All 210 files compiled
  with no warning matches (`.scratch/spinner-hscroll-v2-compile.log`), and
  `git diff --check` passed. The isolated four-worker full suite discovered
  **8732 tests: 8 unexpected, 30 skipped**
  (`.scratch/spinner-hscroll-v2-full-suite/`,
  `.scratch/spinner-hscroll-v2-full-run.log`). It is **not green**; the eight
  unexpected names match those from the earlier full-suite runs recorded
  above. An overlapping cleanup caused three extra view cold-load/parse
  failures in the first follow-up attempt; they did not recur when the final
  full run was isolated from cleanup.

Hidden frozen-tool power cleanup, 2026-09-28 (source: completion verifier,
independent nonbatch re-review, clean Eask runs):

- A completion verifier reproduced battery fallback queries continuing after
  the only zero-fps, tool-only view was hidden. A buffer-local window-change
  callback had considered the replacement buffer but not the departing view.
  The callback now reevaluates the departing view without resuming its pending
  transcript render. Deleting the view's last window does not dispatch that
  buffer-local hook, so the shared power observer also installs a default
  window-state callback only while subscribed views exist. It asks their
  schedulers to recheck visibility and removes itself with the final watcher.
- A regression covers replacement, preservation with a second window, last-
  window deletion, no battery queries after unsubscription, and reopening
  (`test/test-mevedel-view-stream.el`). In batch Eask it explicitly dispatches
  the hooks; the independent reviewer also observed automatic delivery in
  nonbatch Emacs 31.1: replacement and last-window deletion both cleared the
  watcher, shared timer, and hooks; a second visible window retained them;
  reopening restored watching without a decorative timer. Review returned
  PASS. No active user Emacs library was hot-reloaded.
- After `eask clean elc`, the named view roster plus power and chat tests
  passed **973/974 expected, zero unexpected, one optional Markdown-mode
  skip** (`.scratch/spinner-delete-final-focused.log`). All 210 files compiled
  without warnings (`.scratch/spinner-delete-final-compile.log`). The isolated
  four-worker full suite discovered **8733 tests: 8 unexpected, 30 skipped**
  (`.scratch/spinner-delete-final-full-suite/`,
  `.scratch/spinner-delete-final-full-run.log`). The full suite is **not green**;
  the eight failing names match earlier runs and do not include a view or
  power test. `git diff --check` passed.
