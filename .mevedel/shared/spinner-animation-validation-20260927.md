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

Theme repaint of frozen color labels, 2026-09-28 (source: completion verifier,
independent production-view review, clean Eask runs):

- A verifier showed that a paused, zero-fps shimmer label held explicit
  foreground colors from the old theme indefinitely: cache invalidation
  alone could not trigger a frame or metadata redraw. Theme enable/disable
  hooks now repaint registered visible color spans after invalidation at their
  displayed or frozen phase, without rebuilding the status row or starting
  a decorative timer. Hidden views retain one pending refresh and repaint
  on visibility rearming. Source-loaded nonbatch Emacs 31.1 review reproduced
  visible and hidden behavior, phase/modification/undo preservation, and
  zero-timer operation; review returned PASS. That probe simulated theme
  colors and ran the real theme hooks; it did not enable a graphical theme.
- After `eask clean elc`, the view roster plus power and chat tests passed
  **974/975 expected, zero unexpected, one optional Markdown-mode skip**
  (`.scratch/spinner-theme-final-focused.log`); all 210 files compiled
  without warnings (`.scratch/spinner-theme-final-compile.log`). The isolated
  four-worker full suite discovered **8734 tests: 8 unexpected, 30 skipped**
  (`.scratch/spinner-theme-final-full-suite/`,
  `.scratch/spinner-theme-final-full-run.log`). The full suite is **not green**;
  all eight failing names match the previously recorded non-view failures.

Horizontally hidden labels with visible elapsed text and concurrent color-bank
eviction, 2026-09-28 (source: latest Goal verifier, independent production-view
verification, current clean Eask runs):

- The completion verifier reproduced two defects: a static status row's elapsed
  suffix stayed at `0s` when only the label was horizontally offscreen, and the
  oldest of seven active shimmer views kept its 60-Hz timer but stopped changing
  color frames after eviction from the six-entry shared cache. The view stream
  now registers a separate metadata span and schedules its semantic update
  independently of decorative-label visibility; its timer callback and semantic
  redraw use the same gate. The animation module pins up to four color banks
  per live view alongside the six-entry shared reuse cache. Theme invalidation
  clears both. Unit regressions cover the isolated suffix visibility and
  seven-view bank retention/bounds (`test/test-mevedel-view-stream.el`,
  `test/test-mevedel-view-animation.el`).
- A fresh graphical production view with `static` style and hscroll 12 had
  `label-visible=nil`, `metadata-visible=t`, a one-second timer, and changed
  `Working... · 0s` to `Working... · 4s` after two seconds
  (`.scratch/spinner-followup-gui.log`,
  `artifact://executions/execution-KNKZwt.log`). An independent source-loaded
  graphical verifier observed the same transition to `3s` after 3.2 seconds
  (`artifact://executions/execution-jpPQmG.log`). It also displayed seven
  distinct shimmer views at once: every view had a 16.67-ms timer, the oldest
  changed frames after eviction, and 20 warm ticks resolved no colors
  (`artifact://executions/execution-1p9MQF.log`). Read-only review and this
  production verification each returned PASS. Neither observation guarantees
  60 *displayed* frames per second under the concurrent test workload.
- After `eask clean elc`, the plan's named eight-file focused roster passed
  **914/915 expected, zero unexpected, one optional Markdown-mode skip**
  (`.scratch/spinner-followup-final-focused.log`). Compilation completed all
  210 files without warnings (`.scratch/spinner-followup-final-compile.log`).
  An isolated four-worker full run discovered **8736 tests: 8 unexpected,
  30 skipped** (`.scratch/spinner-followup-final-full-suite/`,
  `.scratch/spinner-followup-final-full-run.log`). The full suite is **not green**;
  the unexpected names match the eight recorded above, none in view/animation
  tests. Concurrent plan-handoff working-tree changes are unrelated to this
  follow-up. No running user Emacs library was hot-reloaded.

Same-window animation attention follow-up, 2026-09-28 (source: current Eask
checks and independent read-only graphical verifier `/root/verify_7`):

- Animation target and elapsed-suffix visibility, horizontal suspension, and
  redisplay rearming require target visibility and frame attention in the same
  window. Transcript rendering keeps its separate buffer-wide attention gate.
  The target-frame selection still considers all visible frames because a
  shared display property must work in unfocused frames with other palettes.
  A two-window regression checks no timer, frame writes, or metadata updates
  for a background-only target, then a resumed glyph timer through the
  focus-change hook on a deterministic focus-state change
  (`mevedel-view-stream.el`, `test/test-mevedel-view-stream.el`).
- A fresh two-frame production-view probe measured `ticks=0 writes=0
  watcher=nil queries=0` across 350 ms with a focused offscreen label and an
  unfocused visible label under `auto`. Hscroll 13 revealed only the suffix:
  `period=1.0`, `writes=0`, no watcher, and `Working... · 0s` advanced to
  `Working... · 2s` in 1.15 s. A deterministic focus-state swap rearmed a
  16.67-ms timer and delivered 20 frame writes; actual compositor focus
  transfer was not granted, so recovery after a genuine focus event is not
  directly verified. The verifier returned `VERDICT: PARTIAL` on that basis.
- After Eask cleanup, the named eight-file roster passed **915/916 expected,
  zero unexpected, one optional skip** (`.scratch/spinner-attended-final2-focused.log`);
  all **210 files** compiled without warnings
  (`.scratch/spinner-attended-final2-compile.log`). The isolated full suite ran
  **8737 tests, 8 unexpected, 31 skipped**
  (`.scratch/spinner-attended-final2-full-suite/summary.json`,
  `.scratch/spinner-attended-final2-full-run.log`). The eight failures are the
  same non-view failures recorded above, including the concurrent plan-handoff
  failures; the full suite is **not green**. No user Emacs library was hot-reloaded.
- The final regression was tightened to call the production focus-change hook
  instead of directly restarting its timer. After another Eask cleanup, the
  named roster again passed **915/916 expected, zero unexpected, one skip**;
  **210 files** compiled with no warnings
  (`.scratch/spinner-attended-focus-hook-{clean,focused,compile}.log`). The
  completed full suite above predates only this test assertion improvement;
  its production source and other tests are unchanged.

Horizontal redisplay timer-ownership follow-up, 2026-09-28 (source: Goal
verifier failure, scoped fix, current checks, independent `/root/verify_8`):

- The Goal completion verifier reproduced two pending timers when an automatic
  horizontal pan revealed a label while its suffix still owned a one-second
  metadata timer: the deferred zero-delay probe replaced the buffer's timer
  pointer but did not cancel the old callback. The old callback was eventually
  guarded as stale, but stop could not cancel its queued wakeup. The redisplay
  handoff now cancels that timer and clears its cadence before installing the
  probe; the existing timer-identity check still guards stale callbacks.
  A new source-backed ERT case checks pending metadata, probe replacement,
  duplicate redisplay, and stop cleanup. Docs and ADR 0119 describe the handoff.
- Independent fresh graphical production-view verification observed automatic
  horizontal pan with `hscroll=12`, hidden label, visible suffix, and a 1-s
  timer. After the pan, `old-pending=nil probe-pending=t`; stop cleared both.
  A second pan resumed a 16.67-ms animation timer (36 ticks), and a major-mode
  change left no current timer. Its adversarial stale-callback checks for stop,
  buffer kill, and supersession also passed (`/root/verify_8`, graphical
  execution `exec-000761`, `VERDICT: PASS`).
- After clean Eask bytecode, the plan's named eight-file roster passed
  **916/917 expected, zero unexpected, one optional skip**
  (`.scratch/spinner-timer-final-focused.log`); **210 files** compiled without
  warning matches (`.scratch/spinner-timer-final-compile.log`). A direct
  source-loaded replay of the verifier's batch scenario showed
  `old=nil probe=t period=nil` and no timer after stop.
- The isolated four-worker full suite discovered **8738 tests, 8 unexpected,
  30 skipped** (`.scratch/spinner-timer-final-full-suite/summary.json`,
  `.scratch/spinner-timer-final-full-run.log`). The new timer-ownership test
  passed in worker 1. All eight unexpected names exactly match the preceding
  full run's unrelated failures: gptel bridge install, preset transitions@2,
  message inject@2, steering inject@9, skills-ui layout@8, and three concurrent
  plan-handoff dispatch cases (@3, @8, @10). The full suite is **not green**.

Power-observer TRAMP suspension investigation, 2026-09-28 (source: fresh
`/root/verify_9` probe, scoped Eask runs, and installed Emacs 31.1 timer/TRAMP
source):

- When the shared fallback timer was created in a discarded temporary timer
  list, a later visible-view rearm previously retained its dead timer object
  and never queried power. A pending-list check and regression now recover
  that path. Repeated `watch` calls inside a real `with-tramp-suspended-timers`
  binding could also duplicate an already-existing timer; retaining uncertain
  old references and reconciling after restoration fixes that ordinary
  restoration path. `/root/verify_9` saw one timer on restoration, one
  deferred query, zero additional queries on 120 frame callbacks, and normal
  final cleanup when unwatch occurred after restoration.
- **Remaining failure:** final unwatch *inside* the TRAMP binding cannot remove
  its already-hidden outer timer with `cancel-timer`. Independent probe after
  restoration: `watchers=0 queued-polls=1 reference=nil retired=nil
  old-pending=t`. The callback's watcher guard avoids a backend query but the
  orphan wakeup violates the plan. Installed `tramp.el` binds `timer-list`
  dynamically to nil, and installed `timer.el` implements `cancel-timer` by
  deleting only from the current binding. This is an unresolved completion
  blocker; the current first repair must not be declared complete. The
  interrupted first full run (`.scratch/spinner-power-final-full-run.log`)
  has no summary and is not completion evidence.
- After Eask bytecode cleanup, the plan's eight-file roster plus power tests
  passed **924/925 expected, zero unexpected, one optional skip**
  (`.scratch/spinner-power-suspension-roster.log`). All **210 files** compiled
  without warning matches (`.scratch/spinner-power-suspension-compile.log`),
  and `git diff --check` passed. A complete isolated four-worker suite
  discovered **8741 tests**, with **8 unexpected and 30 skipped**
  (`.scratch/spinner-power-suspension-full-suite/summary.json`); the eight
  failures are the same non-view cases named in the preceding paragraph. The
  new power tests and production-view timer-suspension test passed in this
  suite. This run predates resolution of the inside-binding final-unwatch case
  and is not a green full suite.

Top-level power-timer ownership resolution, 2026-09-28 (source: current
source/tests, independent `/root/verify_9` fresh Emacs 31.1 verification,
independent `/root/power_timer_review`, and isolated Eask runs):

- The **remaining failure above is resolved in the current source**. Instead of
  replacing timers that merely appear absent inside TRAMP's dynamically bound
  temporary `timer-list`, the shared UI-host poll is inserted into the
  top-level timer list. Cancellation also removes its owned timer there even
  when called inside nested suspension, and stale callbacks are rejected by
  identity. Regression tests exercise creation, repeated watching, final
  unwatch inside suspension, notification rearming, and stale callbacks.
- `/root/verify_9` returned `VERDICT: PASS` after a fresh production-view
  probe: one deferred external-power query, unchanged animation phase, no
  further queries across 120 frame callbacks, and no remaining observer
  timers or hooks after stopping inside suspension. The independent review
  returned PASS; its in-memory nested-TRAMP probe confirmed that foreign
  timers and their ordering remain intact. These are controlled Emacs 31.1
  checks, not a live remote-connection test. Neither reloaded the user's
  active Emacs.
- After Eask cleanup, power and stream tests passed **159/159**
  (`.scratch/spinner-power-toplevel-focused.log`), and the accepted plan's
  eight-file view roster plus power tests passed **924/925 expected, zero
  unexpected, one optional skip**
  (`.scratch/spinner-power-toplevel-roster.log`). All **210 files** compiled
  without warnings (`.scratch/spinner-power-toplevel-compile.log`), and
  `git diff --check` passed. The current isolated four-worker full suite ran
  **8741 tests, 8 unexpected, 31 skipped**
  (`.scratch/spinner-power-toplevel-full-suite/summary.json`,
  `.scratch/spinner-power-toplevel-full-run.log`). Its eight failures exactly
  match the earlier non-view gptel bridge, preset, message-inject,
  steering-inject, skills-ui, and three concurrent plan-handoff cases. Every
  power test passed; the full suite is **not green**. The optional skip count
  differs by one from the previous run. Genuine KDE/Wayland focus transfer
  remains unverified; the earlier deterministic focus-hook recovery passed.
