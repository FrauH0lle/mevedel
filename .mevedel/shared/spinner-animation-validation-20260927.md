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
  that subjective check still needs a safe fresh session or user observation.
