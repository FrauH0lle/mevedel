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
  was `1` and BAT0 was `Full`; no controlled physical on-battery measurement
  was possible without unplugging the laptop. Keep that validation pending;
  don't extrapolate battery life from timer counts or short GUI runs.
- The running host Emacs had loaded an older `mevedel-view` (the new spinner
  style symbol was absent), whereas GUI tests loaded the committed sources
  into a fresh Emacs. Avoid hot-reloading the user's active request solely for
  visual verification. Direct human inspection of production smoothness while
  streaming remains pending until a safe reload/new session is available.
