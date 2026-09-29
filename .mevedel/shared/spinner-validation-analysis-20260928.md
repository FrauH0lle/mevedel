# Spinner Goal: validation and outcome analysis — 2026-09-28

Task: `spinner_validation_analysis`; session `2026-09-27T16-24-5ca69305960c`.
Analysis only: inspected persisted evidence, Git history/diffs, and relevant
contracts. No tests, profiling, source edits, session mutations, or CPU/telemetry
aggregation. This report's verdict concerns analysis completeness, not a new
product certification.

Source abbreviations below:
- **S** = `.mevedel/sessions/2026-09-27T16-24-5ca69305960c`
- **V** = `.mevedel/shared/spinner-animation-validation-20260927.md`
  (also `work://shared/spinner-animation-validation-20260927.md`).

## Outcome and change boundary

**Observed:** the persisted Goal is `complete`, updated
`2026-09-28T18:58:12+0200` (`S/session.meta.el:6787`). Its accepted plan is
`S/local/plans/accepted-20260927-174116.md`; validation requirements are at
lines 120–146. The final whole-Goal report ends in one PASS verdict and explicitly
distinguishes animation acceptance from a green repository suite
(`S/session.meta.el:235–281`). Goal completion is a verifier-backed judgment,
not a mechanical proof (`docs/goals.md:228–260`).

Git places the initial feature at `3a9e950a` (12 files, 1,792 insertions/229
deletions): configurable main/tool styles, animation/power modules, view scheduler,
tests, README and contracts. Subsequent commits repair real edge cases: one-shot
freeze/stall handling (`5d5228fc`), low-color terminals (`40cd5b49`), same-window
attention (`2af207c8`), redisplay timer ownership (`2578056b`), power/view timers
under TRAMP (`cd582691`, `d345db81`), expiry notifications (`376db0a6`), frozen
face/frame/glyph refresh (`51cfecd9`, `0854deed`, `c4524359`), and incremental/full
tool-phase preservation (`ac1fd14b`, `c2ac0428`). The last visibility corrections
are `d78d7247`, `b8a05bc7`, `73d05bef`, and **`2313e32e`**. HEAD **`e83c9b0f`**
only adds native-focus continuity evidence to V; it does not change production
source. Unrelated plan-handoff/docs working-tree changes remain present.

## Final validation: directly checked artifacts

| Check | Persisted result | Source |
|---|---|---|
| Animation/stream focused run | 182/182 expected | `.scratch/spinner-metadata-final-focused.log:192` |
| Accepted-plan view roster plus power/chat/hooks/integrity | 1,089 tests: 1,088 expected, zero unexpected, one optional composer skip | `.scratch/spinner-metadata-roster.log:1108–1111` |
| Eask compilation | 210 files, no warning/error matches in inspected log | `.scratch/spinner-metadata-final-compile.log:214` |
| Complete two-worker suite | **8,760 tests, eight unexpected, 30 skipped; not green** | `.scratch/spinner-metadata-final-full-suite/summary.json:592–611` |
| Fresh production scheduler probe | Actual full/save periods 16.67/33.33 ms, retained phase, frozen display through metadata update, hidden zero ticks | `S/tool-results/executions/execution-dGbOiQ.log:1–4` |
| Independent lifecycle probe | Timer replaced at save30; phase/cache retained; save0 draft/display retained; hidden and stale-callback cleanup | `S/tool-results/executions/execution-NO0dnt.log:1–5` |

The full-suite failure lists contain only: gptel bridge install; preset
transitions@2; message inject@2; steering inject@9; skills slash-command layout@8;
and plan-handoff dispatch@3/@8/@10
(`.scratch/spinner-metadata-final-full-suite/0.log:4857–4859`,
`1.log:4982–4986`). Four recur in the original baseline replay
(`.scratch/spinner-baseline-replay.log:344–350`). The later clean baseline has
only the skills failure in the 140-test roster, whereas the concurrent tree has
that plus the three handoff failures
(`.scratch/spinner-current-baseline-focused.log:200–203`;
`.scratch/spinner-suite-failures-focused.log:359–365`).
**Inference supported by these comparisons:** the recurring failures are outside
the animation change; this does not make them passing tests or diagnose their
root causes. V:82–93 supplies the baseline checkout attribution.

## Limitations and reservations: what remains, what was resolved

- **Battery hardware was available and used.** Raw samples show AC=0,
  Discharging and constant brightness 39321; the backend classified battery
  with one query and an actual 33.33-ms timer
  (`.scratch/spinner-physical-battery-20260927/1-auto-30.samples.tsv:1–7`,
  `1-auto-30-spinner-auto-power.stats:1–2`). V:28–44 records five short sequential
  runs and noisy/coarsely updated whole-system measurements. This supports
  physical battery-policy operation, **not attributable energy savings or a
  battery-life percentage**. Later source changes were not all remeasured on
  physical battery; current fixtures/probes provide complementary coverage.
- **Fresh graphical frames versus native focus:** the latest attempt had no
  focused frame, no events and no timer (`.scratch/spinner-native-focus-final.log:1–3`).
  It cannot establish native recovery. However, the earlier raw native event
  log really shows focus-out cancelling the timer and focus-in restoring
  16.67-ms cadence with phase retained
  (`S/tool-results/executions/execution-R0ldpj.log:1–2`). I checked
  `git diff 51cfecd9..2313e32e -- mevedel.el mevedel-view.el`: focus-hook installation
  is unchanged; callers additionally pass `RESUMED=t`, and pixel-scroll advice
  is added. The independent verifier explicitly revised its focus reservation
  using that continuity and current controlled-focus evidence
  (`S/session.meta.el:422–469`). **Inference:** this is a defensible evidence
  composition, not proof that the latest compositor granted focus, nor coverage
  of every window manager. The active user's older loaded Emacs was not reloaded.
- **Hscroll is a repaired defect family, not a documented remaining blocker.**
  Earlier fixes successively missed replacement-string indices, right-fringe
  suffixes at hscroll zero, wrapped ellipsis/pixel scroll, constant-colored tails,
  invariant glyph separators, and a narrow viewport exposing only the middle of
  elapsed text. V:721–907 records reproductions and corrective GUI/regression
  evidence. Final-source inspection confirms separate text-row visibility,
  displayed-index/changing-bound motion checks, separator exclusion, and an
  independent event-repaint gate (`mevedel-view-stream.el:629–811`). Final notes
  attribute six main/tool separator cases, ten narrow hscroll cases and a wrapped
  suffix case to fresh graphical replays (V:874–895). I inspected this recorded
  coverage and current code, but did not rerun that GUI matrix. Controlled focus
  and short observation windows limit generalization; the final completion
  report identifies no unresolved hscroll failure.
- **Appearance/input scope:** the user approved a separate 18-second production
  preview (V:62–71). Other runs exercised simulated streaming, typing, scrolling
  and theme/policy changes (V:45–61). This is not evidence of physical typing in
  the older host Emacs, keyboard-to-screen latency, scanout, or guaranteed 60
  displayed fps. TRAMP evidence uses controlled real suspension bindings, not a
  live remote connection (V:435–444).

## Expensive/repeated verification patterns

**Observed:** the handoff repeatedly records cleanup → focused roster → all-210-file
compile → complete suite after small follow-ups. Completion review found further
boundary cases even after scoped reviewers passed: frozen tools progressed from
lightweight-row to incremental to full/batched projection coverage (V:602–662,
688–719); visibility moved through successive geometric/color boundaries
(V:721–907). These were substantive discoveries, not merely repeated identical
checks.

**Observed avoidable churn:** an overlapping eight-worker run lost a worker and
inventory (V:145–151); concurrent cleanup produced cold-load/parse failures
(V:179–186); concurrent edits produced integrity noise (V:523–532); a four-worker
attempt later stopped with missing results and needed a two-worker retry
(V:807–816). Isolated replays/full retries also investigated unexplained transient
execution/hooks failures (V:650–662,755–762). The final full run alone took
721.17 seconds, versus 22.68 seconds for its named focused roster
(`.scratch/spinner-metadata-final-full-run.log:7`,
`.scratch/spinner-metadata-roster.log:1108`). These are individual persisted test
durations, not an aggregate CPU/cost estimate.

**Analysis:** verification was valuable but fragmented. Grouping adversarial cases
by the underlying invariant (displayed changing region, retained phase across
all projection paths, timer ownership through all lifecycle paths) could expose
adjacent defects earlier. Isolating cleanup/build activity from parallel runners
would avoid known interference. The accepted plan did require full Eask validation;
this observation is not a recommendation to omit required final checks. The last
native-focus reconsideration is a useful positive example: assess existing evidence
against relevant source changes instead of requiring an environmentally blocked
rerun merely for freshness.

All requested analysis areas are covered. Remaining uncertainty is the explicitly
bounded product-validation scope above; no new execution-based correctness claim
is made.

VERDICT: PASS
