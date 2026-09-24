# Live view corruption investigation — 2026-09-16

Task: user requested investigation before product changes; preserve the live
broken view for session `2026-09-16T10-18-06d94b273906/segment-0001.chat.org`.

## Observations

- Authoritative final answer at transcript lines 2747–2778 is intact. Live data
  range 527688–529075 is one `gptel=response` run. The saved and live answer
  contain one fix heading and one HTML example; the live view contains four
  copies of the fix heading and seven copies of the HTML example.
- A fresh rendering in an undisplayed temporary buffer using the loaded native
  functions produces correct text. Ordinary seven-character streaming chunks
  also work with the loaded runtime. The affected view remains untouched.
- Idle affected view retains live-data-tail marker 528879 and live-view-tail
  marker 3789 despite cleared in-flight/data-turn markers. Its data-tail marker
  points into prose, not the displayed HTML block it purports to reconstruct.
- Loaded render functions are native functions attributed to Straight's `.elc`,
  with native unit `mevedel-view-render-1ee58ad9-dabbee7f.eln`.

## Reproduction and limits

- Scratch ERT: `.scratch/view-render-investigation-20260916/replay.el`.
  Run through Eask after required `clean elc`. Latest run: two pass, one fails.
  Ordinary chunks (1, 7, 23, 51 chars) pass. A live update nested inside settlement
  passes. Settlement injected during response fontification inside an older live
  update fails: expected `(:tail nil :copies 1)`, actual `(:tail t :copies 2)`.
- Repeated the failing ordering with loaded native functions in temporary buffers:
  also leaves a live-tail marker and two HTML examples. Temporary override was
  target-buffer guarded and restored; temporary buffers/directory were removed.
- This establishes a reentrant render/settlement defect and a mechanism matching
  duplicate output plus leftover streaming state. It does NOT prove which call
  yielded/reentered during the original session. Historical render debug logging
  was off; the exact original event ordering is unavailable.
- Relevant code: `mevedel-view-render.el:3322–3577` and
  `mevedel-view-stream.el:1029–1087`. An older live render can resume after nested
  settlement and insert stale text/reestablish retained-tail state.

At the investigation checkpoint no product source or live affected buffer was
changed. Existing checkout edits belong to other work. Do not claim the full
screenshot was reproduced without injected event ordering.

## Accepted implementation, main agent, 2026-09-16

- User approved `work://plans/accepted-20260916-150655.md`. Implementing now;
  accepted scope excludes commits and resetting the original live view.
- `/root/async_audit` implemented fontification isolation and transport ownership.
  Main independently verified 52/52 focused tests and warning-free two-file
  compilation. The agent also implemented the initial core view coordinator;
  latest integration 478/479 passed, terminal cleanup/source replacement still red.
  `/root/view_finish` now owns remaining view fixes and acceptance coverage,
  including deferred agent-observer error isolation. Original agent is idle.
- `/root/lifecycle_audit` completed request settlement/final-patch ownership;
  main independently verified the combined 186-test lifecycle suite (all pass),
  `artifact://executions/execution-gXvg82.log`.
- Main owns documentation, integration, review, and compiled-runtime verification.
- Main's bounded compile found missing compile-time gptel FSM setter definitions
  in `mevedel-turn.el`; added `eval-when-compile` dependency, warning now resolved.
  Other bounded chat/directive compile warnings predate their ownership hunks;
  full compilation remains required to establish final status.
- `/root/standards_review` and `/root/spec_review` are reviewing scoped uncommitted
  ownership changes against captured baseline `afb72042ea7f2287b476eb96a3a41249bd03b0e4`.
  Full-suite, final compile, and compiled-runtime verification are outstanding.
- Main added `test/test-mevedel-view-render-reentry.el`: isolated Eask red run
  failed on revived tail state (0/1), log
  `artifact://executions/execution-JJsX1T.log`. No core fix was applied at that point.
- Upstream gptel refresh succeeded (`Already up to date`); stream insertion still
  calls `gptel-post-stream-hook` after inserting a chunk and before locking its
  tracking marker. Live configured gptel source resolves to `/home/roland/gptel`;
  upstream freshness does not establish runner/runtime dependency equivalence.
- Broken view before implementation runtime checks: size 4616, unmodified,
  SHA256 `d38b9047acb7f486d8d6f1668f968bd51f51b494aee5b53024972a5617c510da`.
- Disposable live-runtime verification script prepared at
  `.scratch/view-render-investigation-20260916/verify-runtime.el`; check-parens
  passed, behavior not yet run against rebuilt artifacts. Do not load test/helpers.el
  into the live user Emacs; its Eask-only isolation contract must remain intact.

## Lifecycle implementation handoff, /root/lifecycle_audit, 2026-09-16

- Preserved earlier turn/chat/presets/directive ownership edits. Admission now
  captures entry request identity before readiness/authority checks, rejects
  changes there, and rechecks current request plus settlement fence after
  cancellation callbacks. Deterministic canceller-admits-B and readiness reentry
  tests failed before their fixes (logs `execution-5udtHk.log` and
  `execution-0P6hkW.log` under `artifact://executions/`).
- Fixed two preset fixture defects without changing assertions: register tools
  inside the hook-agent case; move discussion tool registration inside its case
  rather than a separate macro-generated test. Preset count is now 60, not 61.
- Final isolated Eask cleanup plus single-file runs: ownership 10/10, turn 33/33,
  presets 60/60, chat 51/51, directive-request 32/32. Evidence:
  `artifact://executions/execution-S8Ssvc.log`. No unexpected diagnostics;
  scoped `git diff --check` passed. No full suite, compile, or commit by this agent.
- Added final-patch cancellation/error checks proving hold release and old
  response persisted on disk. Canonical pre-hold workspace lookup and directive
  capture are in-memory; terminal activity recording/state recomputation also
  perform no I/O or callback dispatch. No speculative broader hold added.
- Final integration/docs/live-runtime validation remain main-owned. Initial
  confined Eask launch failed npm directory grant preparation; focused commands
  ran only after explicit execution escalation approval.

## View completion handoff, /root/view_finish, 2026-09-16

- Reproduced terminal-cleanup spinner revival and deferred agent observer error
  escape in isolated Eask before fixes (`execution-iNm40a.log`,
  `execution-lHskwC.log`). Both now pass. Added both render orders, both agent
  refresh orders with two handles/expanded reminder, desired disclosure state,
  source replacement, and reverse segment replacement coverage.
- Fixed the acceptance test's concrete agent reader drift: the shared
  `after-header-position` skipped real history in headerless agent transcripts,
  leaving a duplicate Assistant header during incremental rendering. Numeric
  point and semantic line-prefix assertions now both pass.
- Reproduced review's full-recovery loss with the unchanged maintained stream
  assertion: focused view suite 1023/1024, only render-response/test@39 failed
  (`artifact://executions/execution-Qn8i2G.log`). Mandatory release now cancels
  only incremental scheduling, preserving full error recovery. Also corrected
  pending-entry docstring to include CLEANUP.
- Final required Eask clean plus `test ert test/test-mevedel-view*.el` passed
  1025/1025, zero unexpected, 11.98s ERT, no unexpected diagnostics:
  `artifact://executions/execution-WN9dYc.log`. Scoped diff --check and five-file
  check-parens also passed. No global compile/fullsuite/runtime work by this
  agent. Main may now perform them; note Eask cleanup removed concurrently
  generated bytecode during the earlier 17:57 run.
- Applied implementation files: mevedel-view-render.el, mevedel-view-stream.el,
  mevedel-view-agent.el. Applied test files: test/test-mevedel-view-render-reentry.el
  and test/test-mevedel-view-segments.el. Preserved all other edits and original
  broken live view. No commit.
- ToolSearch exposes no mail tool in this continuation; status recorded here.

## Final integration and runtime verification, main agent, 2026-09-16

This checkpoint supersedes the earlier outstanding-work status above.

- Reconciled resumed execution: all children idle, no yielded main commands.
- Accepted focused command passed 1138/1138, zero unexpected:
  `artifact://executions/execution-yKCjKy.log`.
- Complete `python3 test/run_tests.py`: 7955 cases, zero unexpected, 19
  conditional skips (remote acceptance/history and file-notification checks).
  Reports: `.scratch/test-suite-performance/20260916-183410/`; all eight workers
  exited zero. Logs contained no warning/error diagnostics.
- Full Eask compilation: 199 files, zero skipped, no warnings; cleanup removed
  199 generated repository bytecode files. Evidence:
  `artifact://executions/execution-lbbU2q.log`. Final git diff --check passed.
- Standards review: no documented-standard violations; corrected stale pending
  entry docstring. Spec review's one blocker (terminal release cancelling full
  recovery) was reproduced, fixed, and re-reviewed PASS. These review verdicts
  are scoped to review, not substitutes for the checks above.
- Rebuilt actual Straight mevedel package, loaded its rebuilt `.elc` files, then
  synchronously native-compiled and loaded the eleven changed implementation
  libraries. Disposable replay passed direct settlement, observer settlement,
  and stop against both bytecode and native code: one response copy, no stale
  tail, multiline `>` draft and point preserved. Actual loaded renderer unit:
  `/home/roland/.emacs.d/.local/cache/eln/31.1-8806c27d/mevedel-view-render-1ee58ad9-0b81ab29.eln`;
  native-comp-function-p was true for all three reported render entry points.
- Limitation: the original broken view is no longer open in the resumed Emacs
  (uptime about 30 minutes when checked). The final original-view hash check
  therefore could not run. No reopening, refresh, or reset of that view was
  performed. Earlier recorded hash remains historical evidence, not a new check.
- No commit was made; unrelated checkout changes were preserved. Exact natural
  production reentry remains unknown; deterministic injection proves this class
  of defect, not the original event sequence.
