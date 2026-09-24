# Sleep inhibitor task findings

## Current run completed (2026-09-23; starting HEAD 915d82e)

Source: current main agent, /root/correctness_review and /root/standards_review.
Commit 8d21c1a on master contains the reviewed 17-file feature. Earlier completion
claims below are retained reports, not evidence for this working tree.

- Reproduced async terminal formatting leaks, stale fork delivery affecting a
  replacement, and nested WAIT capture overwrite. Identity-gated whole callback
  delivery and exact-owner cleanup fix these cases; insertion hooks and immediate
  pre-teardown rechecks preserve replacement owners.
- Latest 505 focused cases pass; 208 files compile without diagnostics. Both
  feature review axes PASS. Full run 20260923-154051: 8427 cases, zero unexpected,
  21 conditional skips; all workers successful and diagnostics clean. It repeats
  exactly the partitions/order of a preceding run with one HOME/XDG isolation
  failure in an existing cold-compile test. That case and its predecessor replay
  also pass; no source-confirmed cause found, and no check changed or skipped.
- `.scratch/sleep-inhibition/review.patch` is the feature-only committed snapshot.
  Reconstructed tracked blobs match starting HEAD + saved unrelated dirty patch
  + feature patch exactly. Post-commit index is empty; original unrelated hunks
  and untracked memory remain intact. New Lisp file modes are 100644; this
  metadata correction is the only post-review adjustment, with unchanged blobs.
- No native inhibitor or machine power action has been run. Native enforcement
  and macOS runtime remain unverified.

## Retained completion claim (2026-09-23; not current validation)

Source: user backlog request; main agent owns coding, /root/lifecycle and
/root/platforms investigated read-only. Commit d6a01dc on master contains the
reviewed 12-file feature patch. Post-commit check confirms an empty index and
all unrelated tracked hunks unchanged against baseline.patch after normalizing
diff hashes and offsets; committed patch equals the reviewed snapshot.

- One identity-owned local helper protects root and retained-agent lifetimes:
  systemd-inhibit sleep-only + pipe cat on Linux, direct caffeinate -i -w
  Emacs-PID on macOS. No native inhibitor was run; native power enforcement
  and macOS runtime remain unverified.
- Correctness review reproduced process-coding selector reentry cancelling
  admission. Parent ERT reproduced that, obsolete admission returning, and
  telemetry failure retaining a hold. Fixed helper coding to no-conversion,
  fenced ownership after acquisition, and protected root bookkeeping and agent
  acquisition with unwind cleanup. Reviewer independently reran the original
  harmless-cat reproducer successfully. Both independent review axes PASS.
- Final suite: 8424 tests, zero unexpected, 21 conditional skips, all eight
  workers exit 0; report .scratch/test-suite-performance/20260923-133900/.
  Worker diagnostics search is clean. 208 files compile without warnings;
  Eask clean elc ran afterward. Validation logs: revision-full-suite.log and
  revision-compile.log under .scratch/sleep-inhibition/.
- Test lesson: supplemental suites must not require a primary test file to
  share fixtures (full-suite discovery loads it twice). Shared harmless-process
  fixture belongs in test/helpers.el. Real generated buffers, not temporary
  buffers with suppressed hooks, exercise buffer-kill cleanup.

## Retained earlier run (2026-09-23, starting HEAD 915d82e; not current validation)

Source: user backlog request; current main agent and /root/sleep_lifecycle,
/root/sleep_platforms investigations. The reports below were retained evidence,
not proof of this starting tree: it had no sleep module or feature commit.

- Reconfirmed separate root admission and retained-agent dispatch. Protection
  needs identity ownership at both seams, plus terminal release before agent
  formatting and guaranteed release when root reminder restoration fails.
- Native-source investigation favors Linux sleep-only systemd-inhibit with
  pipe-bound cat; macOS directly supervised caffeinate -i -w Emacs-PID.
  macOS utility mode hides the assertion-owning child from Emacs's sentinel.
- Isolated tests use harmless shell/cat substitutes, never native inhibitors.
  A disposable batch Emacs crash test proves EOF cleanup without shutdown
  hooks. Ordinary disposable buffers are required for kill-hook tests:
  with-temp-buffer creates buffers with hooks suppressed.
- Correctness follow-up (/root/sleep_correctness_review) reproduced two gaps
  missed by the first static pass. Root reservation mismatch throws before
  deferred cleanup exists; release at the shared commit failure boundary without
  destroying retry state. Native process coding-selection quit strands a newly
  registered owner; roll back acquisition on nonlocal exit. The coding callback
  in process-coding-system-alist must be a function symbol, not a lambda.
  Parent reproduced both failures in ERT, then implemented the narrow fixes.
- 319 focused tests now pass; 208 source files compile without warnings.
  Final full suite: 8409 tests, zero unexpected, 21 conditional skips, all eight
  workers exit 0 (.scratch/test-suite-performance/20260923-101146). Worker logs
  have no product warning/message or Org notice matches. Final independent
  standards and correctness reviews both PASS; optional fixture guard removed.
- Commit 9de9404 on master contains exactly the reviewed eleven-file patch.
  Post-commit comparison confirms an empty index and every unrelated tracked
  hunk unchanged after normalizing diff offsets/blob hashes. No native inhibitor
  was run; native power enforcement and macOS runtime remain unverified.

## Retained earlier report (2026-09-23; not current validation)

Source: user backlog task, main agent and current /root/lifecycle and
/root/platforms investigations. The older report below was a lead, not current
verification; all lifecycle/backend claims used today were rechecked.

- Confirmed independent agent dispatch and pre-finalizer settlement release.
  Cover synchronous dispatch quit, no-FSM error, early buffer kill, uninstall,
  and root reminder-restoration failure as distinct cleanup boundaries.
- Current implementation uses one identity-keyed owner map and local helper.
  Linux selects sleep-only systemd-inhibit with --no-ask-password and pipe-bound
  cat. macOS uses caffeinate -i -w Emacs-PID without a utility. Windows is
  unsupported; failures warn and continue, retrying only on a new active interval.
- Independent standards/spec reviewers found fixture isolation defects: a
  caller-buffer cleanup hook leak and unstubbed native helper discovery in the
  first overlap case. Fix by using one shared fixture with a temporary buffer
  and controlled platform/discovery, not by muting failures.
- First full current suite: 8402 tests, zero unexpected, 21 conditional skips;
  worker logs contain no product warnings/messages or Org notices. Fixture
  corrections add one regression. Final revalidation: 8403 tests, zero
  unexpected, 21 skips, all eight workers exit 0 (20260923-080221 report).
  Final focused run: 184 tests passed; 208 files compile without warnings.
  Both independent follow-up reviews report VERDICT: PASS.
- No native inhibitor or power-management action was run. Native macOS remains
  source/manual-verified only. Unrelated performance changes are preserved;
  selective backlog staging leaves the original Inbox edits unstaged.
- Final preservation comparison finds all original unrelated hunks intact,
  plus one concurrent unstaged backlog note about compaction collapsing tools.
  Leave that note untouched and outside the feature-only commit.
- Main agent commits the 11 feature files on master as 59b4dbc
  (feat(sleep): Inhibit system sleep during active requests). Index is empty;
  unrelated changes remain unstaged. New Lisp files use normal 100644 mode.

## Historical report (2026-09-22; not current validation)

Source: user backlog implementation; read-only investigations by /root/lifecycle and /root/platforms.

- Root requests use mevedel-turn admission/cancel/end. Retained agents do not call request-begin; acquire separately at runtime dispatch and release at runtime finalization, not raw provider DONE (yielded executions may still run).
- systemd-inhibit's parent owns the inhibitor FD; its utility receives a parent-death signal. Pipe EOF closes the utility lifetime after editor death. Explicit sleep-only scope avoids display/idle inhibition.
- Apple caffeinate supports idle-system-only -i and PID exit monitoring -w; avoid -d and -u. Utility mode reverses parent/child roles and obscures assertion-child death, so use PID-watch mode instead.
- Emacs 31 system-sleep.el loading installs a persistent Linux delay inhibitor; it is not request-lifetime-neutral.
- No platform inhibition or machine power action was used during investigation. macOS runtime verification is unavailable on this Linux host.

Review correction (source: /root/correctness_review, reproduced by main):
agent settlement formats results and probes publication state before calling
the finalizer. Those operations can fail after provider work has ended. Release
at terminal settlement entry as well as direct finalization, but not raw provider
DONE when a yielded execution tail still runs. Callback fault-injection tests
reproduced the old leak, then passed with this fix while preserving the root's
overlapping hold. Both independent final reviews returned PASS; full-suite
validation completed: 8388 tests, zero unexpected, 21 conditional skips.
Final compilation completed for 208 files without warnings. Full-suite logs
contain out-of-scope Org buffer-local notices and one memory-review message;
the focused feature/lifecycle runs are quiet. Native macOS execution remains
unverified, and no real OS inhibitor or power action was used in tests.
