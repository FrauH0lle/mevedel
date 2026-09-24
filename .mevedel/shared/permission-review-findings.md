# Permission-review boundary findings

Source/task: parent review-improve of f4c42f9 from f3294663, 2026-09-18.

- Observed: gptel snapshots request settings from its `:buffer` using
  `buffer-local-value`. Caller-local dynamic bindings did not put the guardian
  model/system policy into the serialized review request. Reproduced with an
  offline native dry run in test/test-mevedel-permission-review.el; request-buffer
  locals correct it. No provider accuracy/cost claim follows from this test.
- Observed: generic permission-card presentation does not imply native-tool
  authority. Hook-tightened Bash and Eval keep their actual child/live-Emacs
  boundary. Live Eval runs in host Emacs even with a remote session target.
- Observed: queue revalidation may return `(deny . REASON)`, not only `deny`.
  Comparing only the bare symbol admitted an obsolete automatic approval.
- Hypothesis with timing evidence: the initial full-suite Bubblewrap refusal
  coincided with activation of the host binfmt_misc automount. The unchanged
  profile file passed all six tests on rerun and ten standalone launch probes
  passed. Exact mount trigger unproved; do not hide the original failed run or
  weaken fail-closed launcher checks. Evidence in .scratch/review-f4c42f9/ and
  agent://root/bubblewrap_failure.
- Observed in subsequent stack/A-B probes: a local hook sentinel advanced into
  remote handlers while diagnostic-flush TRAMP I/O was active. Removing the
  identity fence's sandbox probe exposed the schedule; warm-up was not the root
  fix. Command completion now reuses transport-idle deferral, retaining request
  cancellation and the first terminal result. Real mock-TRAMP regression and
  queued cancel/advance tests pass; full-final-r1 has 8057 cases, zero unexpected.
- Observed by verifier_r1 and reproduced by parent: exactly-once settlement and
  eventual provider cleanup did not prevent a new request after cancellation
  during yielding evidence I/O. Guards after evidence and validation now stop
  startup; real-timer regressions cover cancellation and timeout at both stages.
  Parent full-r2: 8058 cases, 8039 passed, 19 conditional skips, zero unexpected;
  re-review remains in progress, not final certification.

- Observed by correctness_r2 and reproduced by parent: actual Bash/Eval policy
  must be reevaluated even for generic hook cards, including segment and added
  capability denies. Exact-directory mount representability applies only to
  confined/refused-required child execution, not disclosed unconfined work.
- Observed by verifier_r2 and reproduced by parent: queue reevaluation alone
  misses a held PermissionRequest callback before queue admission. A Full Access
  transition could authorize despite newly installed segment/network denies.
  Captured-entry policy reevaluation now also guards that pre-queue settlement;
  the focused six-file suite passes368/368, including another-buffer resumption.
- Unresolved observation: full-post-r2 failed an untouched durability test's
  manual-recovery-local assertion. Unchanged file98/98, full-roster single1/1 and
  original worker-prefix156/156 subsequently passed. Instrumented actual
  containment predicates and pre/post-refresh state were correct on replay.
  No root cause established; no durability assertion or source was changed.
- Observed by spec_r3 and reproduced by parent: permission entries dropped the
  prepared session-only ApplyPatch classification, so automatic review denied
  valid Plan work edits. Capture that validated fact and preserve it through
  both context adapters and queue revalidation; authored patch text is not proof
  of Plan eligibility. Actual prepared-work allow-once/ask regressions pass.
- Observed by correctness_r3 and reproduced by parent: forcing identity fences
  to probe `off` alternated the mode-sensitive cache with required confinement,
  repeating full remote readiness probes. Reuse the cached mode, freshly observe
  identity even for sandbox-unavailable readiness, and keep that child readiness
  blocked in its owning cache. Probe counters establish this, not wall latency.
- Unresolved observation: remote-r3 requester aborted on the local nested-Eask
  HOME/XDG containment guard before session startup. Diagnostic replay passed
  28/32 with four declared skips; guard unchanged. No failing predicate captured
  or root cause established. Relationship to durability containment is only a
  hypothesis. Full post-r3 had zero unexpected results with one additional
  availability-dependent skip. Verifier_r3 independently passed that case.
- Observed by verifier_r3 and reproduced by parent: the earlier pre-queue policy
  fix covered Full Access only. Held hook allows in unchanged Ask/Edits still
  bypassed newly installed segment/network denies, for both reviewer modes.
  Current policy now revalidates in common approval settlement, not a mode branch.
- Observed by verifier_r3 and reproduced by parent: request-end during
  permission-wait ran before queue/reviewer registration; a late approval then
  executed live Eval. Request cancellation now records terminal state before
  cleanup, immediately runs late cleanup registrations, and blocks new permission
  admission. Review startup respects synchronous registration-time cancellation.
  First corrected focused suite passed511/511; final validation is recorded below.
- Verification lesson: test before queue admission, not only already-pending
  review cancellation or mode transitions. Verifier_r3 returned FAIL on the
  preceding23-path snapshot despite941/941 independent suite passes. The final
  parent corrections span28 paths and await independent verification at the
  three-round cap. No fourth round and no clean-result claim.

- Parent final broad run initially failed four checks with eight sandbox fallback
  warnings. Unchanged four-file isolation passed212/212. Three child-execution/
  naming failures remain unexplained, not dismissed as harmless flakes.
- Confirmed fixture defect: the hook directory-write test dynamically let-bound
  permission mode but had no session or local buffer mode. Actual policy was
  Ask/best-effort; forced unavailable confinement reproduced its missing denial.
  The fixture now establishes real buffer-local Edits and tests both availability
  branches without relaxing the directory-denial assertions. Instrumented original
  worker prefix passed577/577; post-fix focused suites passed99/99. Source policy
  was not changed to compensate for this harness defect.
- Final parent replay exec000156 passed8047/8066 with19 skips and zero unexpected;
  all four previously failing cases and five additional earlier skipped cases
  passed. No worker diagnostic matches. Compile built200 files without warnings;
  uninstrumented SSH/Podman acceptance passed28/32 with4 declared skips. These
  results do not establish causes of earlier unexplained failures or warnings.
- Selectively amended local reviewed HEAD to
  7a5949d93b38c577d5f30e46a5d42a28ad0e0c2a,28 correction paths; unrelated work
  preserved. No push or live Emacs reload. Evidence and full disposition remain
  in .scratch/review-f4c42f9/contract.md. Independent certification is unavailable
  at the three-round cap; final verifier FAIL predates the last corrections.

These are observations from a capped review, not a clean independent signoff.

User subsequently authorized exactly one additional complete review round on
2026-09-18. Round4 entry is the same7a5949d snapshot against originalf3294663;
all five reviews and a separate verifier are required. This is a task-specific
cap override, not a skill edit or permission for round5. Approval-bundle redesign
remains out of scope. See .scratch/review-f4c42f9/round4-brief.md for coordination.

Round4 parent observations (2026-09-18): all five reviewers finished on7a5949d.
Spec/correctness reproduced public abort draining before terminal state; structural/
correctness reproduced hook startup after synchronous cancellation; correctness
reproduced a sibling lost while approval validation yielded. Parent regressions now
cover those boundaries, with owner-local fixes and360/360 focused cases passing.
Hook late-resource release must preserve a transport-deferred completion; cancellation
owns cancelling that delivery. Full/compile/remote run is pending, and separate
adversarial verification has not started. No clean signoff follows from focused green.
Details and red-run fixture corrections: .scratch/review-f4c42f9/contract.md.
Concurrent unrelated agent lifecycle/view edits are excluded from the correction
paths; tests in this checkout also exercise those edits.

Round4 broad validation:8053 passed/8072 with19 skips,0 unexpected; warning-free
compile. Remote acceptance had1 timeout failure among32 (27 pass,4 skip). Auxiliary
instrumented SSH and paired current/old-handler replays passed but did not reproduce
or diagnose the timeout; subset ordering differed. Do not promote green replays to
a cause or claim it was harmless. Final verifier owns executable checks now and
will run an uninstrumented original-order remote replay. No speculative fix applied.

Final round4 outcome: verifier_r4 completed independent386/386 and874/874 suites,
seven fresh boundary-probe groups, and uninstrumented original-order remote28/32
with4 declared skips and0 unexpected. VERDICT: PASS on corrected state; prior timeout
still undiagnosed. Static cumulative coverage was targeted, not exhaustive87-path
line coverage; five axis reports precede corrections. No fifth round.
Parent selectively amended exactly13 correction paths into
cac27c1a3c38ad2e9b94578e0124e6f7495d322e; committed delta equals inspected staging.
Unrelated21 modified paths and2 untracked entries remain; no push or live reload.
Complete evidence/commands/limitations: .scratch/review-f4c42f9/contract.md.

## Focused timeout follow-up (parent, 2026-09-18)

User authorized diagnosis and implementation, not another review round. Reproduced
the same10s SSH hook failure by coalescing final TRAMP exec and JSON writes without
changing bytes/order: preceding shell read-ahead loses stdin on exec. Implemented
hook-owned readiness acknowledgment, early-buffer/filter handling, and guarded
exact-once stdin delivery; timeout/cancellation unchanged, payload not in argv.
Five maintained paths changed: hook owner, two test files, hooks manual, ADR0066.
Parent: old-handler maintained regression red; full8054/8073 with19 skips and no
unexpected; warning-free compile; actual SSH/Podman coalescing regression included
in remote28/32 with4 skips and no unexpected. Independent verifier PASS:126/126
focused plus large-payload integrity and real-backpressure cancellation probes.
The imposed reachable schedule establishes the bug; it cannot retrospectively
prove the cause of the original uninstrumented timeout. Combined-tree validation;
other-agent changes preserved, index empty, no commit/push/live runtime reload.
Detailed evidence and excluded instrumentation failures:
`.scratch/remote-hook-timeout/notes.md`. Independent executable slot released.
