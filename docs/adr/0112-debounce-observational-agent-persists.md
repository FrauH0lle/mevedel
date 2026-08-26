# Debounce observational agent persists

Status: accepted

A profiled session (2026-08-25) wrote 434 immutable publication generations
(227MB, peak 21/minute).  Session publication machinery accounted for roughly
30% of 8.6GB of allocations in a 31-minute window while garbage collection
took 60% of CPU samples.  The dominant trigger was
`mevedel-agent-control--persist-session`: every agent activity transition
forced a full synchronous sidecar publication, and a single permission prompt
produced two — one when `block-turn` marked the agent blocked, one when its
release closure marked it running again.  With 763 permission decisions in
the session, observational flavor changes alone produced hundreds of
generations whose content the next acknowledged commit rewrote anyway.

## Decision

Agent persistence splits along the seam the code already had:

- **Acknowledged mutations stay synchronous.**  Spawn registration, mailbox
  enqueue, RESULT publication, and settlement call
  `mevedel-agent-control-commit-session`, which forces the save and refuses
  reentrant queueing.  Their contract (this repository's `docs/sessions.md`
  and ADR 0064) is unchanged: the caller returns only after the batch changes
  the immutable head.
- **Observational persists debounce.**  `mevedel-agent-control--persist-session`
  — reached from activity transitions, mailbox consumption, reservation
  rollback, and follow-up dispatch failure, all already best-effort with
  swallowed errors — schedules
  `mevedel-session-persistence-save-agent-state-soon`: one timer per session
  (`mevedel-session-persistence-agent-save-debounce`, 2s), handed to
  `mevedel-transport-run-when-idle`, landing as one plain non-forced
  `mevedel-session-artifacts-save`.  The non-forced save keeps the
  byte-comparison elision, so a pending save whose content a synchronous
  commit already published costs no target transaction; the synchronous
  commit also cancels a pending one outright.  The deferred thunk re-arms
  instead of writing while a critical publication is active, and the
  kill-emacs hook flushes pending saves inline because registry mutations do
  not mark the root buffer modified.

Crash inside the debounce window loses only observational flavor:
`mevedel-agent-control-recover-interrupted` treats every active activity
identically, and mailbox delivery is at-least-once, so a consumed-but-
unpersisted message re-delivers exactly as it already could when persistence
errors were swallowed.

## Diagnostic streams append the delta

The same profile showed `mevedel-session-publication-append-diagnostic`
re-reading and rewriting the whole diagnostic file per flush — quadratic in
stream size, ~10-14MB of base64 traffic per flush at a 5.2MB telemetry log.
The control filesystem gained an `append` verb under the same pinned-parent
and symlink-refusal proofs, and the diagnostic path now sends one operation
carrying only the delta.  Append works in place: a crash mid-operation can
tear one trailing line.  That ceiling is accepted because the streams are
single-writer under the reserved lease, line-oriented, and never read at
resume; the upgrade path is write-to-temp plus concatenation if a consumer
ever parses them strictly.
