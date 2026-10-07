# Lifecycle review, 2026-10-07

Scope: branch changes since `5adfcfb` in Goal accounting/continuation,
retained-agent dispatch/settlement, pending-input recovery, session codec and
checkpoints, journal scheduling, and the memory-review workload. Consulted the
PRD in the implementation worktree, current development and workflow contracts,
and upstream gptel request/abort behavior.

## Fixed: local recovery could not requeue interrupted follow-ups

The new session codec correctly restores a queued entry persisted as
`dispatching` as `failed-turn`, and automatic delivery refuses that state.
However, the Emacs Pending Inputs cockpit still treated only steering entries
as recoverable failed input. `R` cleared the failure pause without examining
failed follow-ups, while `f` rejected them as already being follow-ups. Editing
retained the failure state. The queue therefore remained blocked after an
apparently successful recovery, and the user could only delete the entry.

`f` now explicitly requeues a failed follow-up at the follow-up tail, retaining
its identity, attachment ownership, guest attribution, directive scope, and
skill restrictions. The existing steering conversion shares this path. `R`
checks both categories and requires each failed entry to be requeued or deleted
before clearing the failure pause. No automatic replay is introduced.

Two regressions exercise the actual cockpit commands, including queue order,
absence of duplicate entries, preserved metadata, and rejection before review.
The local interaction contract is updated in `docs/view.md`.

## Verification

- New tests against the original `HEAD` implementation, loaded only into an
  isolated Eask test process: exactly the two new cases failed (58/60 passed).
  Log: `.scratch/lifecycle-before.log`.
- Updated implementation: pending-input and test-runner suites, 62/62 passed.
  Log: `.scratch/lifecycle-focused.log`.
- Whole-tree test-file integrity: 2/2 passed, including the count of top-level
  test definitions. Log: `.scratch/lifecycle-integrity.log`.
- `git diff --check` passed. Compilation and integrated full-suite verification
  are coordinated by the parent review agent.

The first local test attempt exposed and corrected a missing closing
parenthesis in a newly inserted case; a second exposed an invalid directive
scope in its fixture. Both were test-construction mistakes and are corrected in
the passing evidence above.

## Fixed: local model selection retained a blocking unavailable-model error

Provider errors retain a blocking issue with ID `request`; native preflight can
report an unavailable model under ID `authentication`. Browser model recovery
cleared these IDs, but the normal local selection path did not. Its subsequent
readiness check cleared only `model` and refused the newly selected provider
because the old blocking error survived. This prevented local repair through
the advertised model picker.

The shared provider-selection seam now clears root issues in the `model`
category, across the three IDs used by model selection, dispatch and readiness.
It retains unrelated authentication, configuration, history, and input failures,
as well as the pending-input failure pause. The selected provider still passes
ordinary readiness validation. A regression exercises both successful readiness
after selection and continued rejection for unrelated holds. The implemented
contract is recorded in `docs/sessions.md`.

Models plus test-file integrity: 53/53 passed (`.scratch/lifecycle-models.log`).
Against the original `HEAD` model implementation, exactly the new regression
fails (50/51 passed; `.scratch/lifecycle-models-before.log`).

No additional correctness defect was established in the Goal/agent accounting,
native history settlement, or journal and memory workload changes inspected.
This is a bounded review statement, not proof of every engine behavior.
