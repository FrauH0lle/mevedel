---
name: review-improve
description: "Run a bounded corrective review of cumulative changes since a fixed point using independent Standards/Spec, thermo-nuclear maintainability, ponytail complexity, and correctness reviews, followed by adversarial verification. Use for a rigorous review, fix, and re-review workflow that must preserve unrelated work and finish with verified, disciplined corrections; do not use for report-only reviews."
---

# Review and Improve

Review the full cumulative change since the user-supplied fixed point, correct
confirmed issues, and repeat until clean or three rounds have completed. Keep
implementation in the owning request; review and verification agents are read-only.

Do not fundamentally change the implementation without the user's permission.

## Establish the review contract

1. Read the repository guidance and the maintained docs relevant to the diff.
2. Record `git status`, the unrelated working-tree changes that must remain
   untouched, and the current `HEAD`.
3. Resolve the fixed point and require a non-empty three-dot diff. If the user
   supplied a contiguous commit list ending at `HEAD`, use the parent of the
   oldest listed commit as the fixed point; otherwise require an explicit fixed
   point. Capture:
   - `git diff <fixed-point>...HEAD`
   - `git log <fixed-point>..HEAD --oneline`
4. Locate the originating issue or PRD using the repository's issue-tracker
   workflow, explicit user paths, commit references, branch-matching local
   planning files, and maintained docs. If no separate spec exists, use the
   user's stated commit intents and applicable maintained documentation, and
   explicitly report that no external spec was available.
5. When the change touches gptel-coupled behavior named by repository guidance,
   refresh and consult the current gptel source before judging
   behavior.

The review scope is the fixed point through the current working tree. On later
rounds, include uncommitted corrections and new in-scope files in addition to
the committed three-dot diff; never let a review silently omit them.

## Review loop

Run at most three complete rounds. Each complete round uses six independent
agent invocations: five reviewers, then one adversarial verifier on the
post-correction state. This is not a requirement to run six agents at once.
Run focused validation before the first review batch so reviewers have actual
check results. In every round apply all three attached review contracts:

!$code-review
!$thermo-nuclear-code-quality-review
!$ponytail:ponytail-review

- Code review: run its Standards and Spec axes independently as prescribed.
  Keep those two reports separate. Supply the fixed point, commit list,
  standards sources, spec source or fallback intent, and the full current
  cumulative change.
- Thermo-nuclear code-quality review: audit the same cumulative change for
  structural and maintainability regressions. Treat ambitious restructuring as
  advice only when it would fundamentally alter the implementation; ask before
  applying it.
- Ponytail review: independently identify code that can be deleted,
  replaced by existing/stdlib facilities, or shortened without weakening the
  contract.
- Correctness review: use the dedicated `reviewer` agent with its existing
  `/review` contract in `agents/reviewer.md`, not another specialized skill
  review. Supply the full current cumulative change and intended behavior.
  Preserve its scope: concrete regressions in correctness, performance,
  security, and maintainability, with triggering inputs/environments and
  affected callers. Keep its prioritized JSON findings and overall correctness
  verdict intact. Spec compliance is not a substitute for this review.

Run the Standards, Spec, thermo-nuclear, ponytail, and correctness reviewers
independently and in parallel when capacity permits; batch them when capacity
is lower than five. Do not impose the dedicated reviewer's JSON contract on
the four specialized reviews. Preserve all five outputs as distinct reports.
Consolidate only actionable, high-confidence findings after checking
each one against surrounding code, callers, tests, maintained docs, and
required upstream source. Reject conflicting, speculative, compatibility-only,
or out-of-scope advice.

For every confirmed finding:

1. Fix the root cause with the smallest direct change.
2. Follow the repository's no-backwards-compatibility policy: remove superseded
   paths and update all in-repo callers rather than adding shims.
3. Update focused tests and maintained docs or ADRs when behavior or design
   changes.
4. Avoid unrelated cleanup, new dependencies, speculative abstractions, and
   implementation by review or verification sub-agents.

Validate every round, even when no correction was needed, and re-run relevant
checks after each correction batch:

1. Run `git diff --check`.
2. Run `npx @emacs-eask/cli clean elc` before tests.
3. Run focused tests for every touched behavior.
4. Run proportionate broader tests and `npx @emacs-eask/cli compile`.
5. Resolve diagnostics in touched files; do not hide failing checks.

After fixes and validation, run the dedicated `verifier` agent with its existing
contract in `agents/verifier.md`. Supply the intended behavior, full resulting
cumulative change, review findings and their disposition, and exact validation
commands and results. Require independent checks of changed behavior, edge
cases, error paths, and concurrency where relevant, including a suitable
adversarial probe before PASS. The verifier must not merely endorse the prior
reviews or test summary.

Require exactly one final `VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL`
line. Check that the cited observations support the verdict; report shape alone
does not establish correctness. Reproduce uncertain or consequential claims.
FAIL blocks completion. PARTIAL means verification is incomplete due to an
environmental limitation, not success or permission to leave feasible checks
unfinished. Report blockers and ask when resolution requires unavailable
access, a scope expansion, or a change to the user's requested direction.

Fix confirmed in-scope verifier findings in the main request and re-run relevant
checks. Any correction after a review or verifier report makes that report
stale for the changed behavior: start the next round with all five reviews and
finish with a new verifier report, within the three-round cap.

Stop early only when all five reviews have no unresolved actionable findings,
validation passes, no unresolved diagnostics remain in touched files, and the
verifier returns an evidence-supported PASS for the final state. Corrections
require the next round; do not claim a clean result from pre-correction reports.
Never exceed three rounds. At the cap, report remaining findings, blockers, and
any corrections still awaiting re-review or verification instead of starting a
fourth round or declaring them clean.

## Commit discipline

- Preserve unrelated staged, unstaged, and untracked work.
- If no correction was needed, create no commit.
- Prefer selectively staging the corrections and folding them into the existing
  reviewed `HEAD` with `git commit --amend --no-edit` when that is safe.
- If amending `HEAD` is unsafe, create exactly one correction commit. Amend that
  same correction commit for every later correction; never create multiple
  correction commits.
- Inspect the staged diff before committing. Do not stage unrelated paths.

## Final report

Report:

- findings and fixes for each round, keeping Standards, Spec,
  thermo-nuclear, ponytail, and correctness results distinguishable;
- exact validation commands and results;
- final status and coverage limitations for each of the five reviews;
- adversarial checks, the verifier's final verdict, and whether it covers the
  final state;
- final commit hash and whether it amended the reviewed commit, created the
  single correction commit, or made no commit;
- unresolved findings with a concise reason;
- unrelated working-tree changes that were deliberately preserved.
