---
name: review-improve
description: "Run a bounded corrective review of cumulative changes since a fixed point using independent Standards/Spec, thermo-nuclear maintainability, and ponytail complexity reviews. Use for a rigorous review, fix, and re-review workflow that must preserve unrelated work and finish with verified, disciplined corrections; do not use for report-only reviews."
---

# Review and Improve

Review the full cumulative change since the user-supplied fixed point, correct
confirmed issues, and repeat until clean or three rounds have completed. Keep
implementation in the owning request; review agents are read-only.

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
   refresh and consult the current gptel and gptel-agent source before judging
   behavior.

The review scope is the fixed point through the current working tree. On later
rounds, include uncommitted corrections and new in-scope files in addition to
the committed three-dot diff; never let a review silently omit them.

## Review loop

Run at most three complete rounds. In every round apply all three attached
review contracts:

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

Run the Standards, Spec, thermo-nuclear, and ponytail reviewers independently
and in parallel when capacity permits. Preserve their four outputs as distinct
reports. Consolidate only actionable, high-confidence findings after checking
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
   implementation by review sub-agents.

After each correction batch:

1. Run `git diff --check`.
2. Run `npx @emacs-eask/cli clean elc` before tests.
3. Run focused tests for every touched behavior.
4. Run proportionate broader tests and `npx @emacs-eask/cli compile`.
5. Re-review the resulting cumulative change under all three attached contracts
   unless this was the third round.

Stop early only when Standards, Spec, thermo-nuclear, and ponytail reviews have
no actionable findings and verification passes. Never exceed three rounds. If
the third round still has findings, report them unresolved instead of starting
a fourth round.

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
  thermo-nuclear, and ponytail results distinguishable;
- exact validation commands and results;
- final status for each review skill;
- final commit hash and whether it amended the reviewed commit, created the
  single correction commit, or made no commit;
- unresolved findings with a concise reason;
- unrelated working-tree changes that were deliberately preserved.
