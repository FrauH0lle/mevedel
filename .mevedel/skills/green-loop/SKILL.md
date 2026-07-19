---
name: green-loop
description: "Run a bounded validation loop for current changes: tests, compile/checks, reviewer, verifier, fixes, then repeat until green or capped. Use when user asks to make changes green, run review and verify, 'until everything passes', or prepare a change for final validation."
argument-hint: "What change should be validated?"
context: inline
allowed-tools:
  - "Bash(npx @emacs-eask/cli test ert test/test-*)"
  - "Bash(npx @emacs-eask/cli compile)"
  - "Bash(npx @emacs-eask/cli clean elc)"
  - "Bash(git diff*)"
  - "Bash(git status*)"
  - Agent
  - ToolSearch
---

# Green Loop

Run an explicit, bounded validation loop for the current change in the active
main request. The goal is not to chase perfection indefinitely; it is to surface
the real state of the work, fix actionable issues, and stop with a clear report.

This skill runs inline on purpose. Apply in-scope fixes directly in the main
loop when edit tools are available. Use agents only for read-only review,
verification, or focused investigation.

## Loop contract

Use at most three review/verify rounds unless the user explicitly asks for more.

Each round:

1. Identify the changed files and the intended behavior.
2. Run the most focused relevant validation first.
   - Prefer matching ERT files when the changed module has one.
   - Run `npx @emacs-eask/cli compile` for shared or cross-module Elisp changes.
   - Use broader `npx @emacs-eask/cli test ert test/test-*` only when the change
     warrants it.
3. Run a reviewer for code-review findings when there are non-trivial diffs.
4. Run a verifier for adversarial validation. Treat `VERDICT: FAIL` as blocking.
5. Fix only actionable issues that are in scope for the requested change. Make
   these edits yourself in the main request; do not ask an explorer, reviewer,
   or verifier to edit.
6. Re-run the failing or most relevant validation after each fix batch.

Stop early when:

- focused validation passes,
- reviewer has no blocking findings,
- verifier returns `VERDICT: PASS`, and
- no unresolved diagnostics remain in touched files.

Stop and ask when:

- findings contradict the user's requested direction,
- the fix requires a scope expansion,
- validation needs an unavailable environment, or
- edit tools are unavailable in the current preset,
- the loop would need a third round.

## Constraints

- Do not make unrelated refactors while trying to get green.
- Do not hide failing checks. Report exact commands and failures.
- Do not use destructive Git commands.
- Do not edit generated artifacts such as `*.elc`.
- Keep reviewer/verifier findings tied to exact `file:line` references.
- Do not delegate implementation to read-only agents. They may return analysis,
  review findings, or verification results only.

## Final report

Return:

- changed files,
- validation commands run and results,
- reviewer findings fixed or remaining,
- verifier verdict,
- remaining risks,
- whether another round is recommended.
