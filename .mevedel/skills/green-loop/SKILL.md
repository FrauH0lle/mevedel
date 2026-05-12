---
name: green-loop
description: "Run a bounded validation loop for current changes: tests, compile/checks, reviewer, verifier, fixes, then repeat until green or capped. Use when user asks to make changes green, run review and verify, 'until everything passes', or prepare a change for final validation."
argument-hint: "What change should be validated?"
context: fork
agent: coordinator
allowed-tools:
  - "Bash(npx @emacs-eask/cli test ert test/test-*)"
  - "Bash(npx @emacs-eask/cli compile)"
  - "Bash(npx @emacs-eask/cli clean elc)"
  - "Bash(git diff*)"
  - "Bash(git status*)"
  - Agent
  - SendMessage
  - TaskCreate
  - TaskUpdate
  - TaskList
---

# Green Loop

Run an explicit, bounded validation loop for the current change. The goal is not to chase perfection indefinitely; it is to surface the real state of the work, fix actionable issues, and stop with a clear report.

## Loop contract

Use at most two review/verify rounds unless the user explicitly asks for more.

Each round:

1. Identify the changed files and the intended behavior.
2. Run the most focused relevant validation first.
   - Prefer matching ERT files when the changed module has one.
   - Run `npx @emacs-eask/cli compile` for shared or cross-module Elisp changes.
   - Use broader `npx @emacs-eask/cli test ert test/test-*` only when the change warrants it.
3. Run a reviewer for code-review findings when there are non-trivial diffs.
4. Run a verifier for adversarial validation. Treat `VERDICT: FAIL` as blocking.
5. Fix only actionable issues that are in scope for the requested change.
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
- the loop would need a third round.

## Constraints

- Do not make unrelated refactors while trying to get green.
- Do not hide failing checks. Report exact commands and failures.
- Do not use destructive Git commands.
- Do not edit generated artifacts such as `*.elc`.
- Keep reviewer/verifier findings tied to exact `file:line` references.

## Final report

Return:

- changed files,
- validation commands run and results,
- reviewer findings fixed or remaining,
- verifier verdict,
- remaining risks,
- whether another round is recommended.
