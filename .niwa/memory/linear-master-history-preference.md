---
name: Land branches on master as a linear, squashed history
description: Combine related commits before landing and avoid merge commits on master
type: feedback
modified: 2026-11-20
---

Before a feature branch lands on `master`, combine commits that belong
together (not necessarily into one) and land it without merge commits such as
"Merge branch 'worktree/fc-11'": rebase onto `master` and fast-forward.

**Why:** the user stated this when integrating both the docked-composer and
the schedules work on 2026-11-20; `--no-ff` ticket merges and back-merges of
the integration tip had produced a noisy, non-linear history.

**How to apply:** ticket-level `--no-ff` merges on a private integration
branch are fine while working, but before landing rebuild the branch as a
few coherent commits (each a tested integration state where possible, with
standards-conforming messages), verify the final tree equals the tested one,
rebase onto current `master`, rerun the merge-time checks, then fast-forward.
Ask before moving `master` itself.
