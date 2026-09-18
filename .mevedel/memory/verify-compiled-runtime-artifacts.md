---
name: "Verify against the compiled library the runtime loads"
description: "Repository edits do not reach the running Emacs until Straight's compiled build artifacts are rebuilt; live availability is not cold-start correctness"
type: project
---

# Verify against the compiled library the runtime loads

In this environment the running Emacs loads `~/.emacs.d/straight/build/mevedel/mevedel-*.elc`. The build tree's `.el` files are symlinks back to the repository, but each `.elc` is a separate compiled copy with its own mtime.

With `load-prefer-newer` nil, the older `.elc` keeps winning after repository edits. Observed 2026-09-15: repository mtime 08:07:32 against build `.elc` mtime 07:14:46, and a live Eval still ran the previous behavior.

- Hot-loading with `(load ".../mevedel-X.el" nil t t)` can confirm an edited source quickly, but it does not recompile the build tree and does not survive a restart. Treat hot-loaded results as provisional and re-check after rebuilding.
- Do not judge presence or absence of a change from a literal text search or `symbol-file` alone. Macroexpansion (for example from `setq-local`) can hide the token you are looking for while the behavior is present; verify behavior, not spelling.
- Live availability is not cold-start correctness. `featurep`/`fboundp` true in an inspecting session only show what the loaded build artifact already provides. Observed 2026-09-16: `void-function (mevedel-view--resume-attended-views)` came from the install path registering a focus callback without loading the module that owns it, so a focus change before the first chat view opened called an undefined function — while the inspecting session already had both `featurep` and `fboundp` true from Straight's compiled files. Existing coverage masked this because it required `mevedel-view` and stubbed the callback, so a fresh `emacs --batch -Q` regression was needed to reproduce the exact error before the fix. When verifying a load-order, registration, or startup change, use a fresh process rather than a session that already loaded the owning module.
- The same build-tree resolution affects modules that derive a package root from the loaded file: on 2026-09-14 `Read mevedel://tools/execution.md` failed with `Resource address escapes its owning root`. The investigation attributed this to the loaded `.elc` resolving under Straight's build tree rather than the checkout and proposed preferring an `.el` sibling, but no fix was implemented and the aggregate `mevedel-resource--file-list` probe was not retained.

The related stale-`.elc` trap for the Eask test command is covered by the existing `eask clean before tests` topic; this topic covers live, loaded-code verification.
