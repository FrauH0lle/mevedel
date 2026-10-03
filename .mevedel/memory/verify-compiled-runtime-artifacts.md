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
- The same build-tree resolution affects modules that derive a package root from the loaded file. `Read mevedel://tools/execution.md` failed with `Resource address escapes its owning root` (observed 2026-09-14, investigated further 2026-09-16): `mevedel-resource--source-dir` had selected the Straight build directory, the Markdown files reported there were symlinks into the checkout, and `mevedel-resource-within-root-p` returned nil because the containment guard rejects symlink components. Model inference from inspected code (`mevedel-resource.el:147,789,972,1510`): the defect is package-root selection, not invalid resource addresses.
  - Cheap confirmation of that diagnosis: temporarily bind the loaded resource root to the canonical directory of the sibling `.el` source. Observed 2026-09-16, both `mevedel://memory.md` and `mevedel://tools/execution.md` then read successfully through `mevedel-resource-prepare`/`mevedel-resource-execute`, and discovery found 124 Markdown files against 0 before the rebinding, with `:global-root-unchanged t`. No fix was implemented; the request to fix it and audit similar instances stayed planning-only in the captured evidence.
  - Two other package-root initializers already resolve the sibling `.el` when loading `.elc` (`mevedel-system.el:83`, `mevedel-tool-registry.el:49`, `mevedel-skills-core.el:122`), so system/tool prompts and bundled skills avoid this particular root-selection defect while resource roots did not.
  - `mevedel-resource--source-dir` is a `defvar`, so reloading edited source alone does not reset its existing value; a stale value can keep the guard and discovery pointed at the wrong root.
  - Model inference, not runtime-tested (`mevedel-telemetry.el:675–690`): `mevedel-telemetry--library-snapshot` uses `locate-library`, which identifies what would load now rather than necessarily the loaded artifact, and infers provenance by searching for `.git` from that path — a compiled build layout can lose source-checkout provenance.

The related stale-`.elc` trap for the Eask test command is covered by the existing `eask clean before tests` topic; this topic covers live, loaded-code verification.
