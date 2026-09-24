# Packaged documentation root under Straight

Investigation requested by the user, 2026-09-14: Read of
`mevedel://tools/execution.md` reports that the address escapes its owning root.

Confirmed through the live Read tool and read-only Eval probes:
- The loaded resource library is a regular `.elc` under Straight's build tree.
- `mevedel-resource--source-dir` points to that build directory. Its initializer
  canonicalizes the loaded `.elc`, not the sibling symlinked `.el` source.
- All 124 Markdown files under the build's `docs/` are symlinks into the project
  checkout. `mevedel-resource--file-list` consequently returns zero files.
- `mevedel-resource-within-root-p` rejects the reported file under the build
  root; the corresponding checkout file passes under the checkout docs root.

Relevant code: `mevedel-resource.el:147`, `:972`, `:789`, `:804`, `:1510`.
The provider test at `test/test-mevedel-resource.el:503` uses ordinary files,
not a byte-compiled Straight-style installation.

Implementation for the accepted plan, 2026-09-16 (main agent; telemetry delegated
to `/root/telemetry`):
- Added `mevedel-library-source-directory` in utilities and used it in the
  resource, system-prompt, and tool-registry initializers. Existing regular
  sibling source identifies the owner; absent/dangling source keeps the compiled
  directory. Resource containment is unchanged.
- Audit found system/tool roots already followed sibling source, while telemetry
  searched Git from the build path and used `locate-library` instead of loaded
  feature history. Telemetry now separates loaded artifact disk hashes from
  source-checkout provenance.
- Regression fixtures reproduce the original Read failure in cold Emacs and
  now pass for both linked-source and source-less compiled layouts. Focused Eask
  run: 858 passed, one existing skill-install skip, zero unexpected. Independent
  verifier found an additional telemetry bug: `locate-dominating-file` abbreviates
  home paths, but subprocess Git does not expand `~`. Reproduced with isolated
  HOME fixtures, fixed with `expand-file-name`, and verified 54 telemetry tests
  pass (`.scratch/package-root-home-{red,green}.log`).
- Final compilation: 199 files, no warnings (`.scratch/package-root-final-compile.log`).
  Post-fix full suite: 7902 cases, zero unexpected, 17 skips, exit 0; reports at
  `.scratch/test-suite-performance/20260916-150104/`. Eask cleanup completed;
  repository-root bytecode is absent. Static standards and independent installed
  runtime verification passed; the verifier rechecked abbreviated-root Git
  provenance after rebuilding the corrected installed telemetry artifact.
- Rebuilt only the five affected Straight `.elc` files via confined batch Eval.
  Fresh Emacs loaded the installed compiled resolver, resolved the checkout root,
  read both reported manuals, and listed 124 docs. No live globals were changed:
  this running Emacs retains its old `defvar` value until restart or explicit
  refresh. Do not mistake rebuilt disk artifacts for a live hot-reload.
- Committed the eleven task files on `master` as `afb7204`; unrelated working
  changes remain unstaged. The new installation test has normal 0644 file mode.
