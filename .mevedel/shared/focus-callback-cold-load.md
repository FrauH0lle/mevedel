# Focus callback cold-load fix

Task: user reported `apply: Symbol's function definition is void: mevedel-view--resume-attended-views` (2026-09-16).

- Reproduced the exact void-function in a fresh Emacs subprocess through `mevedel-install` followed by `after-focus-change-function`. Existing tests loaded the view and stubbed the callback, hiding the missing dependency.
- Added `(require 'mevedel-view)` at the install boundary in `mevedel.el`; added an isolated cold-install/uninstall/reinstall case in `test/test-mevedel-chat.el`.
- Eask chat, view, and view-stream suites: 211/211 passed. Package compilation emitted no warnings; Eask cleaned bytecode afterward. `git diff --check` passed.
- Live inspection found the callback already defined from Straight's `mevedel-view.elc`; no live reload or Straight rebuild performed. The source fix still needs deployment to that compiled installation for future startups.
- Concurrent rendering/docs edits appeared during validation; they were not authored or changed by this task.
