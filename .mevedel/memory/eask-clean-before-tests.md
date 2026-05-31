---
name: eask clean before tests
description: User wants Eask bytecode cleaned before running project tests.
type: feedback
---

Run `npx @emacs-eask/cli clean elc` before project tests.

**Why:** Stale `.elc` files often shadow edited source files in this project and can produce misleading test failures.
**How to apply:** Before invoking ERT through Eask, run `npx @emacs-eask/cli clean elc`, then run the focused or broader test command.
