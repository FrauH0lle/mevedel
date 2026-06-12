---
name: Forward declaration formatting feedback
description: Keep Emacs Lisp forward declaration groups sorted with declare-function entries before defvars
type: feedback
---

Forward declaration groups in Emacs Lisp files should be sorted by source package. Within each source-package group, put all `declare-function` forms first alphabetically, then all `defvar` forms alphabetically.

**Why:** Mixing `declare-function` and `defvar` entries while sorting made the declaration blocks harder to scan and did not match the user's intended convention.

**How to apply:** When editing file-top forward declarations, keep `;; \`package'` groups alphabetized, and sort entries in two phases: `declare-function` entries first, then `defvar` entries.
