---
name: "Check graphical rendering without replacing live Emacs modules"
description: "Use isolated graphical checks for visual claims; follow maintained tooling rather than the superseded fixture recipe"
type: feedback
---

# Check graphical rendering without replacing live Emacs modules

**Rule:** verify view presentation in an isolated graphical Emacs before reporting a visual result. Batch tests and CSS mockups do not establish how Emacs renders faces. Do not load worktree modules into the user's running Emacs for this purpose: that would replace modules used by other sessions.

**Why:** during turn-separator work on 2026-10-19, mockups and batch tests missed that tool rows inherited `default`'s background and that strike-through on an extended newline reached the window edge. These were observed rendering differences, not user-selected styling rules.

**How to apply:** consult the graphical-check section of `docs/development.md` for the maintained worktree-rendering workflow and its fixture extension guidance. The captured topic records that this tooling superseded the hand-built recipe on 2026-10-24 (`mevedel@982d9811`). Static captures do not establish hover behavior or frame timing; use checks appropriate to the claim.

**Historical correction, 2026-10-24:** copying `font-lock-face` to `face` was a workaround, not the preferred fixture setup. The captured correction attributed missing fontification to fixture buffer names beginning with a space; rename such fixtures rather than retaining the face-copy recipe.

Keep any live inspection bounded and non-perturbing. An earlier row-property dump returned 20 MB; a large dump is not needed to establish a visual result.
