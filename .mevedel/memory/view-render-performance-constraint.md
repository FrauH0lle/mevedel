---
name: "View changes must preserve rendering and CPU gains"
description: "Rendering cost and CPU use are hard constraints; measure graphical-only effects in a graphical environment"
type: user
---

# View changes must preserve rendering and CPU gains

**User constraint, 2026-10-13:** “We invested A LOT of work in reducing CPU recently and rendering work so this is not allowed to regress.”

Apply this as a hard constraint to mevedel view/render changes, including live tool-row grouping and animation changes. A presentation improvement is not permission to regress rendering cost or CPU use.

**Measurement lesson, not a user-specified procedure:** the same session reported that shimmer color frames render only in graphical Emacs; batch Emacs falls back to glyphs. Consequently, batch benchmark parity did not establish parity for shimmer color cost. A nested graphical Emacs was used to exercise that path.

Model inference supported by those observations: compare before/after under conditions that actually execute the changed rendering behavior. Single-run preparation-time outliers in this session were not sustained by six alternating base/change runs each; do not treat one timing outlier as established regression or improvement.
