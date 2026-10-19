---
name: "Narrow-phone collaboration viewer testing"
description: "The user tests in iPhone 13 mini Safari; vertical space and real narrow-viewport renders matter"
type: user
---

Recorded 2026-10-15: Roland reviews collaboration viewer/editor layouts
visually and tests on an iPhone 13 mini in Safari, with an approximately
375 by 629 CSS-pixel viewport. Vertical space is a binding constraint.
Earlier phone testing identified composer-row overflow and iOS text inflation
as failure modes.

**How to apply:** Include real rendered checks around 375 by 629 and 390-pixel
phone widths, as well as a desktop width, when changing the collaboration UI.
Treat those dimensions as useful test targets, not a guarantee of the current
device viewport. Inspect vertical fit, composer overflow, and text sizing;
do not rely only on desktop screenshots or DOM assertions.

The user requested mockups and alignment before implementation in the
October 15 design task. Preserve an explicitly requested approval phase,
but do not turn that task's request into a standing mock-first requirement
for every UI change. Review-finding completion is already covered by
`fix-review-findings-without-deferral.md`.

**Source:** Claude project memories `ui-mock-first-then-fix-everything.md` and
`collab-relay-deployment.md` under
`/home/roland/.claude/projects/-home-roland-Projekte-mevedel/memory/`;
curated for import at the user's request on 2026-10-19.
