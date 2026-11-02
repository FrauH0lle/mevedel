---
name: "Scale verification to consequence"
description: "Rigor rules in other topics apply to consequential or durable claims; temporary or low-stakes work needs proportionate checks"
type: feedback
---

Size verification, audits and safeguards to what a wrong answer would cost.
The other verification topics (production-path, injected-failure, controlled
attribution, consumer-visible state) apply to consequential or durable claims:
user data, releases, safety boundaries. For temporary notes, exploratory
measurements and internal tooling, a focused check is enough; skip independent
checkers, hash-bound reports and repeated re-audits.

**Why:** on 2026-10-29 the shared-note effort spent many hours on audit
machinery for deleting temporary text files; the user called it complexity
creep and had `CODING_STANDARDS.md` "PROPORTIONATE SOLUTIONS" added (8215aec8).

**How to apply:** before adding a verification step, state what failure it
prevents and what that failure would cost. If the cost is low, use the cheapest
check that would notice it.
