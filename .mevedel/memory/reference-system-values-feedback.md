---
name: "Do not attribute guessed values to a reference system"
description: "Check authoritative documentation before presenting a proposed limit as inherited from another product"
type: feedback
---

# Do not attribute guessed values to a reference system

**Correction, 2026-10-09:** after the assistant proposed retaining 10 artifact versions as though that number came from Claude, the user asked, “Did you check how many versions Claude keeps?” The assistant admitted the number was its own guess.

**Rule:** before presenting a value as following a reference product, check that product's authoritative documentation. If the value is a design proposal or estimate, label it as such instead of borrowing the reference product's authority.

**Why:** an unsupported attribution makes a local design choice appear externally established.

**When to apply:** retention counts, size caps, defaults, and other quantitative claims used to justify a design by comparison with another system. Preserve the documentation's qualifications and distinguish different limits rather than treating them as interchangeable.
