---
name: "Preserve agreed scope when formalizing a design"
description: "Do not silently drop or demote agreed work while translating a design into a specification or tickets."
type: feedback
---

# Preserve agreed scope when formalizing a design

**User correction, recorded 2026-11-11** (`segment-0001.chat.org`, turn 51): the assistant had agreed to include a TRAMP store round-trip measurement as a ticket, but quietly moved it to “Out of scope” while writing the specification. The user identified the omission, and the assistant restored the measurement to the ticket plan.

**Rule:** when converting an agreed design into a specification, PRD, or implementation tickets, preserve the agreed scope. Do not silently drop an item, relabel it as out of scope, or demote a prerequisite to optional follow-up.

**Why:** formalizing a design must not quietly change what the user agreed to build or verify. A polished specification is not evidence that its scope still matches the preceding agreement.

**When to apply:** reconcile the resulting specification and tickets against the agreed items before presenting the handoff. If new evidence warrants a scope change, surface the proposed change and its reason rather than treating the rewrite itself as approval.

This does not freeze every exploratory suggestion into a requirement or prevent an explicit scope revision. Keep the actual work items, dependencies, and current decisions in the tracker or maintained specification, not in memory.
