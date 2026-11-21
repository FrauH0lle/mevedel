---
name: "Preserve agreed scope, including approved prototype visuals"
description: "Do not silently drop agreed work or an approved prototype's visual treatment when writing specifications or implementing them."
type: feedback
modified: 2026-11-15
---

# Preserve agreed scope, including approved prototype visuals

**Rule:** when converting an agreed design into a specification, PRD, implementation tickets, or code, preserve the agreed scope. Do not silently drop an item, relabel it as out of scope, demote a prerequisite to optional follow-up, or narrow an approved prototype's presentation.

## Preserve agreed verification work

**User correction, recorded 2026-11-11** (`segment-0001.chat.org`, turn 51): the assistant had agreed to include a TRAMP store round-trip measurement as a ticket, but quietly moved it to “Out of scope” while writing the specification. The user identified the omission, and the assistant restored the measurement to the ticket plan.

## Approved prototype visuals count as scope

**User correction, recorded 2026-11-13** (`segment-0003.chat.org`, turns 245–246): the user asked why notes were not colored and explained, “The demo we had in scratch used colors. I thought we would take them over?” The assistant had incorrectly described uncolored notes as “by design” from the selected label look. That demo's label look included per-author colors; reducing it to a grey label narrowed the selection without asking.

When carrying an approved prototype into a specification or implementation, reconcile its relevant visual treatment as well as its named controls and behavior. A shorthand label for a selected look is not permission to discard demonstrated styling.

**Why:** formalizing or implementing a design must not quietly change what the user agreed to build or verify. A polished specification is not evidence that its scope still matches the preceding agreement.

**When to apply:** reconcile specifications, tickets, and implementations against the agreed items and selected prototype before presenting a handoff or claiming completion. If new evidence warrants a scope change, surface the proposed change and its reason rather than treating the rewrite itself as approval.

This does not freeze every exploratory suggestion or every alternative shown in a demo into a requirement. In particular, the November 13 correction did not confirm that the demo's separate ambient-underline look was also wanted. Explicit scope revisions remain possible. Keep actual work items, dependencies, and current decisions in the tracker or maintained specification, not in memory.
