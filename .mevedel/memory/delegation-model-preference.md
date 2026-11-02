---
name: "Delegation model preference"
description: "Let the coordinating model choose delegated models using the current preset and task needs, without a fixed-model fallback."
type: user
---

For multi-agent work (pair partners, reviewers, implementers and other
sub-agents), the coordinating model should choose an appropriate model and
effort for the task, using the current preset's available choices and policy
as context. There is no standing preference for a particular model or a
fixed high-effort fallback. Respect explicit model and effort settings in
the current preset and the user's request.

The user also prefers the main assistant to act as orchestrator, reviewer, and
coordinator, delegating implementation to agents when appropriate. This is a
role preference, not a requirement to delegate every task.

**Why:** on 2026-10-30, the user explicitly replaced the fixed-model
delegation preference with coordinator-selected models, for example from
the current preset. The earlier fallback was causing one model to be chosen
too consistently. The coordination role preference was stated on 2026-10-26.

**How to apply:** follow explicit user selections first, then the current
preset's model and effort policy. Where selection is discretionary, choose
for the delegated task rather than importing an old model preference from
memory. Check current model availability before choosing. Coordinate and
review delegated implementation when it is appropriate to delegate.
