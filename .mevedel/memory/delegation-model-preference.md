---
name: "Delegation roles and implementation-model preference"
description: "Delegate implementation appropriately; use Codex gpt-6.1-sol at high or greater effort, reserving gpt-6-astra for hard parts."
type: user
---

# Delegation roles and model selection

## Coordination role

The user prefers the main assistant to act as orchestrator, reviewer, and coordinator, delegating implementation to agents when appropriate. This role preference, stated on 2026-10-26, is not a requirement to delegate every task.

## Implementation-model preference

**User correction, 2026-11-07** (`segment-0001.chat.org`, turn 39): for delegated implementation, use Codex **gpt-6.1-sol** at effort **“high or more”**; reserve **gpt-6-astra** for **“hard” parts**.

This corrected the assistant's plan to continue implementation with a gpt-6-astra agent. Apply it to implementation work, not automatically to pair partners, reviewers, or every other delegated role. “Hard” was not given a precise threshold.

## Other delegated roles and current configuration

On 2026-10-30, the user replaced an earlier fixed-model fallback with coordinator-selected models informed by the current preset and task needs, because the fallback caused one model to be chosen too consistently. The November 7 correction supplies a more specific preference for implementation; it does not establish a fixed model for every role.

Respect explicit selections in the current user request and applicable preset constraints. Check current availability rather than assuming a remembered model is selectable. If the implementation preference cannot be met under the current configuration, surface that mismatch rather than silently claiming to have followed it.

For discretionary selections outside the implementation preference, choose model and effort for the delegated task. Coordinate and review delegated implementation when delegation is appropriate.
