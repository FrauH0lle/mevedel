---
name: "Delegation roles and model selection"
description: "Keep design with the coordinator when appropriate; delegate implementation and choose models per task from what is configured."
type: user
modified: 2026-11-16
---

# Delegation roles and model selection

## Coordination and design role

The user prefers the main assistant to act as orchestrator, reviewer, and coordinator, delegating implementation to agents when appropriate. This role preference, stated on 2026-10-26, is not a requirement to delegate every task.

**User statement, recorded 2026-11-14** (`segment-0001.chat.org`, turn 176): “Then you can handle do the design work and hand it to gpt 6.1 workers.”

For comparable design-and-implementation work, keep substantive design ownership with the coordinator and hand implementation to suitable workers, rather than assuming design must also be delegated. This clarifies the preferred division of labor; it does not prohibit specialist design input or override an explicit request to delegate design.

## Model selection

On 2026-11-16 the user withdrew the standing preference (from 2026-11-07) to use Codex gpt-6.1-sol for delegated implementation, because the subscription behind it is not permanent. There is no fixed implementation model. Configured gptel backends can be used through an exact `BACKEND:MODEL` in Agent even when the tool's tier list shows only one backend; check what is configured rather than assuming a remembered model is available.

On 2026-10-30, the user replaced an earlier fixed-model fallback with coordinator-selected models informed by the current preset and task needs, because the fallback caused one model to be chosen too consistently.

Respect explicit selections in the current user request and applicable preset constraints; otherwise choose model and effort per delegated task. Coordinate and review delegated implementation when delegation is appropriate.
