# Publish agent spawns atomically

Status: accepted

`Agent` reserves the canonical path and one agent-turn capacity slot atomically before publishing a child. Path collision, exhausted capacity, or initialization failure leaves no registry entry and releases both reservations, so the local task name remains reusable. The tool returns the path only after the agent record and initial turn are committed. Provider or tool failure after publication settles that turn as errored and releases capacity, while the durable agent identity and path remain available for follow-up.
