# Publish agent spawns atomically

Status: accepted

`Agent` derives a unique canonical path from the free-form task name and reserves
it with one agent-turn capacity slot atomically before publishing a child.
Collisions with retained or preparing identities select a numbered sibling
rather than failing; see [ADR 0034](0034-address-agents-by-canonical-task-path.md).
Exhausted capacity or initialization failure leaves no registry entry and
releases both reservations, so the unpublished path remains reusable. The tool
returns the path only after the agent record and initial turn are committed.
Provider or tool failure after publication settles that turn as errored and
releases capacity, while the durable agent identity and path remain available
for follow-up.
