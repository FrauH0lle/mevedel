# Remove the privileged coordinator

Status: accepted

## Current decision

The root orchestrates ordinary agents directly. No role name implies privileged
coordination authority; a user-defined `coordinator` follows the same role and
tool rules as any other agent.

## Decision history

The built-in coordinator, bundled coordinator skill, preset exposure, forced
first-turn behavior, messaging alias, and special routing were removed. The
intermediary was unused, and its routing assumptions constrained retained-agent
ownership. Ordinary delegation replaced it without keeping name-based exceptions.
