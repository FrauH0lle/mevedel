# Remove the privileged coordinator

Status: accepted

Mevedel will remove the built-in coordinator agent, bundled coordinator skill, preset exposure, forced first-turn behavior, messaging alias, and coordinator-specific routing. The root agent will orchestrate ordinary agents directly, and no agent name will imply special coordination authority; a future user-defined agent named `coordinator` will behave like any other agent. This avoids preserving an unused intermediary and prevents its routing assumptions from constraining the new durable-agent model.
