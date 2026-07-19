# Freeze agent role configuration at spawn

Status: accepted

Spawning resolves and snapshots the agent's role, instructions, tools, base model, reasoning effort, and inherited configuration. That snapshot remains stable across follow-up turns and session resume; later role-definition or default changes affect only newly spawned agents. Session-wide permissions, explicit denies, protected resources, and confinement remain dynamic outer controls and may tighten without changing the retained conversation identity.
