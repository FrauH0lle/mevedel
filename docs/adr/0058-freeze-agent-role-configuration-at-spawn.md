# Freeze agent role configuration at spawn

Status: accepted

Spawning resolves and snapshots the agent's role, instructions, tools, base model, reasoning effort, and inherited configuration. That snapshot remains stable across follow-up turns and session resume; later role-definition or default changes affect only newly spawned agents. Session-wide permissions, explicit denies, protected resources, and confinement remain dynamic outer controls and may tighten without changing the retained conversation identity.

For inherited default roles, tools include both native request tools and the
effective specialist catalog. Specialist membership is recorded as explicit
category/name tool specs in the frozen role, using the existing configuration
codec. Recursive children inherit their immediate caller's effective catalog;
named roles retain their own specs. Frozen identities do not freeze tool handlers
or bypass current availability and permission checks.

## Decision history

- **Complete tool inheritance, 2026-10-14:** a default Claude Code child inherited
  seven native tools but none of its parent's 46 specialists, so it could not
  discover SendMessage or any other specialist. Only native request locals had
  been inherited; the default role's empty tool specs seeded an empty catalog.
  Default effective specs now include the caller's specialist identities, filtered
  by its request's callable roster. The existing freeze and restore path retains
  them without a separate catalog format or a communication-tool exception.
