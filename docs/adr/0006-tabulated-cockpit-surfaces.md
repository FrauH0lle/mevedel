# Tabulated Cockpit Surfaces Share Table Plumbing

Plugins, skills, tools, and the worktree list are selectable session resources,
so their cockpit surfaces should use the same tabulated interaction pattern: a
session-owned buffer, stable row selection, refresh, row details, and
back-to-cockpit navigation. The shared code should own only generic table
plumbing and live session ownership; each resource module keeps its own rows,
actions, state changes, and details.

This avoids three hand-built special-mode listings without turning the resource
surfaces into a generic framework that understands plugin, skill, or tool
semantics. The existing plugin cockpit should migrate onto the shared shell
without changing its rows or behavior, so the helper is proven by the original
table surface rather than only new ones.
