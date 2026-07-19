# Limit automatic agent roster context

Status: accepted

Before each model sample, mevedel automatically exposes only the caller's direct children as compact path and role references. It does not inject the complete retained agent tree. Agents use `ListAgents` for tree-wide discovery and learn other canonical paths from addressed messages and task context, keeping prompt growth bounded as session-scoped agents accumulate.
