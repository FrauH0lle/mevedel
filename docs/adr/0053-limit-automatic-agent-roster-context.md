# Limit automatic agent roster context

Status: accepted

The first WAIT exposes only the caller's direct children as compact path and
role references. Later WAIT boundaries in that turn add newly created direct
children once rather than repeating the roster. It does not inject the complete retained agent tree. Agents use `ListAgents` for tree-wide discovery and learn other canonical paths from addressed messages and task context, keeping prompt growth bounded as session-scoped agents accumulate.
