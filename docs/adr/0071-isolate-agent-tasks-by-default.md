# Isolate agent tasks by default

Status: accepted

Omitted `fork_turns` starts a child with fresh instructions, role
configuration, and its complete initial task but no parent dialogue. Explicit
`all` and positive last-N values retain the immutable post-compaction snapshot
semantics from ADR 0040 and ADR 0056. Copied turns preserve their model-visible
roles and may contain actionable instructions, so callers use them only when
the child must inspect the dialogue and identify it as background in the
initial task. Isolation is the safer default because agents are spawned during
an active parent turn: copying that turn can make a child execute the parent's
orchestration request in addition to its own task.
