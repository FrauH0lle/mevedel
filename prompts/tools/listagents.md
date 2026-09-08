List addressable agents retained by the current root session.

### When to use `ListAgents`

- Inspect the tree's activity or resolve a canonical path for another operation.

### When NOT to use `ListAgents`

- Reading completed work; terminal responses arrive as RESULT deliveries.
- Repeatedly polling for a change; WaitAgent can suspend until activity arrives.

### How to use `ListAgents`

- Results are sorted by canonical path and contain `path`, `role`, and `activity`.
  `/root` is included. `path_prefix` optionally selects a canonical subtree.
- Activity is `starting`, `running`, `waiting`, `permission-blocked`,
  `interaction-blocked`, or `idle`. It describes retained state, not the quality
  or completeness of the agent's work.

### Examples of good usage

<example>
ListAgents(path_prefix="/root/spec_review")
</example>

### Examples of bad usage

<example>
ListAgents(path_prefix="spec_review/../..")
<reasoning>
Use a canonical prefix such as `/root/spec_review`. Traversal and opaque IDs are
rejected.
</reasoning>
</example>
