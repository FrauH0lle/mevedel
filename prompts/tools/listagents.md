List every addressable agent retained by the current root session.

### When to use `ListAgents`

- Getting an overview of the agent tree and each agent's activity
- Gathering progress evidence before deciding to wait, follow up, or
  interrupt
- Resolving the canonical path of an agent you need to address

### When NOT to use `ListAgents`

- Polling in a tight loop while waiting for work to finish -> use
  `WaitAgent`
- Reading an agent's results; activity is state, not output -- terminal
  results arrive as RESULT deliveries

### How to use `ListAgents`

Results are sorted by canonical path and contain only `path`, `role`, and
`activity`. Activity is the exact retained state: `starting`, `running`,
`waiting`, `permission-blocked`, `interaction-blocked`, or `idle`. `/root` is
included. Use the optional canonical `path_prefix` to inspect one subtree.
Omit to list all.

### Examples of good usage

<example>
- Survey the whole tree:
ListAgents()
</example>

<example>
- Inspect one subtree:
ListAgents(path_prefix="/root/spec_review")
</example>

### Examples of bad usage

<example>
Calling ListAgents() repeatedly back-to-back until a child goes idle
<reasoning>
Busy-polling wastes turns. Use WaitAgent with a suitably long timeout;
its wake-up reports why waiting finished.
</reasoning>
</example>

<example>
ListAgents(path_prefix="spec_review/../..")
<reasoning>
path_prefix must be a canonical path such as "/root/spec_review";
traversal and opaque ids are rejected.
</reasoning>
</example>
