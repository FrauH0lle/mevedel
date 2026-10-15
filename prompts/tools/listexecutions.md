List live yielded Bash and Monitor executions owned by this agent.

### When to use `ListExecutions`

- Recover an execution ID or check for an existing command before starting a
  possible duplicate.

### When NOT to use `ListExecutions`

- Waiting for output from a known execution; WriteStdin observes that ID directly.

### How to use `ListExecutions`

- Takes no arguments. Returns execution facts and IDs, or "No yielded executions."
  Only live caller-owned executions are listed; completed executions and other
  agents' commands are excluded. Monitors are root-owned.
- Use a returned ID with WriteStdin for unread output or StopExecution to stop it.

### Examples of bad usage

<example>
ListExecutions() to inspect a sibling agent's running test
<reasoning>
Only the caller's live yielded executions are listed. Ask that agent for the needed
information through the agent communication tools.
</reasoning>
</example>
