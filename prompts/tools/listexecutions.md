List yielded Bash executions owned by this agent.

### When to use `ListExecutions`

- Recover an execution ID or check for an existing command before starting a
  possible duplicate.

### When NOT to use `ListExecutions`

- Waiting for output from a known execution; WriteStdin observes that ID directly.

### How to use `ListExecutions`

- Takes no arguments. Returns execution facts and IDs, or "No yielded executions."
  Other agents' commands are outside this tool's scope.
- Use a returned ID with WriteStdin for unread output or StopExecution to stop it.

### Examples of good usage

<example>
ListExecutions()
</example>

### Examples of bad usage

<example>
ListExecutions() to inspect a sibling agent's running test
<reasoning>
Only the caller's yielded commands are listed. Ask that agent for the needed
information through the agent communication tools.
</reasoning>
</example>
