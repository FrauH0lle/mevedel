List yielded Bash executions owned by this agent.

### When to use `ListExecutions`

- Recalling which executions are still running and their
  `execution_id`s
- Checking for a forgotten execution before starting a duplicate
  command

### When NOT to use `ListExecutions`

- You already hold the `execution_id` you need -> poll it with
  `WriteStdin`
- Nothing has yielded this session; there is nothing to list

### How to use `ListExecutions`

- Takes no arguments and only reports executions owned by the calling
  agent
- Use the returned IDs with `WriteStdin` or `StopExecution`

### Examples of good usage

<example>
- Find the ID of the test run started earlier:
ListExecutions()
</example>

<example>
- Verify nothing is still running before ending the turn:
ListExecutions()
</example>

### Examples of bad usage

<example>
ListExecutions() called between every WriteStdin poll
<reasoning>
The execution_id does not change; polling the execution itself is
enough.
</reasoning>
</example>

<example>
ListExecutions() to look for another agent's execution
<reasoning>
Only executions owned by the calling agent are listed; other agents'
executions are not addressable here.
</reasoning>
</example>
