Stop one yielded Bash execution owned by this agent.

### When to use `StopExecution`

- A running command is obsolete or no longer needed.

### When NOT to use `StopExecution`

- Inferring a hang from quiet output alone; WriteStdin can wait for new evidence.
- Sending a gentle interrupt; WriteStdin accepts a single Ctrl-C character.

### How to use `StopExecution`

- Use the opaque `execution_id` from Bash or ListExecutions. Only the caller's
  yielded executions are addressable.
- Returns the execution's final observation. Stopping ends that execution; it
  cannot be resumed and does not undo completed side effects.

### Examples of good usage

<example>
StopExecution(execution_id="exec-42") after the temporary server is no longer needed
</example>

### Examples of bad usage

<example>
StopExecution(execution_id="make test")
<reasoning>
The argument is an execution ID, not command text. Use the ID returned by Bash
or ListExecutions.
</reasoning>
</example>
