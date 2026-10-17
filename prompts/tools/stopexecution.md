Stop one yielded Bash or Monitor execution owned by this agent.

### When to use `StopExecution`

- A running command is obsolete or no longer needed.

### When NOT to use `StopExecution`

- Inferring a hang from quiet output alone; WriteStdin can wait for new evidence.
- Requesting only an interrupt; WriteStdin accepts a single Ctrl-C character,
  but that is not a guaranteed stop, and its terminal notice can wake you.

### How to use `StopExecution`

- Use the opaque `execution_id` from Bash, Monitor, or ListExecutions. Only the
  caller's yielded executions are addressable; monitors belong to root.
- Returns the execution's final observation. Stopping ends that execution; it
  cannot be resumed and does not undo completed side effects.
- The result already holds the final state. A short stop notice (for example
  `stopped · 2 events`) still reaches you with your next request, but it never
  wakes an idle session and needs no action. Already delivered events are not
  retracted.

### Examples of good usage

<example>
StopExecution(execution_id="exec-42") after the temporary server is no longer needed
</example>

### Examples of bad usage

<example>
StopExecution(execution_id="make test")
<reasoning>
The argument is an execution ID, not command text. Use the ID returned by Bash,
Monitor, or ListExecutions.
</reasoning>
</example>
