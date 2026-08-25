Stop one yielded Bash execution owned by this agent.

### When to use `StopExecution`

- A yielded execution is no longer needed and should not keep running
- Cleaning up a long-running command before finishing the task

### When NOT to use `StopExecution`

- You want the command's remaining output first -> poll with
  `WriteStdin`, then stop
- A gentle interrupt is enough -> send a single Ctrl-C via
  `WriteStdin`; StopExecution ends the execution outright

### How to use `StopExecution`

- `execution_id` is the opaque ID returned by the yielded Bash call or
  by `ListExecutions`
- Stopping is final for that execution; start a new `Bash` call to run
  the command again

### Examples of good usage

<example>
- The dev server is no longer needed:
StopExecution(execution_id="exec-42")
</example>

<example>
- Abandon a build made obsolete by new edits:
StopExecution(execution_id="exec-7")
</example>

### Examples of bad usage

<example>
StopExecution(execution_id="exec-42") because output has been quiet
<reasoning>
Silence is not failure evidence. Poll with WriteStdin and a longer
yield_time_ms before concluding the command hung.
</reasoning>
</example>

<example>
StopExecution(execution_id="make test")
<reasoning>
The argument is the opaque execution ID, not the command text. Get the
ID from the yielded Bash result or ListExecutions.
</reasoning>
</example>
