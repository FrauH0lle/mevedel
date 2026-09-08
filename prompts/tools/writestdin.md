Poll unread output or send input to a yielded Bash execution.

### When to use `WriteStdin`

- Wait for output from a running command, answer a PTY prompt, or send Ctrl-C.

### When NOT to use `WriteStdin`

- Starting a new command or sending ordinary input to a pipe-mode execution,
  whose stdin is closed.

### How to use `WriteStdin`

- Use the `execution_id` returned by Bash. Omit `chars` or pass an empty string
  to poll; ordinary input requires `tty=true` at launch. A single Ctrl-C
  character interrupts either mode.
- `yield_time_ms` bounds this observation, not the command's lifetime. Polls
  default to 5000ms, input sends to 250ms. Choose a wait appropriate to the
  expected output; a quiet interval is not completion or failure evidence.
- Returns new unread output and execution state. Keep the same ID for further
  observations until it settles. Only this agent's yielded executions are
  addressable. For PTY and lifecycle details, read `mevedel://tools/execution.md`.

### Examples of good usage

<example>
WriteStdin(execution_id="exec-42", yield_time_ms=30000)
</example>

### Examples of bad usage

<example>
WriteStdin(execution_id="exec-42", chars="ls -la\n") to a pipe-mode execution
<reasoning>
Pipe-mode stdin is closed. Start a new Bash call, or use an already-running
PTY session when interactive input is intended.
</reasoning>
</example>
