Poll unread output from a Bash or Monitor execution, or send input to a Bash PTY.

Start new commands with Bash. Completion and Monitor events arrive automatically;
poll for evidence, not to keep root work moving. Use StopExecution for a definite
stop; its stop notice never wakes you.

- Use the `execution_id` returned by Bash, Monitor, or ListExecutions. Omit
  `chars` or pass an empty string to poll the combined unread stdout/stderr spool.
  Polling Monitor output is separate from its stdout event notifications.
- Ordinary input requires Bash with `tty=true` at launch; Monitor rejects it.
  A single Ctrl-C character requests an interrupt in either mode (unavailable
  on Windows). It is not a guaranteed stop and does not suppress a terminal wake.
- `yield_time_ms` bounds this observation, not the command's lifetime. Polls
  default to 5000ms, input sends to 250ms. Choose a wait appropriate to the
  expected output; a quiet interval is not completion or failure evidence.
- Returns new unread output and execution state. Keep the same ID for further
  observations when needed. Only this agent's yielded executions are addressable;
  monitors belong to root. For lifecycle details, read `mevedel://tools/execution.md`.
