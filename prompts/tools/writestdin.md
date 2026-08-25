Poll unread output or send input to a yielded Bash execution.

### When to use `WriteStdin`

- Polling unread output from a Bash call that yielded an
  `execution_id`: omit `chars` or pass an empty string
- Answering a prompt or driving a REPL in a PTY execution
  (`tty=true` at launch)
- Interrupting a running execution: a single Ctrl-C character works in
  either PTY or pipe mode

### When NOT to use `WriteStdin`

- Sending ordinary input to a pipe-mode execution; its stdin is closed
  and only Ctrl-C is accepted
- Starting new commands -> use `Bash`
- Ending an execution for good -> use `StopExecution`

### How to use `WriteStdin`

- `execution_id` is the opaque ID returned by a yielded Bash call
- `chars` is the input to send; omit it or pass "" to poll
- `yield_time_ms` bounds the wait for output: polls default to 5000ms,
  input sends default to 250ms
- Poll with a suitably long `yield_time_ms` instead of many rapid
  empty polls

### Examples of good usage

<example>
- Poll a long-running build for new output:
WriteStdin(execution_id="exec-42", yield_time_ms=30000)
</example>

<example>
- Answer an interactive prompt in a PTY execution:
WriteStdin(execution_id="exec-7", chars="yes\n")
</example>

### Examples of bad usage

<example>
WriteStdin(execution_id="exec-42", chars="ls -la\n")
to a pipe-mode execution
<reasoning>
Pipe-mode stdin is closed; ordinary input requires a PTY execution.
Run a new Bash command instead.
</reasoning>
</example>

<example>
WriteStdin(execution_id="exec-42") repeated every few seconds
<reasoning>
Rapid empty polls waste turns. Use one poll with a longer
yield_time_ms to wait for output.
</reasoning>
</example>
