Wait for mailbox activity, new user steering, or a bounded timeout.

### When to use `WaitAgent`

- Your next step depends on an outstanding child's result or expected mail.

### When NOT to use `WaitAgent`

- Inspecting current activity without suspending; ListAgents supplies it.
- There is no expected activity and useful independent work remains.

### How to use `WaitAgent`

- `timeout_ms` defaults to 30000; values outside 10000-3600000 are clamped with
  a corrective note. Choose a wait appropriate to the work instead of busy-polling.
- The result explains why the wait ended; unread mail is injected separately
  before the next model sample. MAIL is interim; RESULT is terminal.
- Timeout is a successful wake-up, not evidence that another agent failed or
  stopped. Inspect activity or await its result before treating it as finished.

### Examples of good usage

<example>
WaitAgent(timeout_ms=600000)
</example>

### Examples of bad usage

<example>
WaitAgent(timeout_ms=600000) followed by InterruptAgent solely because it timed out
<reasoning>
A wait timeout says nothing about the child's progress. Use available activity
and results to decide whether intervention is needed.
</reasoning>
</example>
