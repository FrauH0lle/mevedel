Suspend this active turn until the caller receives mailbox activity, new user
steering, or the bounded timeout expires.

### When to use `WaitAgent`

- Waiting for a spawned child's RESULT before your next step depends
  on it
- Pausing for expected MAIL or user steering instead of busy-polling

### When NOT to use `WaitAgent`

- No outstanding children or expected mail; the wait can only time out
- Checking progress without suspending -> use `ListAgents`
- Chaining many short waits -> prefer one suitably long wait

### How to use `WaitAgent`

A MAIL wake-up does not mean its sender finished; only RESULT delivery is
terminal.

`timeout_ms` is optional and defaults to 30000. Values outside 10000-3600000
are clamped to the nearest bound, with a corrective note in the tool result.
Timeout is a successful wake-up. Prefer one suitably long wait over repeated short waits.
Use ListAgents activity as progress evidence, and do not interrupt an agent
merely because elapsed time exceeded an estimate. The result reports only why
waiting finished; unread mail is injected separately before the next model
sample.

### Examples of good usage

<example>
- Wait up to ten minutes for review results:
WaitAgent(timeout_ms=600000)
</example>

<example>
- Nothing else to do until the workers report:
WaitAgent()
</example>

### Examples of bad usage

<example>
WaitAgent(timeout_ms=10000) repeated six times in a row
<reasoning>
Repeated short waits waste turns. One WaitAgent(timeout_ms=60000)
covers the same window.
</reasoning>
</example>

<example>
WaitAgent(timeout_ms=600000) followed by InterruptAgent on timeout
<reasoning>
A timeout is a successful wake-up, not failure evidence. Check
ListAgents activity before concluding anything about the child.
</reasoning>
</example>
