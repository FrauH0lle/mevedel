Suspend this active turn until the caller receives mailbox activity, new user
steering, or the bounded timeout expires. A MAIL wake-up does not mean its
sender finished; only RESULT delivery is terminal.

`timeout_ms` is optional and defaults to 30000. Values outside 10000-3600000
are clamped to the nearest bound, with a corrective note in the tool result.
Timeout is a successful wake-up. Prefer one suitably long wait over repeated short waits.
Use ListAgents activity as progress evidence, and do not interrupt an agent
merely because elapsed time exceeded an estimate. The result reports only why
waiting finished; unread mail is injected separately before the next model
sample.
