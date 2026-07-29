Suspend this active turn until the caller receives mailbox activity, new user
steering, or the bounded timeout expires.

`timeout_ms` is optional and defaults to 30000. Positive values below 10000
are clamped to 10000; values above 3600000 are rejected. Timeout is a
successful wake-up. Prefer one suitably long wait over repeated short waits.
Use ListAgents activity as progress evidence, and do not interrupt an agent
merely because elapsed time exceeded an estimate. The result reports only why
waiting finished; unread mail is injected separately before the next model
sample.
