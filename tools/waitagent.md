Suspend this active turn until the caller receives mailbox activity, new user
steering, or the bounded timeout expires.

`timeout_ms` is optional, defaults to 30000, and must be an integer from 10000
through 3600000 inclusive. Timeout is a successful wake-up. The result reports
only why waiting finished; unread mail is injected separately before the next
model sample.
