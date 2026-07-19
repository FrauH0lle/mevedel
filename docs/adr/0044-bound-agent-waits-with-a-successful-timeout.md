# Bound agent waits with a successful timeout

Status: accepted

`WaitAgent` accepts an optional `timeout_ms`, defaulting to 30,000 milliseconds and constrained to 10,000 through 3,600,000 milliseconds. Arrival of mailbox activity or new user input still releases the wait immediately. Reaching the deadline returns a successful timeout summary rather than a tool error, allowing the caller to continue other work or wait again while preventing a missed notification from suspending a turn forever.
