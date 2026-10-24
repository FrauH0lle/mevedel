# Wait for agents explicitly

Status: accepted. Incorporates ADR 0044.

## Current decision

WaitAgent suspends its ordinary asynchronous callback until queued or arriving
mail, user input, or timeout releases it. It
names no target, polls no provider, and spends no extra model turn while waiting.
Its turn stays active and holds capacity. The result describes the wake-up;
mail contents arrive through the separate conversation delivery path.

Release the transient wait registration and its timer before invoking its
callback. A callback can synchronously wait again, or fail after a transport
reconnect; neither may retain the old registration or clear a successor.
Callback failure does not consume mail. Durable mailbox delivery, not retention
of the callback, protects unread messages, and a later wait sees them immediately.

The default timeout is 30,000 milliseconds. Numeric values are normalized within
10,000–3,600,000 milliseconds; unrepairable values are rejected. A timeout is a
successful outcome, after which the caller may work or wait again. A MAIL wake-up
does not prove sender completion; RESULT identifies terminal settlement.

## Rationale and consequences

Explicit waiting lets the caller choose whether it needs another result. A
caller may settle while descendants continue. Delegators receive WaitAgent with
their control bundle; communicating leaves do not. A bounded successful timeout
prevents a missed notification from parking a turn indefinitely without claiming
the delegated task failed.

## Decision history

**ADR 0036 replaced implicit BWAIT parking** and its injected FSM state,
terminal interception, and watchdog machinery with the ordinary async tool path.
**ADR 0044 added bounded successful timeouts** to that same waiting decision.
The original phrase “completion from any agent” means a result actually addressed
to this recipient; WaitAgent does not subscribe to every tree event or broadcast
another parent's child results.

The August 2026 durable-settlement tests retained failed waiter callbacks so
notification failure could be retried. Reconnect failures showed that a dead
callback instead prevented every later wait with "Agent is already waiting".
Retiring callbacks before delivery preserves the durable unread mail while
allowing a fresh callback to observe it, and also fixes synchronous reentry.
