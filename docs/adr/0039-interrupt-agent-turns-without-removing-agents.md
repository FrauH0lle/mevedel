# Interrupt agent turns without removing agents

Status: accepted

`InterruptAgent` may target any addressable non-root agent other than the caller, aborts only that agent's current turn, and returns the previous activity status. It does not interrupt descendants, remove the agent, release its canonical path, or discard conversation context. Interrupting an idle agent is a no-op; `FollowupAgent` may start another turn later. User-facing root cancellation remains a separate session control.

## Decision history

InterruptAgent replaced StopAgent to name the effect precisely: it ends a turn
while preserving the retained conversation, descendants, and later follow-up
capability. Root-session cancellation remains a separate user control.
