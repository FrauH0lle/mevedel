# Recover abandoned agent turns as interrupted

Status: accepted

When session resume finds a persisted non-root turn in `starting`, `running`, or an interaction or agent wait without a surviving request, recovery settles that turn as interrupted. It releases capacity, preserves the agent identity, conversation, compaction state, and unread mailbox, enqueues one interruption `RESULT` to the spawn parent, and leaves the agent idle for a later follow-up. Mevedel never retries an abandoned turn automatically because its tools may already have produced side effects.
