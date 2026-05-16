Stop a running sub-agent owned by the current session.

Use this when an agent is clearly stranded, no longer relevant, or is
blocking progress after the runtime reports it is still running.

Pass the full agent id from the Agent launch result. A displayed short
id such as `reviewer--73512314` is accepted only when it resolves to
exactly one running agent.

Stopping a background agent marks its transcript as aborted, delivers
an `<agent-result>` block to the parent mailbox, and lets a parent that
is waiting in BWAIT continue. Stopping a foreground agent completes the
parent Agent tool call with an error result so the main session can
continue.
