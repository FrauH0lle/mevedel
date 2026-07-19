# Charge capacity only for new follow-up turns

Status: accepted

`FollowupAgent` reserves agent-turn capacity only when its target is idle and requires a new turn. A running target receives the follow-up at its next message boundary without consuming another slot, even when the session tree is at capacity. An idle target with no available slot causes a tool error and the follow-up is not queued; callers may use queue-only `SendMessage` instead.
