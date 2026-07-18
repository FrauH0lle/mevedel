Interrupt one retained agent's current turn without removing the agent.

Pass the target's canonical path from ListAgents, or a relative descendant
path below the caller. InterruptAgent rejects `/root`, the caller itself,
malformed paths, unknown paths, and opaque internal ids.

Interrupting an idle agent is a successful no-op. Interrupting an active
agent preserves its path, conversation, mailbox, descendants, and future
FollowupAgent capability. The target's spawn parent receives one RESULT with
outcome `interrupted`, the interruption reason, and useful partial work.

The result contains only the target's activity immediately before the call.
