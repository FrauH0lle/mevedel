Interrupt one retained agent's current turn without removing the agent.

### When to use `InterruptAgent`

- The target's current task became obsolete or is based on wrong
  assumptions, and waiting for it to finish would waste work
- Reclaiming an active-turn slot for more urgent work while keeping the
  agent and its conversation for later follow-ups

### When NOT to use `InterruptAgent`

- The agent is merely slow: elapsed time exceeding an estimate is not
  evidence of a problem -> check `ListAgents` activity or `WaitAgent`
- Redirecting a running agent -> use `FollowupAgent`; it delivers at
  the next safe boundary without discarding the turn
- Targeting `/root` or yourself; both are rejected

### How to use `InterruptAgent`

Pass the target's canonical path from ListAgents, or a relative descendant
path below the caller. InterruptAgent rejects `/root`, the caller itself,
malformed paths, unknown paths, and opaque internal ids.

Interrupting an idle agent is a successful no-op. Interrupting an active
agent preserves its path, conversation, mailbox, descendants, and future
FollowupAgent capability. The target's spawn parent receives one RESULT with
outcome `interrupted`, the interruption reason, and useful partial work.

The result contains only the target's activity immediately before the call.

### Examples of good usage

<example>
- The requirements changed and the running review is now moot:
InterruptAgent(target="/root/spec_review")
</example>

<example>
- Stop a child working from an assumption you just disproved:
InterruptAgent(target="worker_1")
</example>

### Examples of bad usage

<example>
InterruptAgent(target="/root/worker_1")
because it has been running for a while
<reasoning>
Elapsed time alone is not a reason. Use ListAgents activity as
progress evidence, or WaitAgent with a suitably long timeout.
</reasoning>
</example>

<example>
InterruptAgent(target="/root")
<reasoning>
The root session cannot be interrupted; the call is rejected.
</reasoning>
</example>
