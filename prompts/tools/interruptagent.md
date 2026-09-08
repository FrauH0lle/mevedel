Interrupt a retained agent's current turn while keeping its conversation.

### When to use `InterruptAgent`

- The current assignment is obsolete, rests on a disproved assumption, or needs
  to release its active-turn slot.

### When NOT to use `InterruptAgent`

- Redirecting useful work; FollowupAgent delivers a revised task at a safe boundary.
- Stopping work merely because it exceeded an estimate; activity and results give
  evidence that elapsed time alone cannot supply.

### How to use `InterruptAgent`

- Use a canonical target path or a relative descendant path. `/root`, the caller
  itself, malformed/unknown paths, and opaque internal IDs are rejected.
- An idle target is a successful no-op. An active target keeps its path,
  conversation, mailbox, descendants, and follow-up capability. The spawn parent
  receives one RESULT with outcome `interrupted`, the reason, and partial work.
- The tool result reports only activity immediately before the call. Interruption
  does not undo completed edits or stop retained descendants.

### Examples of good usage

<example>
InterruptAgent(target="/root/spec_review") after the reviewed design is withdrawn
</example>

### Examples of bad usage

<example>
InterruptAgent(target="/root")
<reasoning>
The root session cannot be interrupted through this tool; the call is rejected.
</reasoning>
</example>
