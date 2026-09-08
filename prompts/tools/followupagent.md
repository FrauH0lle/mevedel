Continue or steer an existing retained agent conversation.

### When to use `FollowupAgent`

- Assign a next task to an agent holding relevant context, or revise its running
  assignment at the next safe boundary.

### When NOT to use `FollowupAgent`

- Passing interim information without starting work; use SendMessage.
- Polling completion; ListAgents reports activity and WaitAgent waits for delivery.

### How to use `FollowupAgent`

- Pass an existing non-root target's canonical path or a relative descendant path,
  with the complete follow-up task in `message`.
- An idle target starts a turn when capacity is available. A running target
  receives the task at the next safe boundary without another slot.
- Success returns an empty result. The eventual terminal RESULT still goes to
  the target's original spawn parent.

### Examples of good usage

<example>
FollowupAgent(target="/root/spec_review",
              message="Check whether the revised codec resolves your two findings.")
</example>

### Examples of bad usage

<example>
FollowupAgent(target="/root/new_helper", message="Review the diff.") when no such agent exists
<reasoning>
FollowupAgent continues retained agents. Agent creates a new one.
</reasoning>
</example>
