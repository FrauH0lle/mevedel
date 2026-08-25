Continue an existing retained agent conversation.

### When to use `FollowupAgent`

- Assigning the next task to an agent that already holds the relevant
  context
- Steering a running agent with a revised or additional task at the
  next safe boundary

### When NOT to use `FollowupAgent`

- No agent exists for the work yet -> use `Agent`
- Passing interim information that must not start or steer a turn ->
  use `SendMessage`
- Stopping a turn that is no longer wanted -> use `InterruptAgent`

### How to use `FollowupAgent`

`target` accepts a canonical path such as `/root/spec_review` or a relative
descendant path beneath the caller. `message` is the complete follow-up task.
An idle target starts another turn when capacity is available. A running target
receives the task at the next safe boundary without consuming another slot.

Success returns an empty result. Every eventual terminal RESULT still goes to
the target's original spawn parent.

### Examples of good usage

<example>
FollowupAgent(target="/root/spec_review",
              message="Now audit the persistence layer for the same invariant violations you found in the codec.")
</example>

<example>
FollowupAgent(target="tests",
              message="The fixture moved to test/fixtures/session.eld; rerun the failing case against it.")
</example>

### Examples of bad usage

<example>
FollowupAgent(target="/root/new_helper",
              message="Review the diff.")
<reasoning>
The target path does not exist. FollowupAgent only continues retained
agents; spawn new ones with Agent.
</reasoning>
</example>

<example>
FollowupAgent(target="/root/spec_review", message="Are you done yet?")
<reasoning>
Starts a pointless turn just to poll progress. Use ListAgents for
activity evidence or WaitAgent to wait for its RESULT.
</reasoning>
</example>
