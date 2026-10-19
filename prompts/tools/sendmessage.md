Send a message to a retained agent, starting its turn if idle.

### When to use `SendMessage`

- Share a relevant finding with your parent, a sibling, or another retained agent.
- Continue an existing agent's work or revise its assignment.

### When NOT to use `SendMessage`

- Duplicating a final verdict; the terminal response is delivered as RESULT.

### How to use `SendMessage`

- `target` accepts a canonical path, including `/root`, or a relative descendant
  path. Success returns an empty result, or a short notice when the target must
  wait for an active-turn slot.
- An active recipient reads mail before its next model sample in FIFO order.
  Delivery does not interrupt an in-flight response, tool call, or prompt.
  An idle recipient starts a turn; mail to `/root` uses the Background event
  scheduler. Self-mail does not start a turn.
- The eventual terminal RESULT still goes to the recipient's original spawn
  parent. Do not send a duplicate final message.

### Examples of good usage

<example>
SendMessage(target="/root", message="Interim finding: the codec writes the lease twice; checking the second caller.")
</example>

<example>
SendMessage(target="/root/worker_1", message="Check whether the revised codec resolves your two findings.")
</example>
