Queue interim information for a retained agent. Agent mail can wake an idle root.

### When to use `SendMessage`

- Share a relevant finding with your parent, a sibling, or another retained agent.

### When NOT to use `SendMessage`

- Assigning work to a retained child; FollowupAgent starts or steers its turn.
- Duplicating a final verdict; the terminal response is delivered as RESULT.

### How to use `SendMessage`

- `target` accepts a canonical path, including `/root`, or a relative descendant
  path. Success returns an empty result.
- Mail arrives before the recipient's next model sample in FIFO order with other
  unread mail. Mail from an agent to `/root` can start an idle Background event
  turn, subject to the same holds as Monitor. Root self-mail and mail to non-root
  recipients do not start turns; idle children still need FollowupAgent.
  Final results belong in the terminal response and can also wake an idle root
  spawn parent; do not send a duplicate final message.

### Examples of good usage

<example>
SendMessage(target="/root", message="Interim finding: the codec writes the lease twice; checking the second caller.")
</example>

### Examples of bad usage

<example>
SendMessage(target="/root/worker_1", message="Now implement the agreed codec change.") to an idle agent
<reasoning>
Mail does not start an idle child's turn. Use FollowupAgent for the assignment.
</reasoning>
</example>
