Queue interim information for a retained agent without starting a turn.

### When to use `SendMessage`

- Share a relevant finding with your parent, a sibling, or another retained agent.

### When NOT to use `SendMessage`

- Assigning work; FollowupAgent starts or steers a turn.
- Duplicating a final verdict; the terminal response is delivered as RESULT.

### How to use `SendMessage`

- `target` accepts a canonical path, including `/root`, or a relative descendant
  path. Success returns an empty result.
- Mail arrives before the recipient's next model sample in FIFO order with other
  unread mail. Sending does not start or resume a turn; an idle recipient may
  receive it in a later turn. Final results belong in the terminal response.

### Examples of good usage

<example>
SendMessage(target="/root", message="Interim finding: the codec writes the lease twice; checking the second caller.")
</example>

### Examples of bad usage

<example>
SendMessage(target="/root/worker_1", message="Now implement the agreed codec change.") to an idle agent
<reasoning>
Mail does not start work. Use FollowupAgent for the assignment.
</reasoning>
</example>
