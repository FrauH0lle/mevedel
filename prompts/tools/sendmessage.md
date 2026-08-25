Queue one interim plain-text message for any retained agent in this root
session tree.

### When to use `SendMessage`

- Passing new information a running agent should see before its next
  model sample, without starting or steering a turn
- Notifying your parent or a sibling of an interim finding they should
  factor in

### When NOT to use `SendMessage`

- The target must start or steer work -> use `FollowupAgent`
- Reporting your own final verdict -> put it in your terminal response
  so it is delivered as RESULT
- Resending an agent's completed response when its canonical RESULT
  delivery already carries that response

### How to use `SendMessage`

`target` accepts a canonical path such as `/root` or `/root/spec_review`, or a
relative descendant path beneath the caller. Sending never starts or resumes a
turn. The recipient receives the message before its next model sample, in FIFO
order with any other unread mail.

Success returns an empty result. MAIL is interim and may arrive in a later
root turn; agents should put their final verdict in their terminal response so
it is delivered as RESULT.

### Examples of good usage

<example>
SendMessage(target="/root/worker_1",
            message="The codec moved to mevedel-session-codec.el; adjust your search paths.")
</example>

<example>
SendMessage(target="/root",
            message="Interim finding: the lease file is written twice per save; continuing the audit.")
</example>

### Examples of bad usage

<example>
SendMessage(target="/root/worker_1",
            message="Also refactor the persistence layer when done.")
<reasoning>
This assigns work. Mail never starts or steers a turn; use
FollowupAgent for tasks.
</reasoning>
</example>

<example>
SendMessage(target="/root", message="Final verdict: the rule is sound.")
as the last act of a turn
<reasoning>
Final verdicts belong in the terminal response so they arrive as
RESULT; duplicate mail may land in a later root turn.
</reasoning>
</example>
