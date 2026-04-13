Send an asynchronous message to another running agent or back to the
main chat.

The message is queued on the recipient's inbox and delivered as a
user-role turn on their next FSM cycle, wrapped in an
`<agent-message from="...">` block. Delivery is fire-and-forget —
`SendMessage` returns immediately; the recipient processes the
message whenever it next reaches a WAIT state.

### When to use `SendMessage`

- A coordinator wants to course-correct a running worker
  ("switch to the adapter pattern instead").
- A worker needs to surface an intermediate finding or question
  back to the main chat without terminating.
- Two parallel workers need to exchange context while they both run.

### Arguments

- `to` — **required** string. Recipient identifier:
  - `"main"` / `"chat"` / `"coordinator"` — deliver to the main chat
    session that owns this run.
  - An agent type (e.g. `"explore"`) — delivers to the first live
    invocation of that type.
  - A full agent id (e.g. `"explore--abc123"`) — exact match.
- `message` — **required** string. The body to deliver. Plain text;
  the sender name is wrapped automatically.

### Delivery semantics

- Asynchronous: the recipient sees the message on its next turn,
  not immediately.
- Exactly-once: each queued message is delivered once and then
  cleared from the mailbox.
- Sender is auto-stamped. Agents appear under their agent name;
  the main chat appears as `"main"`.

### Examples

<example>
SendMessage(to="explore", message="Focus on the parser module only.")
-> "Message delivered to explore."
</example>

<example>
SendMessage(to="main", message="Found a blocker in module A. Pausing.")
-> "Message delivered to main."
</example>
