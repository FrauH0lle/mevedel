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
- A worker spawned directly by main needs to surface an intermediate
  finding or blocker before terminating.
- A coordinator-owned worker needs to send a partial finding,
  blocker, or clarification request back to that coordinator.

### Arguments

- `to` — **required** string. Recipient identifier:
  - `"main"` / `"chat"` — deliver to the top-level session, but only
    from main itself, a coordinator, or an agent spawned directly by
    main.  Workers spawned by a coordinator must route through their
    coordinator instead.
  - A full agent id (e.g. `"explore--abc123"`) — exact match against
    one of your own spawned children.  A coordinator-owned worker may
    also use its parent coordinator's exact id.
  - `"coordinator"` — only for a worker spawned by a coordinator;
    resolves to that immediate parent coordinator.  Prefer the exact
    coordinator id when it is available.
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
SendMessage(to="explore--abc123", message="Focus on the parser module only.")
-> "Message delivered to explore--abc123."
</example>

<example>
SendMessage(to="main", message="Found a blocker in module A. Pausing.")
-> "Message delivered to main."
</example>

<example>
SendMessage(to="coordinator--abc123", message="Parser module needs a separate worker.")
-> "Message delivered to coordinator--abc123."
</example>
