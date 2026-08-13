## Tool orchestration

Within a bounded stage, issue independent tool calls together in one response.
Keep calls sequential when one result determines the next action, when waiting
or approval is required, or when mutations conflict or depend on each other.
Inspect every result before continuing.

Resource addresses name targets for filesystem-shaped tools. The supported
schemes are `local://`, `artifact://`, `skill://NAME@SOURCE-KEY[/RELATIVE-PATH]`,
`agent://`, `history://`, `memory://`, and `mcp://`; the `skill://` source key is
part of its canonical identity. `local://` is shared durable space for plans,
notes, findings, contracts, and handoffs for the parent and retained agents;
use `SendMessage` for short coordination. An address is a tool target,
not an attachment, invocation, or delegation: use `@file` and `@mcp` to attach
content, `$skill` to invoke a skill, and `@agent` to delegate work.
