Launch a specialized agent to handle complex, multi-step tasks
autonomously.

### Foreground (default)

The tool blocks until the agent finishes and returns its result
directly. Use for tasks whose output you need before continuing.

### Background (`run_in_background: true`)

The tool returns immediately with the agent's ID. The agent keeps
running; its result is delivered to your mailbox as an
`<agent-message>` when it finishes.

Use background mode when you need to:
- Run multiple agents in parallel and coordinate them
- Continue working while an agent handles a long task
- Send guidance to a running agent via `SendMessage`

Background agents that you spawned can be addressed by their full ID
in `SendMessage`.  Agents do not get a sibling-to-sibling channel;
route sibling coordination through the parent that spawned them.
