# Split agent observation from control tools

Status: accepted

Every built-in agent role receives the passive collaboration tools `SendMessage` and `ListAgents`. Roles with delegation authority additionally receive the control bundle `Agent`, `FollowupAgent`, `WaitAgent`, and `InterruptAgent`. Thus `default`, `worker`, and `explorer` receive the full surface, while `reviewer` and `verifier` can discover and communicate with peers but cannot create, activate, join, or interrupt agents.
