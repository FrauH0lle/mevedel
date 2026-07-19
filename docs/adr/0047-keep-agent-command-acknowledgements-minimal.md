# Keep agent command acknowledgements minimal

Status: accepted

On success, `Agent` returns only the child's canonical `path`; `SendMessage` and `FollowupAgent` return an empty successful tool result. The original tool calls already retain their target and message, so V2 does not add acknowledgement IDs or duplicate role and status metadata. Invalid paths, capacity exhaustion, and delivery failures remain tool errors. `WaitAgent`, `InterruptAgent`, and `ListAgents` retain their operation-specific structured results.
