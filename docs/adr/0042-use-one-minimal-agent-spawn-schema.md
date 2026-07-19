# Use one minimal agent spawn schema

Status: accepted

`Agent` has two required fields, `task_name` and `message`, plus optional `role`, `fork_turns`, `model`, and `effort`. The task name is both the local path segment and short display label; the message is the complete initial task. V2 removes the redundant `description`, renames `prompt` to `message` and `subagent_type` to the optional `role`, and removes `run_in_background`. No compatibility aliases or dual schema are retained.
