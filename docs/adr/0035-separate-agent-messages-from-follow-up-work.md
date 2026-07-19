# Separate agent messages from follow-up work

Status: accepted

`SendMessage` enqueues information for an addressable agent but never starts an idle turn. `FollowupAgent` supplies a task and ensures the target processes it, starting another turn when the target is idle and delivering it to the current turn when the target is active. This keeps communication distinct from the decision to spend another model turn and continue the target's retained conversation.
