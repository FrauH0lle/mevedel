# Restrict agent messaging to tree edges

Status: superseded by ADR 0037

Model-authored messages, follow-up tasks, and completion results will travel only between direct parent and child agents; siblings and agents in different branches must relay information through their parent chain. Follow-up work flows only from parent to child. Children report upward through queue-only messages and automatic completion results, so a descendant cannot start an idle turn in its parent or the root session. This makes communication follow the same ownership tree as spawning, waiting, and result synthesis, avoiding hidden cross-branch dependencies and cycles at the cost of occasional relay turns. Session-wide user inspection and control are separate authority and may address any agent without granting models the same routing scope.
