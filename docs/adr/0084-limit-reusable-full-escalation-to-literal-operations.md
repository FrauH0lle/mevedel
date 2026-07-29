# Limit reusable full escalation to literal operations

Status: accepted

Full execution escalation prompts offer invocation, session, and workspace
authority only when the Bash command or batch-Eval expression can be stored
literally without wildcard or dynamic ambiguity. Otherwise escalation remains
invocation-only. Any reusable choice states that filesystem, network, and
process confinement will be disabled for every matching operation.
