# Centralize agent permissions in the root session

Status: accepted

All agents share the root session's dynamic permission mode, rules, explicit denies, protected resources, and confinement policy. Permission requests from any agent enter the root session's existing UI queue and identify the requester by canonical agent path. The requesting turn remains active and consumes capacity while awaiting a decision; interrupting it removes only that turn's unresolved requests. Session-level allow and deny decisions apply immediately across the tree. Mevedel does not create per-agent permission stores or child-buffer approval prompts.
