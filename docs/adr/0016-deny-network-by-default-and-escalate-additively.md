# Deny network by default and escalate additively

The default confined Bash and batch Eval profile denies outbound network
access.  Mevedel returns the original failure and confinement facts to the
model, whose system guidance directs it to make a new, justified tool call when
network access is still required.  Approval grants network only for that
invocation while retaining filesystem and process confinement; it does not
imply full unsandboxed execution.  This avoids speculative network-intent
classification and automatic replay while limiting exfiltration by default.
Edits requires real confinement and refuses an unavailable backend. Ask's
configured `best-effort` may select disclosed direct execution on an unavailable
initial probe. Network additions require authority in Ask and Edits, potentially
through the optional invocation reviewer. Full Access disables network and
filesystem confinement and bypasses those prompts.

## Decision history

The initial decision retained protected-resource prompts in every mode. Repeated
approval-only session friction and the user's explicit Full Access requirement
changed that scope in September 2026; default isolation still governs Edits.
