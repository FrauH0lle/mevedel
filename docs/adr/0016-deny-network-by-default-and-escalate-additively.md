# Deny network by default and escalate additively

The default confined Bash and batch Eval profile denies outbound network
access.  Mevedel returns the original failure and confinement facts to the
model, whose system guidance directs it to make a new, justified tool call when
network access is still required.  Approval grants network only for that
invocation while retaining filesystem and process confinement; it does not
imply full unsandboxed execution.  This avoids speculative network-intent
classification and automatic replay while limiting exfiltration by default.
When optional confinement is unavailable and sandbox mode `auto` falls back to
direct execution, Mevedel cannot enforce this boundary and must disclose that
network is unrestricted.  Network-only escalation asks in `ask` and `auto`
unless pre-authorized, and is automatic in `full-auto`; protected-path access
continues to require confirmation in every mode.
