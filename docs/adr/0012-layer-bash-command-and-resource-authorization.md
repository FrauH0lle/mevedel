# Layer Bash command and resource authorization

Mevedel authorizes a Bash invocation only when both its command form and its
identified filesystem resources are authorized.  A user-authoritative command
allow from the session, workspace-persistent, or global user rule bucket may
override any conservative classifier category, including dangerous or complex;
delegated invocation and request rules may not.  No command rule can grant
access to paths outside allowed roots or exact path grants, while explicit
denies and protected paths remain authoritative.  This preserves informed user
control without letting skills elevate their own authority, broad command
patterns become implicit filesystem capabilities, or Bash bypass native
file-tool permissions.  Protected-path authorization is reinforced by
execution confinement rather than treated as an effect-safety proof: `.git`
is read-only by default, other protected paths are inaccessible by default,
and an approved invocation receives only the additive read or write capability
needed for that path.
