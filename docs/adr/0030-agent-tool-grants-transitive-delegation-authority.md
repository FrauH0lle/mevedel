# Agent tool grants transitive delegation authority

Status: accepted

An agent with the `Agent` tool may spawn any agent role available to its session, and each child receives its own role-defined tools without intersection against the parent's direct tools. The `default`, `worker`, and `explorer` built-in roles include `Agent`, while `reviewer` and `verifier` deliberately omit it and remain leaves. Custom roles may make the same choice. Mevedel will not attempt to propagate a monotonic tool ceiling through nested delegation; session permissions, explicit denies, protected resources, and confinement remain the outer authority boundary. Consequently, a role with `Agent` may be directly read-only but is not team-read-only.
