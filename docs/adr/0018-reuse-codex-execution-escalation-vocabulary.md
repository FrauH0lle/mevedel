# Reuse Codex execution escalation vocabulary

Bash and batch Eval expose `sandbox_permissions` with `use_default`,
`with_additional_permissions`, and `require_escalated`, plus an
`additional_permissions` network/filesystem profile and a required
justification for non-default requests.  Additive requests retain confinement
and widen only named capabilities; `require_escalated` is the distinct full
bypass.  Mevedel does not add Codex's `prefix_rule` argument because existing
permission rules already own reusable command authorization and persistence.
Full escalation asks in every permission mode unless a user-authored rule
explicitly grants that boundary; a normal command-pattern allow does not, and
delegated rules cannot grant it.  Permission rules express this authority with
an optional `:sandbox-permissions` qualifier, which matches an already
escalated request rather than triggering one.  Bash command patterns and Eval
expression patterns may scope the grant; omitting a pattern is a legal,
deliberately broad user grant.
