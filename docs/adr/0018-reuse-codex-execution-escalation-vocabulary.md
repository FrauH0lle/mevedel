# Reuse Codex execution escalation vocabulary

Bash and batch Eval expose `sandbox_permissions` with `use_default`,
`with_additional_permissions`, and `require_escalated`, plus an
`additional_permissions` network/filesystem profile and a required
justification for non-default requests.  Additive requests retain confinement
and widen only named capabilities; `require_escalated` is the distinct full
bypass.  Mevedel does not add Codex's `prefix_rule` argument because existing
permission rules already own reusable command authorization and persistence.
Full escalation requires approval in Ask and Edits unless direct authority
already grants that boundary; the optional reviewer may approve one invocation. A normal command-pattern allow and
delegated rules cannot grant it. Full Access already authorizes direct execution
and never asks for escalation.  Permission rules express this authority with
an optional `:sandbox-permissions` qualifier, which matches an already
escalated request rather than triggering one.  Bash command patterns and Eval
expression patterns may scope the grant; omitting a pattern is a legal,
deliberately broad user grant.

## Decision history

Originally full escalation prompted in every mode. The September 2026 audit
found it responsible for most Full-auto cards, including recurring package-runner
requests after an unusable exact-directory cache grant. The user selected Full
Access semantics; explicit escalation remains a separate exception in Ask/Edits.
