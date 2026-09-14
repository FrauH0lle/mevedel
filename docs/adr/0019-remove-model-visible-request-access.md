# Remove model-visible RequestAccess

There is no model-visible RequestAccess tool or separate directory-access
permission system.  Native
filesystem calls already request missing authority through the normal
permission pipeline, while Bash and batch Eval use additive permissions; both
settle through one resource-grant interface with invocation, session, and
persistent scopes.  Manual project-root commands remain for deliberate broad
user configuration.

## Decision history

RequestAccess and its directory-specific prompt, cache, renderer, diagnostics,
and agent assignments were removed in favor of shared resource grants.

The September 2026 session audit showed that ordinary exact-path prompts were
not a sufficient directory workflow: one external design folder needed two
search approvals and thirteen file-read approvals. The existing permission
card now offers exact-resource or containing-directory-tree selection through
`g`, with explicit read/write access and an independent invocation, session,
or workspace lifetime. Native tools and additive execution use the shared
grant store; remembered tree approval rechecks covered queued requests across
the agent tree while preserving hook asks and other policy. This replaces the
removed directory workflow without restoring a model-visible RequestAccess
tool or a second permission system.
