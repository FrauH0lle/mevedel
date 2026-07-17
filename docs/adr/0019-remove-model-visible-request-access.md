# Remove model-visible RequestAccess

Remove the model-visible `RequestAccess` tool and its directory-specific
prompt, cache, renderer, diagnostics path, and agent assignments.  Native
filesystem calls already request missing authority through the normal
permission pipeline, while Bash and batch Eval use additive permissions; both
now settle through one resource-grant interface with invocation, session, and
persistent scopes.  Keep the manual project-root commands for deliberate broad
user configuration.  If proactive model permission requests later prove
necessary, add a generalized adapter over the shared grant interface rather
than restoring a parallel directory-access system.
