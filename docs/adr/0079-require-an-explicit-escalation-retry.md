# Require an explicit escalation retry

Status: accepted

A refused or failed confined invocation returns model-visible instructions for
requesting additional or full execution authority, but never opens an
escalation prompt itself. Only a new model invocation with an explicit
capability request and justification enters permission resolution, so failure
cannot manufacture broader authority or replay a process automatically.

Failed confined Bash and batch-Eval results include this conditional recovery
hint:

> This command ran with network/path confinement. If confinement caused the
> failure, retry with `with_additional_permissions` and request only the
> required network or exact path capability. Use `require_escalated` only when
> additive permissions cannot represent the requirement.

Successful and semantic non-error outcomes omit the hint.

For example, `rm -rf /` in `full-auto` without write authority for `/` returns
a missing-authority refusal rather than opening a prompt. Only a new invocation
that explicitly requests write access to `/` may enter the combined authority
prompt.

When sandbox mode is `required` but confinement is unavailable, additive
network or path authority cannot repair the missing backend and does not
prompt. The refusal explains that only a new `require_escalated` invocation can
request direct execution.
