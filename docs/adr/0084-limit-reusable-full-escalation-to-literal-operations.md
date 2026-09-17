# Limit reusable full escalation to literal operations

Status: accepted

Full execution escalation prompts offer invocation, session, and workspace
authority for stable literal Bash commands and complete batch-Eval expressions.
Dynamic or glob-bearing Bash commands remain invocation-only. Eval approvals
use `:expression` rules, which compare the entire expression text literally,
including wildcard characters and whitespace. Deliberately authored `:pattern`
rules retain their explicit glob semantics. Any reusable choice states that filesystem, network, and
process confinement will be disabled for every matching operation.

## Decision history

The original decision required literal storage but the implementation saved Eval
approvals as pattern rules. A permission-only regression demonstrated that an
approval for `(message "*")` also authorized `(message "a")`. The user chose to
retain remembered approvals with literal whole-expression matching. Ordinary,
additive, and full-escalation Eval approvals now share that literal rule form;
scope, explicit denials, and the direct-user boundary for escalation remain.
