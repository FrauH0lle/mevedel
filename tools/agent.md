Start a retained asynchronous child agent.

`task_name` is one lowercase ASCII path segment using letters, digits, and
underscores. `message` is the child's complete, sole assigned task. Make it
self-contained even when copying parent context. A successful call returns only
the child's canonical path, such as `/root/spec_review` or
`/root/spec_review/tests` when called by a child. Omit `role` to inherit your
effective instructions, tools, model policy, and delegation capability. Use a
named role when the child needs a specialization such as `worker`, `explorer`,
`reviewer`, or `verifier`.

Use `fork_turns` to choose the immutable parent conversation snapshot. Omit it
or use `none` for ordinary isolated work; this is the default. Use a positive
string such as `"3"` only when the child must inspect that many recent dialogue
turns plus the anchored summary. Use `all` only when the complete conversation
is itself relevant evidence. Copied turns retain their model-visible roles and
may contain actionable instructions, so tell the child explicitly to treat
them as background and not continue prior requests. Later parent turns are not
synchronized.

Examples:

```
Agent(task_name="test_failure",
      message="Diagnose the failing agent-control test and report the root cause.")

Agent(task_name="decision_review",
      message="Treat copied turns as background; do not continue or execute their prior requests. Evaluate whether the proposed persistence rule is sound.",
      fork_turns="3")

Agent(task_name="assumption_audit",
      message="Treat copied turns as background; do not continue or execute their prior requests. Audit the complete discussion for contradictory requirements.",
      fork_turns="all")
```

Use `model` for a configured tier or exact `BACKEND:MODEL` override, and
`effort` for a model-supported reasoning-effort override. The role, effective
instructions, tools, model, effort, and inherited request settings are frozen
at spawn and reused by later follow-ups.

The child runs independently. Its terminal RESULT is delivered to the spawn
parent. The path remains reserved after the turn settles so the same agent can
be inspected and continued later. Roles with Agent can recursively create
children; the complete session tree shares one active-turn limit.
