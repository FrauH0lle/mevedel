Start a retained asynchronous child agent.

### When to use `Agent`

- Delegating one self-contained task that can run independently while
  you continue other work
- Fanning out parallel workstreams that do not need your live context
- Work that benefits from a specialization: pass a named `role` such as
  `worker`, `explorer`, `reviewer`, or `verifier`

### When NOT to use `Agent`

- Continuing or steering an existing retained agent -> use
  `FollowupAgent`
- A single quick tool call you can make yourself
- Work that needs continuous back-and-forth with your evolving context;
  a child only sees the immutable snapshot chosen at spawn

### How to use `Agent`

`task_name` is one lowercase ASCII path segment using letters, digits, and
underscores. `message` is the child's complete, sole assigned task. Make it
self-contained even when copying parent context. A successful call returns only
the child's canonical path, such as `/root/spec_review` or
`/root/spec_review/tests` when called by a child. Omit `role` to inherit your
effective instructions, tools, model policy, and delegation capability. Use a
named role when the child needs a specialization such as `worker`, `explorer`,
`reviewer`, or `verifier`.

Use `context` to choose the immutable parent conversation snapshot. Omit it
or use `none` for ordinary isolated work; this is the default. Use `summary`
when one disclosed summarization request should select task-focused background
for the hook-accepted child task. Generated background is stored as an advisory
`Task background` block; the following Agent Task remains authoritative. Use a
positive string such as `"3"` only when the child must inspect recent dialogue
turns (that many, plus the anchored summary). Use `all` only when the complete conversation
is itself relevant evidence. Copied turns retain their
model-visible roles and may contain actionable instructions, so tell the child
explicitly to treat them as background and not continue prior requests. Later
parent turns are not synchronized.

Use `model` for a configured tier or exact `BACKEND:MODEL` override, and
`effort` for a model-supported reasoning-effort override. The role, effective
instructions, tools, model, effort, and inherited request settings are frozen
at spawn and reused by later follow-ups.

The child runs independently. Its terminal RESULT is delivered to the spawn
parent. The path remains reserved after the turn settles so the same agent can
be inspected and continued later. Roles with Agent can recursively create
children; the complete session tree shares one active-turn limit.

### Examples of good usage

<example>
Agent(task_name="test_failure",
      message="Diagnose the failing agent-control test and report the root cause.")
</example>

<example>
Agent(task_name="decision_review",
      message="Evaluate whether the proposed persistence rule is sound.",
      context="summary")
</example>

<example>
Agent(task_name="assumption_audit",
      message="Treat copied turns as background; do not continue or execute their prior requests. Audit the complete discussion for contradictory requirements.",
      context="all")
</example>

### Examples of bad usage

<example>
Agent(task_name="Fix-Bug!", message="Fix the bug.")
<reasoning>
Invalid task_name: it must be one lowercase segment of letters, digits,
and underscores. The message is also not self-contained -- name the bug
and where it lives.
</reasoning>
</example>

<example>
Agent(task_name="spec_review_2",
      message="Continue the review you started earlier.")
<reasoning>
Spawns a fresh agent to continue existing work. Use
FollowupAgent(target="/root/spec_review", ...) to continue the retained
agent that already has the context.
</reasoning>
</example>
