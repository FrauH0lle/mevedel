Start a retained asynchronous child agent.

`task_name` is one lowercase ASCII path segment using letters, digits, and
underscores. `message` is the child's complete task. A successful call returns
only the child's canonical path, such as `/root/spec_review` or
`/root/spec_review/tests` when called by a child. Omit `role` to inherit your
effective instructions, tools, model policy, and delegation capability. Use a
named role when the child needs a specialization such as `worker`, `explorer`,
`reviewer`, or `verifier`.

The child runs independently. Its terminal RESULT is delivered to the spawn
parent. The path remains reserved after the turn settles so the same agent can
be inspected and continued later. Roles with Agent can recursively create
children; the complete session tree shares one active-turn limit.
