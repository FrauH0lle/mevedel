Start a retained asynchronous child agent.

`task_name` is one lowercase ASCII path segment using letters, digits, and
underscores. `message` is the child's complete task. A successful call returns
only the child's canonical path, such as `/root/spec_review`.

The child runs independently. Its terminal RESULT is delivered to the spawn
parent. The path remains reserved after the turn settles so the same agent can
be inspected and continued later.
