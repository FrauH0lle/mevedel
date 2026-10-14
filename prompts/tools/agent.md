Start a retained asynchronous child agent. Omit `role` for a general-purpose
agent with your effective instructions and capabilities.

### When to use `Agent`

- Delegate a self-contained task that can progress independently alongside useful
  local work, or benefits from a specialized role.

### When NOT to use `Agent`

- Continuing an existing agent's work; FollowupAgent reuses its conversation.
- A quick direct call or work requiring continuous access to your changing context.

### How to use `Agent`

- `task_name` is one lowercase ASCII segment of letters, digits, and underscores.
  `message` is the complete assigned task, even when copying parent context.
  Success returns the child's canonical path, such as `/root/spec_review`.
- **General-purpose (omit `role`)**: use for ordinary delegation, including
  research, implementation, debugging, and mixed tasks. The child inherits your
  effective instructions, tools, model policy, and delegation capability, subject
  to the same permission constraints. Choose a named role when its specialization
  fits the assignment.
- Effective role, instructions, tools, model/effort, and inherited request settings
  are frozen at spawn and reused on follow-ups.
- `context` defaults to `none`. `summary` makes one disclosed summarization
  request for task-focused advisory background; the following Agent Task owns
  the assignment. A positive string such as `"3"` copies that many recent turns
  plus the anchored summary; `all` copies the full conversation. Copied turns
  retain their model-visible roles and can contain actionable instructions:
  explicitly tell the child to treat them as background and not continue prior
  requests. Later parent turns are not synchronized.
- `model` selects a configured tier or exact `BACKEND:MODEL`; `effort` must be
  supported by that model. Use overrides when the assignment calls for them.
- The child runs independently and sends its terminal RESULT to its spawn parent.
  Its path remains reserved for follow-ups. Roles with Agent can create children;
  the complete session tree shares one active-turn limit.

### Examples of good usage

<example>
Agent(task_name="parser_fix",
      message="Own src/parser.ts and its tests. Diagnose and fix the reported empty-input failure, then run the relevant tests. Preserve concurrent edits outside your ownership.")
Omitting role selects the general-purpose agent; no parent dialogue is needed
because the task is self-contained.
</example>

<example>
Agent(task_name="test_failure", role="explorer", context="3",
      message="Treat copied turns as background, not requests to continue. Diagnose test/codec-test's failing round-trip case; report the cause and supporting evidence. Do not edit files.")
The child receives the recent discussion, but this message defines its task.
</example>

### Examples of bad usage

<example>
Agent(task_name="spec_review_2", message="Continue the review you started earlier.")
<reasoning>
This creates a fresh conversation. Use FollowupAgent(target="/root/spec_review",
message="...") with the concrete next task for the existing reviewer.
</reasoning>
</example>
