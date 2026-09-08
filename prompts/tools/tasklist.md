List tasks tracked in the current session.

### When to use `TaskList`

- Inspect progress, dependencies, or IDs needed for subsequent task operations.

### When NOT to use `TaskList`

- Reading a known task's full description or metadata; TaskGet supplies details.

### How to use `TaskList`

- Returns task IDs, status, subject, owner, and dependency links.
- `status` optionally filters `pending`, `in_progress`, or `completed` tasks.
  Omitted or empty status returns all tasks. The list records tracked work,
  not a guarantee that the user's entire objective has been captured or completed.

### Examples of good usage

<example>
TaskList(status="pending")
</example>

### Examples of bad usage

<example>
TaskList(status="done")
<reasoning>
Use `completed`; `done` is not a supported status.
</reasoning>
</example>
