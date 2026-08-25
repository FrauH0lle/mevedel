List the tasks currently tracked in the session.

Returns every task with its ID, status, subject, owner, and any
dependency links. Pass an optional `status` filter to narrow the
list.

### When to use `TaskList`

- You need an overview of current progress
- You want to see what is pending or blocked
- You need to pick the next task to work on

### When NOT to use `TaskList`

- You need the full details of one known task -> use `TaskGet`
- Polling for changes after every tool call; list when you actually
  need to pick or report work

### How to use `TaskList`

- `status` — optional filter: `"pending"`, `"in_progress"`, or
  `"completed"`. Omit to see all tasks.

### Examples of good usage

<example>
TaskList()
</example>

<example>
TaskList(status="pending")
</example>

### Examples of bad usage

<example>
TaskList(status="done")
<reasoning>
Invalid filter value. The status filter accepts "pending",
"in_progress", or "completed".
</reasoning>
</example>

<example>
TaskList() immediately followed by TaskGet on every returned ID
<reasoning>
TaskList already returns ID, status, subject, owner, and dependency
links. Use TaskGet only for the tasks whose details you need.
</reasoning>
</example>
