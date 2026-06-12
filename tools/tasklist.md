List the tasks currently tracked in the session.

Returns every task with its ID, status, subject, owner, and any
dependency links. Pass an optional `status` filter to narrow the
list.

### When to use `TaskList`

- You need an overview of current progress
- You want to see what is pending or blocked
- You need to pick the next task to work on

### Arguments

- `status` — optional filter: `"pending"`, `"in_progress"`, or
  `"completed"`. Omit to see all tasks.

### Examples

<example>
TaskList()
</example>

<example>
TaskList(status="pending")
</example>
