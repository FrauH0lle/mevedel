Update the status or fields of an existing task.

Use this to mark a task as `in_progress` when you start it and
`completed` when you finish. When a task is marked completed, its ID
is automatically removed from the `blockedBy` list of every task that
depended on it, so downstream tasks become unblocked.

### When to use `TaskUpdate`

- You are starting work on a pending task — set status to
  `"in_progress"`
- You just finished a task — set status to `"completed"`
- You need to revise a task's subject, description, owner, or
  dependencies as the plan evolves
- You discover a new dependency between existing tasks

### Fields

- `id` — **required** integer task ID to update
- `subject` — optional new subject line
- `description` — optional new description (empty string clears it)
- `status` — optional `"pending"`, `"in_progress"`, or `"completed"`
- `owner` — optional new owner (empty string unassigns)
- `blocks` — optional full replacement of the blocks list
- `blockedBy` — optional full replacement of the blocked-by list
- `metadata` — optional replacement of the metadata object

Unspecified fields are left untouched.

### Examples

<example>
TaskUpdate(id=2, status="in_progress")
</example>

<example>
TaskUpdate(id=2, status="completed")
</example>

<example>
TaskUpdate(id=3, owner="worker-2", blockedBy=[1, 2])
</example>
