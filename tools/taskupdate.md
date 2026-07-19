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
- `owner` — optional retained agent path such as `/root/worker_2`, or a
  deliberate user-defined bucket; empty string assigns the task to Main.
  Prefer subjects/descriptions for workstream names and use the actual retained
  path instead of inventing a proxy owner for an agent.
- `blocks` — optional full replacement of the blocks list
- `blockedBy` — optional full replacement of the blocked-by list
- `metadata` — optional replacement of the metadata object
- `note` — optional visible status note for an owner group; empty
  string intentionally clears it
- `noteOwner` — optional owner for `note`; omit for the current caller,
  or pass an empty string for Main

Unspecified fields are left untouched.

Only pass `note=""` when intentionally tearing down or replacing the
visible status note; ordinary completion updates should omit `note`
unless they need to change the status line.

### Examples

<example>
TaskUpdate(id=2, status="in_progress", note="Working through validation")
</example>

<example>
TaskUpdate(id=2, status="completed")
</example>

<example>
TaskUpdate(id=3, owner="/root/worker_2", blockedBy=[1, 2])
</example>
