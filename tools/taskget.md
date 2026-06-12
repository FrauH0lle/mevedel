Retrieve full details for a single task by ID.

Returns the task's subject, description, status, owner, dependency
links, and metadata. Use this when `TaskList` output isn't detailed
enough and you need to inspect one specific task.

### Arguments

- `id` — **required** integer task ID

### Example

<example>
TaskGet(id=3)
</example>
