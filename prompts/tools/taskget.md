Retrieve full details for a single task by ID.

Returns the task's subject, description, status, owner, dependency
links, and metadata. Use this when `TaskList` output isn't detailed
enough and you need to inspect one specific task.

### When to use `TaskGet`

- You need the description, metadata, or dependency details of one
  specific task before working on it

### When NOT to use `TaskGet`

- You need an overview of several tasks -> use `TaskList`
- A recent `TaskList` already showed the fields you need

### How to use `TaskGet`

- `id` — **required** integer task ID

### Examples of good usage

<example>
TaskGet(id=3)
</example>

### Examples of bad usage

<example>
TaskGet(id=1), TaskGet(id=2), TaskGet(id=3) to see all tasks
<reasoning>
Iterating over every task re-implements TaskList. Call TaskList once.
</reasoning>
</example>

<example>
TaskGet(id="parse config")
<reasoning>
Tasks are addressed by their integer ID, not by subject text. Find the
ID with TaskList first.
</reasoning>
</example>
