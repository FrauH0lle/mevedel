Retrieve one task's full details by ID.

### When to use `TaskGet`

- Read a task's description, metadata, or details absent from the list view.

### When NOT to use `TaskGet`

- Getting an overview of task statuses; TaskList supplies that in one call.

### How to use `TaskGet`

- Pass the integer ID. Returns subject, description, status, owner, dependencies,
  and metadata; an unknown ID returns an error.

### Examples of good usage

<example>
TaskGet(id=3)
</example>

### Examples of bad usage

<example>
TaskGet(id="parse config")
<reasoning>
The address is an integer ID, not subject text. TaskList can recover the ID.
</reasoning>
</example>
