Update an existing task's fields or status.

### When to use `TaskUpdate`

- Record progress or revise a tracked task's scope, ownership, or dependencies.

### When NOT to use `TaskUpdate`

- Creating a work item; TaskCreate supplies a new ID.
- Posting only a status note; TaskNote leaves task fields unchanged.

### How to use `TaskUpdate`

- Address the task by its integer `id`. Omitted fields stay unchanged; supplied
  `blockedBy` and `metadata` replace those fields rather than merging them.
  Unknown or completed dependency IDs are dropped.
- Completing a task removes its ID from dependent tasks' `blockedBy` lists.
  Track actual work state; the status does not itself prove verification.
- Use the retained agent path for agent ownership, or a deliberate user-defined
  bucket. Empty `owner` selects Main; empty `description` clears the description.
- `note` changes an owner group's visible status, independently of task ownership.
  `noteOwner` defaults to the caller; empty string selects Main. Omit `note` to
  keep it unchanged; `note=""` clears it. Notes show only above open work.

### Examples of good usage

<example>
TaskUpdate(id=2, status="completed")
</example>

### Examples of bad usage

<example>
TaskUpdate(id=2, blockedBy=[3]) intending to add 3 while keeping existing dependencies
<reasoning>
This replaces the list. Include every dependency that should remain.
</reasoning>
</example>
