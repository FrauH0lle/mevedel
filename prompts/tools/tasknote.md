Set or clear the visible status note for a task owner group.

### When to use `TaskNote`

- Show useful progress context above open tasks without changing task fields.

### When NOT to use `TaskNote`

- Creating or completing work items; use TaskCreate or TaskUpdate.
- Delivering a result to the user; a status note does not replace the response.

### How to use `TaskNote`

- Pass the note text, or an empty string to clear it. Omitted `owner` means the
  caller; empty owner selects Main. Retained agent paths and deliberate
  user-defined owner buckets are supported.
- The note is visible only while its owner has at least one open task. It does
  not create work or alter task completion state.

### Examples of good usage

<example>
TaskNote(note="Checking the remaining validation cases")
</example>

### Examples of bad usage

<example>
TaskNote(note="Task 3 is finished") intending to complete task 3
<reasoning>
This changes only the note. TaskUpdate(id=3, status="completed") changes task state.
</reasoning>
</example>
