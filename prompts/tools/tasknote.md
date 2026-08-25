Set or clear the visible status note for a task owner group.

Use this for short progress context that should sit above the open
tasks for Main or for a sub-agent. It is not a task and does not create
or complete work items. The note is shown only while the selected owner
has at least one open task; completed-only groups do not keep the
overlay visible.

### When to use `TaskNote`

- Posting short progress context that should sit above the open tasks
  for Main or for a sub-agent
- Updating the visible status line between task transitions without
  touching any task fields

### When NOT to use `TaskNote`

- Creating or completing work items -> use `TaskCreate` / `TaskUpdate`
- Reporting results to the user -> output text directly
- The owner group has no open tasks; the note is only shown while at
  least one open task exists

### How to use `TaskNote`

- `note` — **required** short status note. Pass an empty string only
  when intentionally clearing the note.
- `owner` — optional retained agent path or deliberate user-defined bucket.
  Omit it for the current caller, or pass an empty string for Main.

### Examples of good usage

<example>
TaskNote(note="Finishing focused regressions")
</example>

<example>
TaskNote(owner="/root/worker_2", note="Waiting on #1 before continuing")
</example>


### Examples of bad usage

<example>
TaskNote(note="Task 3 is finished")
<reasoning>
Completion is task state, not a status note. Use
TaskUpdate(id=3, status="completed") instead.
</reasoning>
</example>

<example>
TaskNote(note="")
<reasoning>
Clears the visible status note. Only do this when intentionally
tearing down or replacing the note, never as a routine call.
</reasoning>
</example>
