Set or clear the visible status note for a task owner group.

Use this for short progress context that should sit above the open
tasks for Main or for a sub-agent. It is not a task and does not create
or complete work items. The note is shown only while the selected owner
has at least one open task; completed-only groups do not keep the
overlay visible.

### Arguments

- `note` — **required** short status note. Pass an empty string only
  when intentionally clearing the note.
- `owner` — optional owner. Omit for the current caller, or pass an
  empty string for Main.

### Examples

<example>
TaskNote(note="Finishing focused regressions")
</example>

<example>
TaskNote(owner="worker-2", note="Waiting on #1 before continuing")
</example>

<example>
TaskNote(note="")
</example>
