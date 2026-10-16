Create or update tracked work items in the session task list, and set an
owner's visible status note.

### When to use `TaskWrite`

- Tracking status, ownership, or dependencies helps carry out the task, or the
  user asks for a checklist.
- Record progress or revise a tracked task's scope, ownership, or dependencies.

### When NOT to use `TaskWrite`

- The tracking adds no useful information for a direct answer or small edit.
- Assigning work to an agent; an owner label does not start an agent turn.

### How to use `TaskWrite`

- `tasks` is an array. An object without `id` creates a task and needs a
  nonblank `subject`; the result supplies its integer ID. An object with `id`
  updates that task: omitted fields stay unchanged, and supplied `blockedBy` and
  `metadata` replace those fields rather than merging them. The whole batch
  applies or, if any object fails, none of it does.
- Status defaults to `pending` for new tasks; use `in_progress` for work underway
  and `completed` for finished work. Concurrent work can have several active
  items. Completing a task removes its ID from dependents' `blockedBy` lists.
  Track actual work state; a status does not itself prove verification.
- Omitted owner on a new task means the caller. An agent owner is its actual
  retained path, such as `/root/worker_1`; deliberate user-defined buckets are
  also supported. Workstream names belong in subjects/descriptions. On an
  update, empty `owner` selects Main and empty `description` clears it.
- `blockedBy` contains prerequisite task IDs. Unknown and completed IDs are
  dropped, so use actual returned IDs, not guesses for future tasks.
- `note` changes an owner group's visible status, independently of task fields.
  `noteOwner` defaults to the caller; an empty string selects Main. Omit `note`
  to keep it; `note=""` clears it. Notes show only above open work. A call may
  carry only a note.

### Examples of good usage

<example>
TaskWrite(tasks=[{"id": 1, "status": "completed"},
                 {"subject": "Document accepted options", "status": "in_progress"}],
          note="Writing the options section")
Completes task 1 and starts a new task in one call.
</example>

### Examples of bad usage

<example>
TaskWrite(tasks=[{"id": 2, "blockedBy": [3]}]) intending to add 3 while keeping
existing dependencies
<reasoning>
This replaces the list. Include every dependency that should remain.
</reasoning>
</example>
