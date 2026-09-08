Create tracked work items in the session task list.

### When to use `TaskCreate`

- Tracking status, ownership, or dependencies helps carry out the task, or the
  user asks for a checklist.

### When NOT to use `TaskCreate`

- The tracking adds no useful information for a direct answer or small edit.
- Assigning work to an agent; an owner label does not start an agent turn.

### How to use `TaskCreate`

- Pass an array of task objects with nonblank `subject` strings. Optional fields
  are `description`, `status`, `owner`, `blockedBy`, and `metadata`; the result
  supplies assigned integer IDs for later calls.
- Status defaults to `pending`; use `in_progress` for work underway and
  `completed` for finished work. Concurrent work can have several active items.
- Omitted owner means the caller. An agent owner is its actual retained path,
  such as `/root/worker_1`; deliberate user-defined buckets are also supported.
  Workstream names belong in subjects/descriptions.
- `blockedBy` contains prerequisite task IDs. Unknown and completed IDs are
  dropped, so use actual returned IDs when establishing dependencies.
- Optional `note` updates the visible status above an owner's open tasks;
  `noteOwner` defaults to the caller and an empty string selects Main. The note
  has no visible row when that group has no open work.

### Examples of good usage

<example>
TaskCreate(tasks=[{"subject": "Validate parsed configuration", "status": "in_progress"},
                  {"subject": "Document accepted options"}],
           note="Checking validation behavior")
</example>

### Examples of bad usage

<example>
TaskCreate(tasks=[{"subject": "Write tests", "blockedBy": [99]}]) before task 99 exists
<reasoning>
Unknown dependencies are dropped. Create the prerequisite, then use its returned
ID instead of guessing an ID for future work.
</reasoning>
</example>
