Create one or more tasks in the session task list.

Tasks are the unified tracking system for planning and progress: they
work as a flat checklist for simple sessions, and as a dependency
graph for agent trees. Each task gets an auto-assigned
integer ID that you can reference later from `TaskUpdate`, `TaskGet`,
or from other tasks via `blockedBy` / `blocks`.

### When to use `TaskCreate`

- You are about to work on a non-trivial task with multiple distinct
  steps
- You need to plan a batch of work items up front so you can track
  progress
- You are coordinating work across multiple sub-agents and need to
  track ownership and dependencies
- The user explicitly asks you to use the task system

### When NOT to use `TaskCreate`

- The work is a single trivial edit or a direct answer
- You would create only one pending task and never update it

### Task object shape

Pass `tasks` as an array. Each task object may contain:

- `subject` — **required** short one-line summary
- `description` — optional longer notes
- `status` — optional `"pending"`, `"in_progress"`, or `"completed"`
  (defaults to `"pending"`)
- `owner` — optional retained agent path such as `/root/worker_1`, or a
  deliberate user-defined bucket. Omit it for your own tasks. Use
  subjects/descriptions for workstream names; use the actual retained path
  instead of inventing a proxy owner for an agent.
- `blockedBy` — optional array of task IDs that must complete first
- `blocks` — optional array of task IDs this one blocks
- `metadata` — optional free-form object for extra data

Top-level `note` may be passed with the create call to update the
visible status note for an owner group. It is shown only while that
owner has open tasks. `noteOwner` selects the owner; omit it for the
current caller, or pass an empty string for Main.

Use `in_progress` for tasks that are actually being worked on now.
Multiple tasks may be `in_progress` when work is genuinely concurrent,
especially across sub-agents.

### Examples

<example>
TaskCreate(tasks=[
  {"subject": "Parse config file", "status": "completed"},
  {"subject": "Validate parsed values", "status": "in_progress"},
  {"subject": "Emit validated config", "status": "pending"}
], note="Validating parsed values")
</example>

<example>
TaskCreate(tasks=[
  {"subject": "Implement module A", "owner": "/root/worker_1"},
  {"subject": "Implement module B", "owner": "/root/worker_2", "blockedBy": [1]}
], note="Module B waits for module A", noteOwner="/root/worker_2")
</example>
