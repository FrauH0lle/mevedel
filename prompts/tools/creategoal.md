Create a durable goal and start pursuing it in this conversation.

### When to use `CreateGoal`

- The user or trusted system/developer instructions explicitly request creating
  a goal, for example: "Create a goal for fixing the failing integration tests."

### When NOT to use `CreateGoal`

- An ordinary task does not implicitly request persistent Goal execution.
- There is an unfinished goal, Plan mode is active, or an accepted Plan handoff
  is pending. Creation cannot replace or resume unfinished work.
- The objective needs tools this conversation lacks, such as editing in a
  read-only discussion. Goal turns keep the current tools; say what is missing.

### How to use `CreateGoal`

- Preserve the full requested outcome in `objective`. Set `token_budget` only
  when explicitly requested; omission uses the user's configured default.
- Continue working in this turn. The harness retains the goal, supplies its
  context, accounts usage, and continues after the turn while it remains active.
- Creation preserves current permissions. Report the returned objective and
  budget; no additional confirmation is required for an explicit request.
