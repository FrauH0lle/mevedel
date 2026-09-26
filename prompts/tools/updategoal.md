Mark the active goal complete or blocked.

### When to use `UpdateGoal`

- `complete`: the objective is achieved and no required work remains.
- `blocked`: the same condition has recurred for at least three consecutive goal
  turns, and progress requires user input or an external change.

### When NOT to use `UpdateGoal`

- Work is difficult, slow, uncertain, or unfinished, or you are stopping because
  the budget is nearly spent. These do not establish completion or a blocker.
- Pausing, resuming, or changing the budget; those controls belong to the user.

### How to use `UpdateGoal`

- Select the status based on the actual objective and evidence. For `blocked`,
  `summary` must name the recurring condition and the specific input or external
  change needed. Ordinary progress belongs in the response or task tracking.
- `complete` starts an independent verification of the workspace. The goal
  completes only if it passes; otherwise the verifier's findings are returned,
  the goal stays active, and the turn ends. The next goal turn addresses them,
  then calls `UpdateGoal` again.

### Examples of good usage

<example>
UpdateGoal(status="complete") after verifying every requirement of the objective
</example>

### Examples of bad usage

<example>
UpdateGoal(status="blocked", summary="The refactor is bigger than expected.")
<reasoning>
Difficulty is not a blocker. Continue useful work; blocked requires a recurring
condition that cannot be resolved without user input or an external change.
</reasoning>
</example>
