Update the active goal. Use only to mark it achieved or genuinely
blocked. You cannot pause, resume, or re-budget a goal; the user
controls those.

### When to use `UpdateGoal`

- The objective is achieved and no required work remains: set
  `status="complete"`
- The same blocking condition has recurred for at least three
  consecutive goal turns and only user input or an external change can
  resolve it: set `status="blocked"` with a `summary`

### When NOT to use `UpdateGoal`

- Work is hard, slow, uncertain, or incomplete; that is not blocked
- The budget is nearly exhausted or you are stopping; that is not
  complete
- Reporting ordinary progress -> use the task tools or plain output

### How to use `UpdateGoal`

- `status` is required: `"complete"` or `"blocked"`
- `summary` is required for blocked: name the recurring condition and
  the exact input or external change needed

### Examples of good usage

<example>
UpdateGoal(status="complete")
</example>

<example>
UpdateGoal(status="blocked",
           summary="Three consecutive turns failed at the same point: pushing needs credentials for the private registry. Provide a token or make the registry reachable.")
</example>

### Examples of bad usage

<example>
UpdateGoal(status="blocked", summary="The refactor is bigger than expected.")
<reasoning>
Hard or slow work is not blocked. Blocked requires the same condition
recurring for three consecutive goal turns and a needed external
change.
</reasoning>
</example>

<example>
UpdateGoal(status="complete") because the token budget is nearly spent
<reasoning>
Complete means the objective is achieved with no required work
remaining, never that you are stopping.
</reasoning>
</example>
