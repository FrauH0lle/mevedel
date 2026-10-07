Inspect the current session goal without changing or resuming it.

### When to use `GetGoal`

- Answer questions about the objective, status, stop reason, or remaining budget.
- Check an inactive goal or refresh facts after context loss.

### How to use `GetGoal`

- Call with no arguments. `goal: null` means this session has no goal.
- Usage includes settled turns and known usage of the current attributed turn.
  In-flight provider usage can lag; `turns_run` counts settled turns only.
- A null budget means unbounded. When native usage is incomplete, `tokens_used`
  and `remaining_tokens` are null; `known_tokens_used` is the measured lower bound.
  A bounded Goal stops automatic continuation when settled usage is incomplete.
  Paused, blocked, and budget-limited goals stay stopped; inspection grants no execution authority.
- An invalid plan address is reported in `plan_reference_error`.
