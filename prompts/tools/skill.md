Invoke a reusable prompt recipe (skill) by name.

A skill supplies task-specific instructions or a configured agent workflow.

### When to use `Skill`

- The user requests the skill and its prepared guidance is not already present.
- An available skill's scope and approach would help the task. A description
  match alone does not require invocation.

### When NOT to use `Skill`

- You need to discover a name or purpose; use `ListSkills(query)`.
- The relevant prepared guidance is already in context and unchanged; reuse it.

### How to use `Skill`

- `Read` inspects a skill's source; `Skill` runs its preparation and configured
  workflow. Reading the file does not invoke the skill.
- Use the exact canonical name from the user, roster, or ListSkills result,
  including a namespace when present. Queried dormant skills can be invoked;
  user-only or disabled skills cannot be invoked by this tool.
- `arguments` is a string interpreted by the skill. Preparation applies
  substitutions and required dependencies; errors report missing prerequisites.
- An inline skill returns prepared guidance. A skill configured to fork runs
  in its own agent context and returns its outcome. The result reports any
  policy fields that could not apply to the current request.
- Keep guidance within user scope and its authored applicability. A new
  message alone does not end an ongoing skill; completed or superseded work
  does. Retrieve guidance again if needed detail is absent or its source changed.

### Examples of good usage

<example>
Skill(name="analyze-log", arguments="~/logs/session.log")
-> Run the discovered log-analysis skill with this argument.
</example>

### Examples of bad usage

<example>
Skill(name="review") called again immediately after its body loaded
<reasoning>
Reuse the loaded guidance unless new arguments or changed instructions require
another invocation.
</reasoning>
</example>
