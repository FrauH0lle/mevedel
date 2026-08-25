Invoke a reusable prompt recipe (skill) by name.

A skill packages instructions for a particular kind of task. Invoking
one returns its prepared body as the tool result; follow those
instructions in place of your default approach.

### When to use `Skill`

- The task at hand is one a listed skill covers; invoke it before
  improvising your own approach
- The user asks for a skill by name

### When NOT to use `Skill`

- The name is not in the active skills listing; do not guess names
- You only want to know what skills exist -> use `ListSkills`
- The skill's instructions are already loaded this turn; follow them
  instead of invoking again

### How to use `Skill`

- `name` is the exact skill name as shown in the skills listing
- `arguments` is an optional argument string passed through to the
  skill
- The result is the skill body with any argument substitution applied;
  treat it as instructions for the current task

### Examples of good usage

<example>
Skill(name="review")
</example>

<example>
Skill(name="analyze-log", arguments="~/logs/session.log")
</example>

### Examples of bad usage

<example>
Skill(name="deploy")
<reasoning>
Guessed name that is not in the skills listing; the call fails with an
unknown-skill error. Check ListSkills first.
</reasoning>
</example>

<example>
Skill(name="review") called again immediately after its body loaded
<reasoning>
The instructions are already in context. Follow them; re-invoking adds
nothing.
</reasoning>
</example>
