List active model-invocable skills, optionally filtered by query.

### When to use `ListSkills`

- Checking whether a skill exists for the task before invoking one
- Recalling the exact name or purpose of an available skill

### When NOT to use `ListSkills`

- You already know the exact skill name from the roster or the user ->
  call `Skill` directly
- Browsing repeatedly in one turn; the listing does not change mid-turn

### How to use `ListSkills`

- `query` is an optional case-insensitive search over skill name and
  description; omit it to list everything
- Names returned here are the valid `name` values for the `Skill` tool

### Examples of good usage

<example>
ListSkills()
</example>

<example>
ListSkills(query="review")
</example>

### Examples of bad usage

<example>
ListSkills(query="please list all skills that could help with testing")
<reasoning>
The query is a substring match over names and descriptions, not a
natural-language request. Use a short keyword such as "test".
</reasoning>
</example>

<example>
ListSkills() followed by Skill(name="some-skill") for a name it did not return
<reasoning>
Only listed names are invocable; a missing name means the skill is not
active in this session.
</reasoning>
</example>
