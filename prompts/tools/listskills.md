Discover enabled model-invocable skills by name or purpose.

### When to use `ListSkills`

- Finding a useful skill for a task, including one omitted from the roster
- Recalling the exact name or purpose of an available skill

### When NOT to use `ListSkills`

- You already know the exact skill name from the roster or the user ->
  call `Skill` directly
- Repeating an unchanged query without a change in need or available skills

### How to use `ListSkills`

- A nonempty `query` searches enabled model-invocable skills, including dormant
  path-scoped skills. Matching is case-insensitive over names and descriptions.
- Without a query, list currently active model-invocable skills. Results are
  capped and report omitted matches; narrow the query when needed.
- Returned canonical names can be passed to `Skill`. Discovery does not invoke
  a skill or make its workflow mandatory. User-only and disabled skills are
  excluded; a missing result is not permission to guess a name.

- File-backed entries include their registered `skill://` address. Use `Read`
  on that address to inspect the source, or `Read("skill://")` to list
  registered resources. Raw directory development uses ordinary filesystem
  permissions.

### Examples of good usage

<example>
ListSkills(query="review")
-> Inspect matching skills before choosing one.
</example>

### Examples of bad usage

<example>
ListSkills(query="please list all skills that could help with testing")
<reasoning>
The query is a substring match over names and descriptions, not a
natural-language request. Use a short keyword such as "test".
</reasoning>
</example>
