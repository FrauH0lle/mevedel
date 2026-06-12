Record a hint that you just gave to the user.

Use this tool EVERY TIME you provide a hint, question, or guidance. This
helps track what has been explained and prevents repetition.

### When to use `RecordHint`

- IMMEDIATELY after providing ANY hint, question, or guidance
- After pointing to documentation or code examples
- After asking a Socratic question
- After breaking down a problem into steps

### How to use `RecordHint`

Call `RecordHint` with:
- hint_type: The teaching method used
- concept: What topic/concept this addresses (short, kebab-case)
- hint_summary: One-line description for user's reference
- depth: How detailed (1=nudge, 2=gentle, 3=medium, 4=detailed, 5=very detailed)

**Important**:
- Call this EVERY TIME you give guidance (builds accurate history)
- The user will see the tool call and result in their chat
- This helps you avoid repeating yourself

### Examples of good usage

<example>
RecordHint(hint_type="technique-hint", concept="error-handling", hint_summary="Suggested try-catch pattern", depth=3)
</example>

### Examples of bad usage

<example>
- Forgetting to call RecordHint after providing guidance
<reasoning>
Always record hints to maintain accurate history.
</reasoning>
</example>

<example>
- Calling RecordHint with generic concept names like "help"
<reasoning>
Use specific kebab-case concepts like "array-methods" or "async-patterns".
</reasoning>
</example>
