Retrieve the history of hints given for the current directive.

Use this tool at the START of each tutoring interaction to:

1. See what hints have already been given
2. Avoid repeating hints
3. Determine appropriate depth for next hint
4. Build on previous explanations

Returns:
- List of previous hints with types, concepts, and summaries
- Suggested next hint depth based on history
- Concepts already explained (to avoid repetition)

### When to use `GetHints`

- At the START of EVERY tutoring interaction
- Before providing new hints
- To check what's already been explained

### How to use `GetHints`

Simply call GetHints() with no arguments.

**Important**:
- ALWAYS call this FIRST when responding to a tutoring directive
- Use the returned information to:
  * Avoid repeating the same hints
  * Build on previous explanations
  * Adjust depth appropriately
  * Reference earlier hints ("Remember when we discussed...?")

### Examples of good usage

<example>
- Check hint history before providing new guidance
GetHints()
</example>

### Examples of bad usage

<example>
Skipping GetHints and providing hints blindly
<reasoning>
Always call GetHints first to avoid repetition.
</reasoning>
</example>

<example>
Calling GetHints multiple times in same response without using the information
<reasoning>
Call it once, review the results, then proceed with tutoring.
</reasoning>
</example>
