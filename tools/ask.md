Ask the user one or more questions and wait for their responses.

Use this when you need clarification or user input to proceed with a
task. Supports multiple questions in a single call with navigation
between them.

Each question MUST provide predefined answer options. Users can always
provide custom input.

### When to use `Ask`

- You need user input or clarification to proceed
- Multiple implementation approaches exist and user should decide
- Gathering user preferences or requirements
- Making decisions that affect the outcome significantly
- User needs to choose between trade-offs

### When NOT to use `Ask`

- You can make a reasonable default choice
- The question is trivial or has an obvious answer
- You're overthinking and should just proceed

### How to use `Ask`

- Can ask multiple related questions in one call (better than separate
  calls)
- Each question MUST provide predefined answer options
- The tool automatically presents a custom input option to users; do
  NOT include a "custom", "other" or similar choice in your options list
- Questions are presented one at a time with navigation:
  - Users can go back to previous questions
  - Users can edit answers before submitting
  - Final confirmation screen shows all answers for review
- Format questions clearly and make options concise
- Provide 2-4 good default options per question

### Examples of good usage

<example>
Ask(questions=[{question: "Which authentication method should we use?", options: ["JWT", "Session cookies", "OAuth2"]}])
</example>

### Examples of bad usage

<example>
Ask(questions=[{question: "Should I continue?", options: ["Yes", "No"]}])
<reasoning>
Just proceed instead of asking for permission to continue.
</reasoning>
</example>
