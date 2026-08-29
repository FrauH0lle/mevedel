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
- Options may be plain strings or objects with `label`, `description`,
  and `sample` fields. Object options still return the selected label
  as the answer.
- The tool automatically presents a custom input option to users; do
  NOT include a "custom", "other" or similar choice in your options list
- Every question is shown at once in a single form. Users move freely
  between them, change an answer by moving back to its question, and
  submit the form when they are done. A question left unanswered is
  submitted as an explicit "no preference" rather than being blocked.
- Format questions clearly and make options concise
- Provide 2-4 good default options per question
- Mark exactly one option per question as recommended by appending
  ` (Recommended)` to that option label. Do not add a separate
  recommendation field.
- Use `description` for a short trade-off note and `sample` only when
  the user needs to compare proposed output before selecting an option.
  A `sample` opens a side frame beside the option list and updates as
  the user moves between options, so give one to every option of that
  question or to none of them -- an option without a sample closes the
  frame. Do not use samples for simple preference questions where
  labels and descriptions suffice.

### Examples of good usage

<example>
Ask(questions=[{question: "Which authentication method should we use?", options: ["JWT (Recommended)", "Session cookies", "OAuth2"]}])
</example>

<example>
Ask(questions=[{question: "Which files should I write?", options: [{label: "Project AGENTS.md (Recommended)", description: "Shared repo guidance.", sample: "# Repository Guidelines\n- Use pnpm.\n- Run focused tests."}, {label: "Personal AGENTS.local.md", description: "Private checkout notes.", sample: "# Local notes\n- Skip the slow suite."}]}])
</example>

### Examples of bad usage

<example>
Ask(questions=[{question: "Should I continue?", options: ["Yes", "No"]}])
<reasoning>
Just proceed instead of asking for permission to continue.
</reasoning>
</example>
