Present questions in one form and wait for the user's answers.

### When to use `Ask`

- Missing information or a preference materially affects correctness, scope,
  authorization, or the result the user wants.

### When NOT to use `Ask`

- A routine, reversible decision has a reasonable default within the user's scope.
- Requesting a tool's built-in permission grant; use its permission fields instead.

### How to use `Ask`

- Group related questions. Each needs concise predefined options; custom input is
  always available, so do not add an "Other" option. Prefer 2-4 useful choices.
- Mark exactly one option per question with ` (Recommended)` in its label.
  Options can be strings or objects with `label`, `description`, and `sample`.
  Object options return the selected label.
- Use `description` for a short trade-off. Use `sample` when comparing proposed
  output helps the choice; supply it for every option in that question or none.
  The current option's sample appears in a side frame.
- The user can revise answers before submitting the form. An unanswered question
  is submitted as "no preference"; it is not authorization. Cancellation returns
  an error indicating that no answer was submitted.

### Examples of good usage

<example>
Ask(questions=[{
  question: "Where should this guidance apply?",
  options: [
    {label: "This repository (Recommended)", description: "Shared with contributors.",
     sample: "AGENTS.md: Run the focused tests before committing."},
    {label: "My checkout", description: "Private local guidance.",
     sample: "AGENTS.local.md: Run the focused tests before committing."}
  ]
}])
Both alternatives supply a sample; the answer is the selected label, not the
sample text. These samples illustrate the proposed placement.
</example>

### Examples of bad usage

<example>
Ask(questions=[{question: "Should I continue the implementation you requested?", options: ["Yes (Recommended)", "No"]}])
<reasoning>
Continue authorized work when no material choice or blocker needs the user's input.
</reasoning>
</example>
