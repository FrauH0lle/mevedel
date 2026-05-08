Present an implementation plan to the user and wait for feedback.

**IMPORTANT**: This MUST be your FINAL tool call. Do not call any other
tools or add text after this.

Use this tool after drafting a plan to get user approval. The plan will
be displayed inline in the chat buffer with interactive controls, and
user feedback will be returned automatically.

User can:
- Implement the plan (begins implementation automatically)
- Implement with clear context (fresh request without conversation
  history)
- Reject the plan with feedback (you revise and call PresentPlan again)

This tool handles all user interaction - treat it as your exit point.
When the user chooses to implement, the plan is saved and implementation
starts automatically - no further action is needed from you.

### When to use `PresentPlan`

- After drafting an implementation plan that needs user approval
- When presenting multiple implementation approaches for user to choose
- Before proceeding with complex multi-file changes
- User explicitly requested to see a plan first

### When NOT to use `PresentPlan`

- Simple single-file changes that don't need planning
- User already approved approach in conversation
- Task is obvious and low-risk

### How to use `PresentPlan`

- **CRITICAL**: This MUST be your FINAL tool call
- **CRITICAL**: Do not add any text after calling PresentPlan
- Structure plan hierarchically with clear sections
- Use section types: "step" (default), "risk", "alternative",
  "dependency"
- Include specific file paths and line numbers where possible
- Include a final section named "Critical Files for Implementation"
  with 3-5 concrete paths that matter most for implementation
- Prefer high-quality plan sections that describe concrete work,
  affected modules, validation, and risks. Avoid generic sections that
  only say "inspect", "implement", or "test" without project context.

### Response handling

- If user implements: Your task is complete, implementation starts
  automatically
- If rejected: You receive user feedback + original plan; revise and
  call PresentPlan again

### Plan structure example

```json
{
  "title": "Implementation Plan: Add Authentication",
  "summary": "Add JWT-based auth with user registration and login",
  "sections": [
    {
      "heading": "Phase 1: Database Schema",
      "content": "Create users table in db/schema.sql...",
      "type": "step"
    },
    {
      "heading": "Risk: Password Storage",
      "content": "Must use bcrypt with cost 12+...",
      "type": "risk"
    }
  ]
}
```
