Launch the planner agent to create an implementation plan.

Use this tool when the task is complex enough to warrant planning before
implementation. The planner agent will explore the codebase, draft a
structured plan, and present it to the user for approval.

After the user approves the plan, implementation begins automatically -
you do not need to do anything further.

### When to use `CreatePlan`

- Complex multi-file changes that benefit from upfront planning
- Architectural decisions or significant refactoring
- User explicitly asks for a plan before implementation
- Task involves tradeoffs that should be discussed first

### When NOT to use `CreatePlan`

- Simple, obvious changes (single file edits, typo fixes)
- User has already described exactly what to do
- Task is straightforward with no ambiguity

### How it works

1. You call CreatePlan with a description and detailed prompt
2. The planner agent explores the codebase and drafts a plan
3. The plan is presented to the user for approval
4. If approved, the plan is implemented automatically
5. You do NOT need to implement the plan yourself after this tool returns

### Plan quality

High-quality plans name concrete phases, files or modules likely to
change, validation steps, and unresolved decisions. They make tradeoffs
visible without bloating the plan.

Low-quality plans are generic checklists like "inspect, implement,
test" with no project-specific detail or sequencing rationale.
