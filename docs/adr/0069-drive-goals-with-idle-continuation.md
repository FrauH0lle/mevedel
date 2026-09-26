# Drive Goals with idle continuation

A Goal is a durable objective whose active session automatically starts another
ordinary root turn whenever settlement leaves the session idle. The model
can create a Goal through `CreateGoal` on an explicit user or trusted instruction
request and inspect any current status through `GetGoal`. It declares only
`complete` or genuinely `blocked` through `UpdateGoal`;
user/system controls handle pause and budget limits, while runtime failures
pause rather than making claims about task feasibility. Planning, approval,
model routing, and prose-verdict parsing are not Goal phases. The one
independent check is at the completion claim: `UpdateGoal(complete)` runs the
`verifier` agent in a separate request, using the `goal-review` workload
(default tier: `strong`), with the exact objective and any
accepted plan but no implementer claims, and completes the Goal only on its
fixed final `VERDICT: PASS` line. Any other verdict returns the findings to the
still-active Goal. This keeps a small lifecycle with explicit terminal tool
calls and a completion contract supplied in Goal context. Assessing repository
evidence remains model judgment; neither tool nor verifier proves completion.

Creation inside a running root turn establishes its Goal attribution before the
next provider interaction. Existing retained context, accounting, and terminal
settlement supply policy and subsequent continuation. Native and discoverable
tool paths share availability checks; execution also requires the owning root
request. Inspection never resumes stopped work or changes permission mode.

## Decision history

Initially creation was a user command or accepted-plan handoff, and the model
could only complete or block an existing Goal. The requested natural-language
workflow ("Create a goal for XYZ") exposed the missing model entry point. Adding
creation and inspection reuses the existing lifecycle rather than requiring the
user to translate that intent into a slash command. The implementation review
also found that creation mid-turn needs explicit request attribution: otherwise
accounting and post-turn continuation do not recognize the new Goal.

Earlier Goals embedded planner/guardian negotiation and per-cycle review with
their own provider and effort. ADR 0067 and this ADR removed them to shrink
the lifecycle, not because of a measured failure. Completion review returned
once the `plan-implementation` workload let accepted plans run on a
deliberately cheaper implementer: that model should not also be the sole judge
of whether the full objective is achieved. The check is limited to the
completion claim and reuses the existing verifier agent and its
verdict line, so no Goal phase, review record, or prose parsing returns.

Completion checks initially shared the `verifier` workload. Configuring a
stronger Goal acceptance model therefore also raised the cost of ordinary
verification. The dedicated `goal-review` workload separates those choices
while retaining the same verifier role, isolated context, and completion gate.
