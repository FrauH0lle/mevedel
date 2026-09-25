# Drive Goals with idle continuation

A Goal is a durable objective whose active session automatically starts another
ordinary root turn whenever settlement leaves the session idle. The model
can create a Goal through `CreateGoal` on an explicit user or trusted instruction
request and inspect any current status through `GetGoal`. It declares only
`complete` or genuinely `blocked` through `UpdateGoal`;
user/system controls handle pause and budget limits, while runtime failures
pause rather than making claims about task feasibility. Planning, approval,
independent review, model routing, and prose-verdict parsing are not Goal
phases. This trades automatic phase-specific review for a much smaller
lifecycle with explicit terminal tool calls and a completion contract supplied
in Goal context. Assessing repository evidence and whether the full objective
is complete remains model judgment; the tool does not prove task completion.

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
