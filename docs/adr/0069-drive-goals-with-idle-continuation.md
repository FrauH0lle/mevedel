# Drive Goals with idle continuation

A Goal is a durable objective whose active session automatically starts another
ordinary root turn whenever settlement leaves the session idle. The model
declares only `complete` or genuinely `blocked` through `UpdateGoal`;
user/system controls handle pause and budget limits, while runtime failures
pause rather than making claims about task feasibility. Planning, approval,
independent review, model routing, and prose-verdict parsing are not Goal
phases. This trades automatic phase-specific review for a much smaller
lifecycle with explicit terminal tool calls and a completion contract supplied
in Goal context. Assessing repository evidence and whether the full objective
is complete remains model judgment; the tool does not prove task completion.
