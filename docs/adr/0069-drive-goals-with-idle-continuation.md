# Drive Goals with idle continuation

A Goal is a durable objective whose active session automatically starts another
ordinary root turn whenever the session becomes idle: when a root turn settles,
and when the last interaction or queued follow-up holding an idle session
clears. The model can create a Goal through `CreateGoal` on an explicit user or
trusted instruction request and inspect any current status through `GetGoal`. It declares only
`complete` or genuinely `blocked` through `UpdateGoal`;
user/system controls handle pause and budget limits, while runtime failures
pause rather than making claims about task feasibility. Planning, approval,
model routing, and prose-verdict parsing are not Goal phases. The one
independent check is at the completion claim: `UpdateGoal(complete)` runs the
`verifier` agent in a separate request, using the `goal-review` workload
(default tier: `strong`), with the exact objective and any
accepted plan but no implementer claims, and completes the Goal only on its
fixed final `VERDICT: PASS` line. Any other verdict returns the findings to the
still-active Goal and ends the root turn at that tool boundary, so each rejected
attempt settles as its own Goal turn and continuation starts the next. This keeps a small lifecycle with explicit terminal tool
calls and a completion contract supplied in Goal context. Assessing repository
evidence remains model judgment; neither tool nor verifier proves completion.

Creation inside a running root turn establishes its Goal attribution before the
next provider interaction. Existing retained context, accounting, and terminal
settlement supply policy and subsequent continuation. Native and discoverable
tool paths share availability checks; execution also requires the owning root
request. Inspection never resumes stopped work or changes permission mode.

Continuation and follow-up delivery are re-offered at the closing seam, not by
each interaction kind: the interaction zone offers the root session's held
idle work when its last pending interaction closes, and the composer offers
queued follow-ups when an edit empties the draft that held them. Every offered
path rechecks its own gates, so a spurious offer is harmless.

The lifecycle is shared by gptel and native ACP turns. Claude's SDK reports
usage for each model sample before its post-tool hook; message identities let
the engine merge cumulative deltas and duplicate assistant snapshots without
double charging. Its final prompt totals replace reported counters, while
missing fields preserve earlier known usage. Existing Goal accounting and
acknowledged budget reminders consume those normalized counters. No separate
native Goal controller or final-turn-only budget mode is needed.

## Decision history

Continuation was first scheduled only at Goal start, resume, budget and
objective changes, and root-turn settlement. Its gate, however, also waits for
interactions and queued follow-ups that can clear while no turn runs: a child
agent's permission card or Ask answered while the root is idle, a Plan approval
decided, or a composer draft holding the follow-up queue cleared. Each left an
active Goal stalled, despite the README promising automatic continuation while
idle, until the user typed something or ran `/goal resume`. Offering the work
again where interactions close and where the draft empties fixes every
interaction kind at once without per-kind scheduling calls.

The subscription integration initially had only final prompt totals, leaving
within-turn budget reminders unproven. A bounded native probe observed three
sample identities whose summed usage exactly matched the final prompt total.
A second probe delivered the 100% reminder at the first tool boundary and
Claude wrapped up without the next planned read. This establishes the existing
reminder and settlement semantics; it does not establish a server token cap or
subscription quota measurement. Workflow tests also exercise automatic
continuation, queued-input priority, pause/resume and independent completion
verification through ACP and the real MCP pipeline.

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

Rejected completion claims first returned their findings inside the same root
turn. In a September 2026 session the implementer answered six verifier rounds
and two compactions without ever yielding, so one root turn ran from 21:56 to
09:01: the Goal showed zero turns and zero elapsed time all night, the journal
had a single capture of the whole night to summarize, queued follow-ups could
not run, and a network drop lost the only checkpoint. The "same condition
across three consecutive Goal turns" rule for `blocked` could never apply to
repeated rejections. Ending the turn at the rejected claim gives those
mechanisms their boundaries without a time or token heuristic.
