# Plan mode

Plan mode is a sticky planning workflow independent of the session's stored
`ask`, `edits`, or `full-auto` permission mode. Enter it with `/plan`,
`/plan PROMPT`, the cockpit, or `C-<tab>` in the composer. `Shift-TAB` in
the composer continues to cycle only
`ask` -> `edits` -> `full-auto` -> `ask`,
including while Plan is active. The composer displays both dimensions as
`[Plan · MODE]`. Directive scope pauses Plan dispatch while preserving Plan for
the return to chat; its composer chrome shows MODE and `Plan paused`. A session
with an unfinished Goal cannot enter Plan; a
completed Goal is historical and does not block it.

`/plan PROMPT` enters Plan and sends `PROMPT` through the ordinary
`UserPromptSubmit` and skill-preparation transaction. Ordinary follow-up turns,
including implementation requests, remain planning input until the user exits
Plan or accepts a proposal.

## Directive Plan before implementation

A top-level directive can independently enable **Plan before implementation**
from its overlay or inspector Settings menu. Nested directive presentations use
their owner's setting. The setting is off by default and does not enter sticky
session Plan mode. Implement, Implement this, Request changes, and Retry first
send the exact eventual implementation request through a read-only planning
turn. Discuss is unchanged. Batch processing remains sequential: an
enabled directive reaches approval and implementation before the next item
starts.

Directive planning uses the directive's model and effort override when set;
otherwise it resolves the preset's `planning` workload. Its proposal
uses the ordinary `<proposed_plan>` parser and shared approval interaction. The
directive card offers only implementation Mode, Model, Skills, Instructions,
Accept, Feedback, and Cancel: execution is always Direct in the directive's
bound session and current context. The selected mode and model are request-local
implementation settings; they do not replace the session policy or the
directive's planning model. Selection survives proposal feedback, while a fresh
directive action starts with fresh defaults seeded with the directive's own
skill selection (deduplicated by skill source); the card's selection at accept
time is the single authority for that attempt. Browser feedback from a full or
owner link continues that directive's planning conversation directly, with the
current proposal as reference; it neither submits ordinary chat nor enters the
host's composer. View links cannot answer the approval. Feedback captured from
a replaced, cancelled, or invalidated attempt cannot restart its planning.
If planning policy resolution or synchronous request startup fails, the
proposal, selection, and approval attempt remain available for another feedback
submission. Startup recovery does not roll back an attempt that has already
settled or been replaced.

## Directive skills

A directive's Settings menu also toggles **skills** on the workspace record,
selected from the user-invocable roster of the directive's live bound
execution session, else the workspace's active chat session (with no open
session, selection is refused). The selection persists with the record, shows
as a `SKILLS:` label line, and attaches to direct implementation-type prompts
(Implement, Implement this, Request changes, Retry) with the accepted-plan
handoff semantics: argument-free instruction mentions stored by canonical
`SKILL.md` source that ignore skill model/effort, agent, hooks, and permission
policy. Discuss never sees the attachments. Skill sources are
revalidated at every dispatch; a missing, disabled, malformed, or no longer
user-invocable selection starts no request and leaves the directive retryable,
and batch processing skips that directive and continues.

The durable presentation progresses through Planning, Plan Ready, Plan
Accepted, Implementing, and the ordinary terminal lifecycle. Planning turns,
the current proposal, selection, and the exact accepted implementation prompt
belong to the workspace directive record. Resume restores a pending proposal;
cancel keeps the planning history as a draft; Continue Plan re-enters the
directive composer scope. Editing the owner request or a nested detail
invalidates an unstarted proposal; a late planning result cannot restore its
approval. Each in-flight attempt has a transient identity shared by its record,
session owner, and approval callbacks, so an old callback cannot publish,
accept, or release a replacement attempt. Failed, aborted, malformed, and
ownerless planning terminals remain cancelled drafts and release the session
workflow before queued input resumes; resume does not recreate ownership for
them. Rewind removes planning turns at and after the checkpoint and restores an
accepted pre-attempt plan when it removes that plan's implementation attempt.

Only one root workflow owns a session at a time. Ordinary Plan cannot start
while directive planning is active, and a directive cannot begin planning while
ordinary Plan is active. Ordinary chat input uses the existing pending-input
queue while directive approval owns the session.

## Planning model policy

Every root Plan request resolves the current preset's `planning` workload at
request realization. The resolved backend, model, and effort are local to that
request; the session's ordinary model policy and chat buffer remain unchanged.
Changing the preset therefore affects the next Plan request. Missing workload
fields inherit the session policy, while an invalid workload fails before
dispatch.

For example:

```elisp
(:model-workloads ((planning :tier strong)
                  (plan-implementation :tier balanced)))
```

A single leading user-invoked inline skill may override the planning model or
effort for that request. Instruction skills and model-side Skill calls do not.
Retained agents keep their own workload policies. Plan Summary preparation
uses the shared `summarization` workload.

The `plan-implementation` workload initializes the model and effort shown in
approval for both ordinary Plan and directive Plan before implementation. It
resolves from the owning chat buffer's preset, independently of the temporary
planning model or a directive's planning override. Its default entry is empty,
so unconfigured fields inherit the session policy. Invalid model policy prevents
creating the initial approval selection; it never silently falls back.
This workload does not select models for ordinary coding turns or delegated
workers, which retain their own policy.

## Tool boundary

Reading commands are allowed in Plan, not just native Read/Glob/Grep. Bash
uses the shared read-only classification for file reading, searching, listing,
comparison, path inspection and supported Git inspection. Pipes and `&&`,
`||`, `;` compositions are allowed when every component is read-only. For
example, `git status --short && git diff --stat` needs neither a broad Bash
allow rule nor `.git` write authority. Read-only execution is hardened against
shell startup and Git helpers; unsafe helper-dependent worktree inspection
refuses rather than silently executing helpers. Explicit denials and ordinary
resource permissions still apply. Unknown commands, output-file options,
mutating compounds and unsupported shell constructs remain blocked.
This rule is shared by standalone/sticky Plan, directive planning, and retained
agents in a planning session. See [Bash specifics](permissions.md#bash-specifics).

Plan requests expose `ApplyPatch` for session-only `work://` descendants,
including retained agents, so durable plans and notes can be updated through
the ordinary `ApplyPatch` path. Before materialization, the pipeline denies any
proposal with an ordinary, shared, memory, or bare endpoint tree-wide; this includes
mixed local/ordinary and ordinary-only proposals, and neither local nor
ordinary targets are touched. Permission modes and explicit allow rules cannot
widen this workflow boundary. Other edit tools and `Eval` remain unavailable:
arbitrary Emacs Lisp cannot be classified reliably as read-only, including in
a child process. Resource-address completion remains side-effect free. See
[`address-to-resource.md`](address-to-resource.md#execution-target-and-plan-mode).

Plan can delegate. `Agent` and the other agent tools stay available, so the
planner can run independent investigations in parallel or ask an agent to
critique a draft plan passed in its message. The Plan reminder says so; when
to delegate is the model's call. Each agent turn started under Plan carries an
immutable Plan read-only stamp. Its requests, nested agents and forked skills
keep Plan's tool and Bash limits until that turn ends, even after the proposal
is accepted or the user leaves Plan. A `/review` or `/verify` started during
Plan is bound the same way. A later follow-up from an ordinary turn starts an
ordinary turn. Skill preparation keeps the caller's stamp too, so a skill's
`!` commands follow Plan's Bash classification.

A Plan caller cannot steer an agent turn that runs outside Plan limits.
FollowupAgent on a running agent and SendMessage are accepted only when the
target's current turn is itself Plan read-only; SendMessage to `/root` is also
accepted. Otherwise the call fails and suggests FollowupAgent once the agent
is idle, which starts a Plan read-only turn. While sticky Plan is active this
also refuses agents started before Plan was entered. Interrupting, listing and
waiting are unaffected. Mail and results remain advisory text: a recipient
that reads them later acts under its own authority at that time.

Directive Plan before implementation runs on the implementation preset's tool
surface, bounded by the same Plan stamp, so it can delegate and use read-only
Bash. Its boundary is stricter in one respect: its planning requests do not
expose or allow `ApplyPatch`, including session-only proposals. `Eval` is
unavailable as in every Plan request. Directive Discuss is unchanged; it keeps
its own read-only capability ceiling, which denies delegation.

## Proposal interaction

Only a completed root-assistant prose span can create a proposal. The last
complete `<proposed_plan>...</proposed_plan>` block in that span
becomes the one actionable proposal. A later proposal replaces it; an accepted
user follow-up demotes it to a non-actionable draft while preserving the
selected axes.

Every Plan turn includes the preferred proposal shape: a concrete title,
Summary, Key Changes, Regression Coverage, Validation, and Assumptions. This
is model guidance rather than a parser-enforced schema; the proposal parser
continues to accept any nonblank Markdown inside the proposal tags. Automatic
continuation compaction restores this guidance before the next provider call,
so the proposal contract does not depend on the generated summary.

The opening tag must end its line and the closing tag must occupy its own line.
Both may be indented. Prose immediately before the opening tag is accepted:
streaming can concatenate assistant messages without an intervening newline.
Extraction and view hiding use the same delimiter rules, including while a
proposal streams. If a Plan response contains at least one
standalone proposal tag but no complete, nonblank proposal
can be extracted, mevedel shows a warning explaining why approval is unavailable
and asks you to have the assistant resend the full plan with correctly separated
tags. It does not repair the transcript, publish a proposal, or leave Plan mode.
Ordinary inline discussion of tag names, tool output, and injected agent output
do not trigger this warning. Responses without proposal delimiters remain
ordinary discussion; the diagnostic does not try to infer plan intent from prose.

The approval interaction has these axes:

| Location | Current | Fresh | Summary |
|---|---:|---:|---:|
| Here | yes | yes | yes |
| Worktree | yes | yes | yes |

- Location: Here or Worktree.
- Context: Current, Fresh, or Summary. Summary costs one additional model
  request.
- Execution: Direct or Goal.
- Budget: a positive Goal token limit, or Unlimited.
- Mode: Ask, Edits, or Full-auto.
- Model: the proposal-local implementation backend/model and reasoning effort.
- Skills: canonical user-invocable skills attached to implementation.
- Instructions: free-form multiline implementation-only guidance.

Keys are `l` for Location, `c` for Context, `e` for Execution, `m` or `TAB`
for Mode, `M` for the proposal-local Implementation model selector, `s` to
toggle skills, `i` to edit implementation instructions, and—when Goal is
selected—`b` for Budget. The ordinary `/model` cockpit remains session-level.
The instructions editor saves with `C-c C-c` and cancels with `C-c C-k`.
`RET` accepts, `f` opens an editable feedback draft, `q` hides the approval,
and `C-g` cancels.
Location and Context are independent; the default is Here/Current, and
switching to Worktree keeps Current. A Worktree starts at `HEAD`: uncommitted
changes in the source checkout are not copied, apart from the files the session
itself edited, which the fork restores. Accepting a Worktree proposal first
checks that the session can fork (no pending input, request, live execution,
active agent turn or active Goal), then asks for its branch name. Native
acceptance uses the minibuffer; browser owner acceptance uses the card's branch
input. Both validate the branch through the same Git checker before acceptance.
While Worktree is selected, the card names whatever would refuse that check,
in the same words as the refusal, so a queued follow-up is resolved in the
Pending Inputs cockpit first instead of being discovered on `RET`. The notice
that uncommitted changes stay behind comes from a git status check of about
40 ms; it runs when the card first shows and after each selection change, and
other redraws, such as queue events and the control-transfer poll, reuse it.
The blocker check is in memory and stays current on every redraw. Full and owner
links can accept Here using the host-selected settings. Only owner links can
accept Worktree; full links can still send feedback, and the host rejects forged
full-link acceptance. Settings changes remain in Emacs.

Pending input remains available while the approval overlay is open. `C-c TAB`
queues a follow-up without demoting or settling the proposal. Same-turn
steering left from the Plan request must be delivered or resolved before
acceptance. Queued follow-ups do not block a Here acceptance and run after the
accepted implementation kickoff; a Worktree acceptance refuses them, because
the planning session closes once its fork takes over. The same ordering
applies while handoff preparation or retry owns the session.

The approval card keeps the Markdown proposal expanded above a compact
Implementation section. Cycle keys appear beside their settings; accept,
feedback, hide, and cancel remain in the action footer. Each setting is one
screen row of the narrowest window showing the view, cut with an ellipsis and
its full text on hover, and the Worktree notice takes the blank row above the
actions the same way. Changing a setting therefore never changes the local
card's height: the Context and Execution descriptions differ in length and
wrapped below about 84 columns, and the notice used to add two or three rows.
The cut applies to the local card only. The browser shows read-only Markdown
with every setting, model, skill list, notice and full implementation instructions,
without inactive native keyboard hints. A guest's approval information therefore
does not depend on the host's split geometry. If the complete review exceeds
the browser frame budget, the card explicitly directs the user to review and
accept in Emacs instead of offering acceptance with truncated information;
browser feedback remains available. The native card is laid out again
when that width changes. A hidden approval
stays pending and hidden across live-session redraws. `RET` or a mouse click on its
`1 plan` interaction-counter segment shows the card again; resuming a persisted
session also shows it because hidden state is not persisted.

Direct remains the default and sends one ordinary implementation turn. Goal is
an explicit opt-in that continues automatically until complete, genuinely
blocked, paused, or budget-limited. When Goal is selected, the approval shows
the proposal's target token budget. With Direct, the Budget row stays as a
dimmed `Goal execution only` placeholder without a key, so cycling Execution
never changes the card's height: with the composer anchored on its screen row,
each toggle otherwise moved the card and the transcript above it by one row
(ADR 0126). `b` accepts a positive integer; empty input
means Unlimited. The setting starts from the effective session default, stays
local to the pending proposal, survives Execution toggles and revised
proposals, and on acceptance sets only the new Goal's budget, not the
`mevedel-goal-token-budget` default. The first proposal resolves the
`plan-implementation` workload and snapshots its model and effort into the same
persisted selection. Later preset changes do not replace that selection.
`M` changes only that implementation snapshot; choosing a model that cannot
support the selected effort resets effort to its default. Cancellation, Plan exit, and
acceptance all leave the ordinary session model, effort, and permission mode
unchanged. Every implementation target and retry uses the accepted snapshot
within its implementation scope.

### Implementation-scoped policy

The accepted Model, effort, and Mode form one strict implementation policy.
Direct captures it on the one implementation request; retries reuse the same
accepted selection rather than resolving new defaults. Goal stores it on the
durable Goal and uses it for every active ordinary root Goal turn, including
user follow-ups, automatic continuation, and turns after `/goal resume`.
Ordinary chat while the Goal is paused, blocked, budget-limited, or complete
uses the session baseline. An already-admitted implementation turn retains its
captured policy through settlement even if the Goal stops meanwhile.

Neither Here nor Worktree promotes these settings to session defaults. A
Worktree fork copies the planning session's ordinary defaults and preset; the
accepted selection is not installed as that baseline. Summary, control, directive, and unrelated child
requests do not inherit an ambient Goal implementation policy. Directive
implementation retains its own request-local selection on the common
permission path.

Children started from an implementation turn inherit its permission-mode
override for that child turn, including nested delegation, but retain their
own model workload resolution. The override changes mode-based decisions only:
current hard denies, shared permission rules, grants, and workflow ceilings
remain effective. Views, permission logs, and reminders report effective
request settings without changing stored defaults. There is no session-policy
snapshot/restore cycle; user changes to the baseline remain intact.

The exact backend/model and effort must resolve at dispatch. An unavailable
provider/model or unsupported effort fails visibly rather than falling back to
the session model; an active Goal pauses on that runtime failure. A provider's
structured rejection of an accepted model also leaves the baseline untouched,
instead of applying ordinary session-model fallback recovery.

## Acceptance and recovery

Acceptance archives the plan immutably, clears the interaction, exits Plan,
and shows `Preparing implementation...`. It publishes and archives before it
moves any proposal metadata, so an acceptance that fails before the immutable
archive exists leaves the proposal exactly as it was and resume still offers
it. That mirrors the rule for a failure after the archive, which stays
accepted. Here/Current keeps the planning
transcript. Fresh starts a new context with setup material and the accepted
plan. Both Summary locations project the same effective source history and
generate the shared eight-section handoff background. The accepted proposal is
replaced in that evidence by an omission marker; the exact immutable plan and
implementation-only instructions are supplied separately as relevance focus
and remain authoritative. A Worktree implementation runs these same contexts
in its fork; see [Worktree implementation](#worktree-implementation).

Summary applies that background through aggressive root compaction, with
no verbatim tail, and therefore retains `PreCompact`/`PostCompact`, bounded
request retries, segment rotation, persistence, and the new compact context
epoch. The successful result is cached in the durable retry record, so a later
implementation-start failure does not regenerate it.

Durable Plan descriptors contain only a normalized session-relative artifact
path and its content hash. The qualified logical path is derived after the
owning session is known; remote consumers read verified publication
bytes for that logical artifact. Absolute source or publication paths are not
part of Plan metadata or retry records.

When the prepared submission is ready, the view releases its
`Preparing implementation...` row before kickoff.

After any valid location/context preparation, Direct submits its ordinary
one-turn instruction. Goal instead constructs a Goal in the implementing
session, using its own immutable accepted-plan path and the
Plan-selected Goal budget. Its deterministic objective treats plan
outcomes, constraints, and acceptance criteria as the completion contract while
allowing mechanics to follow current repository evidence. The first canonical
turn receives the prepared context, resolved artifact path, full accepted plan,
and compact kickoff; the transcript stores that full input while the view shows
`Implement accepted plan as Goal`.

The generated implementation turn goes through the ordinary deterministic
skill planner. Skills selected with `s` are stored by canonical `SKILL.md`
source and attached as argument-free instruction mentions. Live `$skill`
mentions written in the `i` instructions editor use the same semantics and
deduplicate with selected skills. Instruction attachments ignore skill
model/effort, agent, hooks, and request permission policy. Their current source
is reloaded at dispatch and retry; an explicitly bound skill that is missing,
disabled, malformed, or no longer user-invocable leaves the accepted handoff
retryable and starts no request. The transcript stores the full generated
prompt while the rendered view keeps the compact implementation label, both
live and after reconstruction. Prepared skill bodies travel in that model
prompt and appear immediately in a separate collapsed attached-skills
disclosure. Its expansion is independent of the Prompt drawer, hook context,
and ordinary system-reminder batches; attachments do not increase the reminder
batch count.

### Worktree implementation

A Worktree implementation is a [Worktree Fork](sessions.md#fork) of the
planning session at the reply that proposed the plan, so the fork carries the
whole planning conversation into a linked worktree on the accepted branch, at
`.worktrees/<branch leaf>`. The fork receives its own byte-identical accepted
artifact and a retry record for the equivalent Here handoff, with
repository-local skill paths retargeted to its checkout, and then runs the
selected context and execution exactly as Here does: Current submits at once,
Fresh rotates the fork's segment, Summary compacts the fork, and Goal lives in
the fork. The fork's view takes over every window that showed the planning
view, and the planning session, now saved with only its accepted-plan archive,
is closed once the accepting command returns. Both sessions stay linked
through the fork's recorded parent and fork point; the planning conversation
remains reachable through the variant button at the plan reply.

If the planning session is shared, handoff shares the implementation fork and
ends the old room with an explicit continuation notice. Each connected browser
receives an Open link for the new room at its existing view, full, or owner tier;
the old bearer never changes which session it names. An unshared planning
session does not start sharing. If sharing the fork fails, the old room ends
with an explicit handoff-failed notice and the native handoff still completes.

Each step is durable before the next. The source retry records the fork point,
then the branch and directory reservation before any Git mutation, then the
fork's save path once the fork transaction has published it. A retry reopens
that fork, or finds the unique saved fork of this parent rooted at the reserved
directory, instead of forking again; a published plan recognized by its hash is
not installed again. The source's retry is cleared only after the fork holds
its own, so at every point exactly one session owns the implementation. A
directory that exists with no fork is refused with the fork transaction's
cleanup command.

Acceptance is final even if preparation fails. The source session persists a
bounded retry record, and `mevedel-retry-plan-implementation` resumes from the
completed preparation step instead of recreating artifacts, summaries, or
worktrees. A post-acceptance preparation failure settles the original approval;
it never restores an action that could archive the same proposal again. Here
and Worktree retries capture the same accepted implementation policy at dispatch
without changing either session baseline. Goal execution reserves and persists
its Goal ID in that record; a Worktree fork inherits the reservation.
A retry reuses a Goal only when the reserved ID, accepted-plan reference, and
stored implementation policy match; a different
unfinished Goal or conflicting policy remains untouched and is reported as a
conflict.

Goal construction is the Plan recovery boundary. The Goal, including
its implementation policy and selected budget, is persisted before the
retry is cleared and kickoff starts. Kickoff retains the exact Goal owner and
reserved identity across publication. If that Goal is paused, cleared, replaced,
edited, or given a different policy before dispatch, or another request starts,
the stale kickoff is rejected without sending or pausing the current Goal.
An actual startup failure pauses only the still-owned matching Goal with its
runtime reason; recovery uses ordinary `/goal resume`, not Plan retry.
Direct keeps the exact prepared retry record
through request startup and clears it only when the canonical request settles
successfully, whether gptel's FSM or an external engine such as Claude Code owns
it. While kickoff authority is staged before admission, a competing ordinary
composer submission is refused rather than inheriting that authority.
Provider errors and aborts retain the record with the terminal reason for
`mevedel-retry-plan-implementation`. Recorded reasons are bounded text: error
data can carry whole requests or sessions, so printing is depth-limited and
truncated rather than copying transcripts into the record and echo area.
If recovery finds the matching Goal already durable but paused by session
restore, it reactivates it without scheduling before retrying the owned
kickoff, preventing both duplicate Goals and duplicate continuations.

For Goal execution, non-command input submitted after acceptance queues behind
the reserved kickoff, in the fork for a Worktree implementation. Each queued
follow-up then runs as its own Goal turn before automatic continuation. Paused
kickoff-owned input remains held until resume.

Plan activity, proposal identity, selection, artifact descriptors, and retry
state persist in session metadata. Resume reconstructs an approval only when
Plan is active, metadata says `proposed`, and the current artifact still
matches its recorded hash. Persisted proposals without a valid implementation model
snapshot are demoted to drafts. Drafts never reactivate
automatically.

A failed approval renderer dismisses the interaction without touching that
metadata, so the proposal stays restorable: only a user cancellation discards
the selection. A directive-owned approval still settles its caller, because a
waiting batch must not hang on a rendering bug.
