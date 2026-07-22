# Goals

A Goal is one durable objective attached to a session. It keeps the objective
in context across ordinary root conversation turns and continues while the
session is idle. There is no Goal phase machine: planning, implementation, and
review are ordinary model work.

## Durable state

The session sidecar stores a strict Goal record containing:

- a unique ID and free-form objective;
- `active`, `paused`, `blocked`, `budget-limited`, or `complete` status plus an
  optional reason;
- token, elapsed-time, and turn accounting;
- an optional token budget and accepted-plan reference; and
- creation and update timestamps.

Old Goal schemas are invalid and load as no Goal. An active Goal loaded from a
saved session is demoted to `paused` with a session-resumed reason so recovery
cannot dispatch work without an explicit `/goal resume`.

## Request context and authority

Immediately before every active root request, mevedel generates request-local
Goal context from the durable record. It contains the objective, accounting,
remaining budget, and any accepted-plan reference. It is never inserted into
the visible or persisted transcript. Ordinary user messages during a Goal are
steering turns and receive the same context and accounting as automatic turns.

`/goal edit <objective>` revises the live objective and rotates the Goal ID
while retaining status, budget, accounting, accepted-plan reference, and
creation time. The revised objective has highest authority; an accepted Plan
remains binding only where consistent with it. Stale `UpdateGoal` calls from an
already-running turn are rejected, but that turn is still charged to the
revised Goal. At a supported in-flight steering boundary, mevedel also sends
the refreshed Goal context on a best-effort basis. The next prompt consumes one
objective-updated reminder, and an active Goal schedules continuation behind
the current request gate.

When a Goal references an accepted Plan, each turn validates the reference
against the session's accepted-path metadata, immutable artifact, and stored
hash. A valid artifact receives exact read authority for that request. A
missing, moved, mutated, or mismatched artifact pauses the Goal before provider
dispatch; transcript prose is never a fallback source of Plan authority.

Child-agent, compaction, and control requests are not Goal turns. A root turn
captures its Goal identity at request start and charges tokens, wall time, and
one turn at canonical success or failure settlement. Token accounting uses
normalized provider input plus output usage, excluding cached-input counts,
with the request estimate as fallback.

Compaction does not copy Goal state into summaries or segment snapshots and
does not queue a static Goal reminder. Ordinary steering remains ordinary
conversation history: unresolved requests may survive as actionable summary
steps, while satisfied requests retire to outcome or evidence under completed
work. Fresh request-local Goal context remains the only model-visible Goal
authority.

## Token budget

The optional token budget is the user-selected runaway bound. Request context
and the cockpit display bounded usage as used/limit and otherwise say
`unbounded`. Charging a turn emits one-shot 50%, 80%, and 100% crossing
reminders. These need no durable reminder ledger because settlement compares
usage immediately before and after the monotonic charge.

Crossing the limit never aborts an in-flight request or tool. When provider
usage is already known at a tool-result boundary, the first 100% crossing adds
one hidden warning asking the model to stop new substantive work and wrap up
the current response. It does not create a budget-exempt wrap-up turn. At
settlement, an otherwise-active Goal at or above the limit becomes
`budget-limited`; a `complete` or `blocked` decision from that turn wins.

`/goal budget <N|none>` replaces or removes the durable limit and queues one
reminder with the old limit, new limit, usage, remaining tokens, and resulting
status. Lowering the limit to current usage immediately limits a nonterminal
Goal. Raising it above usage or removing it from a budget-limited Goal
reactivates the Goal and schedules continuation behind the ordinary request
gate.

## Continuation

Starting or resuming a Goal schedules the ordinary continuation text:

```text
Continue working toward the active Goal.
```

Successful and retryable failed root turns schedule the same continuation only
after canonical request teardown. Dispatch requires all of the following:

- the Goal is active;
- no root request is running;
- no permission or Plan interaction is pending;
- no user message is queued; and
- the token budget is not exhausted.

Queued user input always runs first. Once that steering turn settles, the Goal
loop may continue. There is no maximum-turn or no-tool heuristic.

A transient transport failure is retried once. Terminal provider, transport,
compaction, and other runtime failures pause the Goal with a concrete reason.
A completion or blockage already recorded by `UpdateGoal` is terminal and is
not overwritten by later request failure handling. User interruption also
pauses the Goal.

## Completion tool

`UpdateGoal` is a permission-free control tool visible only to an active root
Goal. It accepts exactly:

- `complete`; or
- `blocked` with a nonblank summary, stored as the Goal reason.

The tool reports only the status transition. Canonical turn settlement still
persists the final accounting.

## Commands and UI

- `/goal <objective>` starts a Goal and schedules its first turn.
- Bare `/goal` opens the Goal cockpit.
- `/goal pause` pauses after the current request.
- `/goal budget <N|none>` replaces or removes the token limit.
- `/goal edit <objective>` replaces the objective without resetting the run.
- `/goal resume [steering]` resumes, queueing steering before continuation.
- `/goal clear` removes Goal state while preserving transcript and artifacts.

A new Goal cannot replace an unfinished Goal or start while accepted Plan
implementation is preparing or retryable. Plan mode cannot start while the
session owns an unfinished Goal.

An accepted Plan may select Goal execution after Here/Current, Here/Fresh,
Here/Summary, Worktree/Fresh, or Worktree/Summary preparation. The prepared
target owns the resulting Goal and immutable accepted artifact. Construction
uses a deterministic objective that preserves the plan's outcomes, constraints,
acceptance criteria, and validation evidence as the completion contract without
parsing Markdown headings. The first ordinary Goal turn receives the prepared
context, resolved artifact path, full plan, and kickoff; later turns use the
small request-local Goal context and may reread the validated artifact.

The Plan-selected permission mode applies to the target session. A Worktree
target also inherits the source buffer's ordinary Goal budget, while the source
session's permission mode and Goal state remain unchanged. Derived artifact
authority exists only while the target Goal is unfinished and never alters
user grants.

The cockpit and status surface show only objective, status/reason, accounting,
elapsed time, and accepted-plan reference. Their redraws preserve the active
composer draft.
