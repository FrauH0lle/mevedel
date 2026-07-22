# Goals

A Goal is one durable objective attached to a session. It keeps the objective
in context across ordinary root conversation turns and continues while the
session is idle. There is no Goal phase machine: planning, implementation, and
review are ordinary model work.

## Durable state

The session sidecar stores a strict Goal record containing:

- a unique ID and free-form objective;
- `active`, `paused`, `blocked`, or `complete` status plus an optional reason;
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
one turn at canonical success or failure settlement.

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
- `/goal edit <objective>` replaces the objective without resetting the run.
- `/goal resume [steering]` resumes, queueing steering before continuation.
- `/goal clear` removes Goal state while preserving transcript and artifacts.

A new Goal cannot replace an unfinished Goal or start while accepted Plan
implementation is preparing or retryable. Plan mode cannot start while the
session owns an unfinished Goal.

The cockpit and status surface show only objective, status/reason, accounting,
elapsed time, and accepted-plan reference. Their redraws preserve the active
composer draft.
