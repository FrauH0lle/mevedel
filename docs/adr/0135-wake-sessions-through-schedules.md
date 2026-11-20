# Wake sessions through schedules

Status: accepted.

## Decision

mevedel can act later through one concept, the **schedule**: a due time plus a
note or prompt with exactly one target. A **wake-up** targets the session
that created it: "check CI in 20 minutes and tell me only if it failed" makes
the conversation continue itself when the time comes. A **workspace
schedule** targets the workspace: "every weekday at 07:00, triage new issues"
starts a fresh session for each run.

- **Interface.** One `Schedule` model tool with an `action` of `create`, `list`,
  `change` or `cancel`. `create` takes `when` (a duration such as `20m` or a local
  time `YYYY-MM-DD HH:MM` for one wake-up, `every 20m` or a 5-field cron
  expression in local time to repeat), a note the woken turn receives, and an
  optional `expires`; `change` takes the same fields for a pending wake-up or a
  workspace schedule, validated the same way. The tool is offered to root requests only; a child
  agent asks the root, so ownership, cost and notification stay with the root
  conversation.
  Wake-ups need no approval card: a wake-up runs under the session's current
  permissions, so it grants nothing the conversation does not already hold.
  Invalid timing, a time not in the future, an expiry not after the time and a
  51st pending wake-up are tool errors at creation, not failures at fire time.
- **Model and effort per wake-up.** `create` takes an optional `model` and
  `effort`, validated at creation like an Agent's. They travel in the `WAKE_UP`
  record's metadata and become the woken turn's request-local model policy
  (the pending request context a skill uses), so the session's saved selection,
  preset and permission mode are unchanged. A mixed batch keeps the session's
  selection rather than guessing which override wins.
- **Recurring timing.** An interval has a one-minute minimum and its next run
  is timed from the actual firing, so missed runs never catch up: a laptop that
  slept through five runs receives one, annotated as overdue. Cron supports
  values, `*`, ranges, steps and lists with cronie's day rule: a day field
  starting with `*` (including `*/2`) is unrestricted, and when neither is, a
  day matching either fires. Matching cronie rather than the stricter POSIX
  wording keeps expressions copied from crontabs meaning what they meant
  there. Names, `L`, `W` and `?` are rejected rather than half-supported. A
  recurring wake-up expires 7 days after creation by default and never later
  than 30, so a forgotten loop cannot run and spend indefinitely; the firing
  whose next run would pass the expiry records it as expired, so it does not
  hold one of the 50 pending slots until then. A persisted entry loads only
  with timing creation would accept.
- **Daylight saving.** Next-run calculation is a pure function of the timing,
  a reference time and the local zone. A local time in a skipped hour fires at
  the first valid minute after the gap; a local time in a repeated hour fires
  once, at its first occurrence. One-shot local times and cron follow the same
  rule, and fire times are stored as absolute seconds.
- **Store as truth.** The session's persisted state holds its wake-ups
  (`:schedules`, sidecar format v0.5.16, added by an explicit migration step).
  Each entry keeps its timing, the absolute due time in seconds, the note, an
  optional absolute expiry and a status. Creating or cancelling commits the
  sidecar before the tool returns and restores the previous store on failure.
- **One timer.** An Emacs-wide scheduler tracks the open root buffers whose
  sessions hold pending wake-ups and arms one timer for the earliest due time.
  Every create, cancel, firing, resume and buffer kill re-arms it; with nothing
  pending no timer exists. Every decision reads one injectable clock,
  `mevedel-schedule-clock`, so tests advance time without sleeping.
- **Delivery through the mailbox.** A due wake-up becomes one `WAKE_UP` record
  in the root mailbox, carrying the note, `scheduled_at`, `fired_at` and, at
  least a minute late, `overdue_by`. Its removal from the store and the record
  share one session commit, so a persisted firing is never both delivered and
  still pending. The record wakes an idle root through the existing
  background-event path, with all of its holds: it waits for a running turn, a
  composer draft, pending decisions, queued or paused follow-ups and recovery
  blockers, and coalesces with other unread mail into one turn.
- **Background event, not a Goal turn.** The woken turn uses the fixed
  background-event prompt, never the Goal continuation text. It neither
  resumes a paused or blocked Goal nor counts toward an active Goal, even
  when the wake-up shares the turn with other unread mail.
- **Expiry and lateness.** A firing past its expiry costs no model turn. A late
  but unexpired firing is delivered once with its overdue annotation; missed
  repeats appear as a skipped span rather than a burst of catch-up turns. The
  session keeps the 20 most recent terminal entries (expired, failed or skipped).
- **Run failures.** Attribute a wake-up's outcome to the background request
  admitted for its mail, not the next turn to settle in the session. In
  particular a user who reads that mail is not responsible for running the
  wake-up. A failed run always notifies. Its third consecutive failure pauses
  a recurring schedule and announces the pause in the same notice; success or
  an explicit resume resets the count. This keeps a broken job from spending
  indefinitely without sending two alerts for one failure.
- **Sessions that are not open.** An Emacs-wide index under `mevedel-user-dir`
  names every saved session holding a pending wake-up, one file per session.
  It is a cache: once Emacs is idle after startup, `mevedel-schedule-restore` reads each indexed
  session's committed sidecar, drops only entries proven stale (session
  directory provably missing, or a readable sidecar with no pending wake-up),
  keeps unreadable ones for the next start, and schedules the rest. Closing a
  session keeps its
  wake-ups scheduled the same way. When one is due, the sidecar is read again
  and the session is resumed headlessly through the ordinary restore, under
  `inhibit-interaction` and without a window, with an echo-area notice; the
  resumed buffer then fires as an open one does.
- **Exactly one firing.** The session lease decides which Emacs fires. A
  headless resume that finds another client holding the lease closes its
  read-only buffer and leaves the session to that holder; resuming with the
  lease, or regaining it through a control transfer, arms the wake-ups. A
  wake-up never fires in two conversations: forks start without wake-ups, Save
  As durably clears the original's wake-ups and index entry before it writes
  the copy that carries them (a failed clear stops Save As; a later failure
  gives them back), and Rewind and Restore keep the live session's wake-ups
  rather than an earlier head's. Every handover is at most once: a crash can
  lose a wake-up, never duplicate it, as the firing commit already ensures.
- **Workspace schedules.** `create` with `target` `workspace` stores a name,
  the run prompt, the same timing forms, an optional expiry (no default), a
  base preset (default `mevedel-implement`) and one implementation policy in
  the strict shape of accepted plans and Goals. At most 50 are active or
  paused per workspace. Creating or changing one always goes through a
  `schedule` permission card, also in Full Access and never settled by
  automatic review, showing everything a run will use and flagging a mode
  above the creating session's (or, for a change, the stored one). The
  approved mode is not capped by the creating session's mode. Deleting needs
  no card.
- **Workspace store and runs.** The workspace state holds each schedule and
  one record per run, keyed by schedule and scheduled time. Every change runs
  as one target program under a lock on the store directory that proves the
  bytes it read before writing. A firing exclusively creates its run record
  in the program that moves the schedule to its next time, so exactly one
  Emacs fires each run and records it before the session starts. A run past
  the expiry is recorded `expired`; one due while the previous run still works
  is recorded `skipped`; otherwise a new saved session named after the
  schedule and run time starts with the approved preset, model, effort and
  permission mode as its own selection, so every turn in it runs under them
  until someone changes them there, and gets the prompt as a quiet first
  turn, without a window. A request-local override was rejected because later
  background turns in the same session (wake-ups, Monitor and Goal turns)
  fell back to whatever the session had selected. A session that cannot
  start is recorded as a setup error under the
  same key. Run sessions are ordinary sessions under automatic cleanup.
- **Workspace restore.** An index under `mevedel-user-dir`, separate from the
  wake-up index, names workspaces with active schedules; startup restore arms
  them and, like the wake-up index, prunes only entries proven stale: the
  workspace is provably gone, or its readable store holds no active schedule
  on a confirming re-read after the entry is deleted.
- **Quiet outbox.** A turn's origin decides whether its completion pushes. A
  user-started turn keeps its completion push to the session's subscribers,
  and to the workspace lobby when the session is shared. Turns nobody started
  (wake-ups, workspace schedule runs, Monitor and background-command wakes,
  Goal continuations) finish without one. The agent reaches the user
  deliberately through `Notify`: one line, recorded in the transcript and
  delivered locally and as a content-free push. An identical line from the
  same session, or from one schedule's runs, is dropped for 24 hours, and a
  notice adds to a user turn's completion push rather than replacing it. A
  failed or paused wake-up or run, a run that cannot start, and a terminal
  Goal always notify. The workspace lobby takes push subscriptions from full
  and owner links, keeps them across restarts, and wakes for these notices
  from every session of the workspace, shared or not.

## Rationale and consequences

A schedule is the missing timer of an always-on agent: events already reach
an open session through the mailbox (agent mail, Monitor events, background
command completions), but nothing could make it act later. Reusing the mailbox
and its background-event wake keeps the gates users already rely on, keeps the
note out of user-role prompts, where it could pose as the user's instruction
to the permission reviewer, and needs no second dispatch path.

One timer for the earliest wake-up, rather than one per wake-up, keeps re-arming
and teardown in one place and satisfies "idle means idle": nothing polls, and
an Emacs with no pending wake-ups has no timer. Emacs timers are absolute, so a
timer due during system sleep runs on wake, which is what makes the overdue
annotation meaningful. A pending wake-up therefore does not hold the sleep
inhibitor. Inbound webhooks were left out: a polling Monitor serves them.

Delivering the note as a queued follow-up was rejected because follow-ups are
user input and run first; a background notification must not acquire user
authority or overtake the user's own queue. A deferred tool call that ends the
turn and resumes from a later answer was rejected because it ties a provider
request to an arbitrary delay and cannot survive the session closing.

Committing the firing with its mailbox record means a crash between the two
cannot lose the wake-up or deliver it twice in this Emacs. A failed commit keeps
the wake-up pending and retries that session after a minute rather than
spinning on an immediately due timer.

The index exists because a closed session's store is only findable by
scanning every workspace, and Emacs does not know at startup which
workspaces exist. It holds nothing the stores do not: reconciling against
them makes a stale entry (a crash between the firing commit and the index
update, a cancelled or deleted session) harmless. One file per session,
rather than one shared list, lets two Emacs instances sharing a user
directory update it without a read-modify-write race. Restore reads each
indexed sidecar once after startup so stale entries never arm a timer; for a
session on a remote target that read is a network round trip, so restore
waits for the first idle moment instead of lengthening startup. In a batch
measurement with 20 local indexed sessions it took about 220 ms, which
`mevedel-install` no longer spends; a wake-up already due fires at that idle
moment.
Pruning deletes an entry and then reads the store again, recreating the entry
when a wake-up appeared: the lease holder commits before it checks its entry,
so a wake-up added during pruning is never left without one. This needs no
lease, which a startup restore of many sessions would otherwise acquire one by
one.

Resuming headlessly rather than firing from the stored sidecar keeps one
firing path: the resumed buffer acquires the lease, runs the same mailbox
commit and background-event wake as an open session, and stays open for the
turn. A resume-only restore, where a wake-up waits until its session is
opened, was rejected because a server restarted nightly would never fire a
wake-up nobody reopens. A lease miss does not retry: the
holder has the session open and fires itself, and retrying would resume the
session every minute while it stays held. The host restart predicate is
unchanged; pending wake-ups survive restarts instead of holding them.

The daylight-saving rule deliberately differs from Claude Code's scheduler,
which skips a time that falls in a gap: a daily 02:30 job silently misses the
spring-forward night, and a pinned one-shot in the gap can move a year ahead
to the next date where that wall time exists. A user who asked for 02:30
expects the work done that night, so firing at the gap's end keeps the run
and its day, at the cost of running half an hour "early" in wall terms on one
night a year. Firing a repeated wall time only once matches what a calendar
entry means; firing twice would duplicate work with no request for it.

A workspace schedule grants standing authority to work nobody watches, so its
card cannot be skipped by a permissive session: Full Access means the user
trusts this conversation's actions, not that it may create unattended jobs.
Capping a schedule's mode at the creating session's was rejected because the
user may deliberately want Full Access for unattended work from a cautious
session; the card shows the mode and flags the escalation instead. Each run
gets a new session rather than continuing one, so context stays lean and
continuity comes from memory and the agent's notes.

Pushing after every finished turn reached the user too often: thirty
overnight Goal turns meant thirty pushes, and the agent had no way to stay
quiet when nothing happened. Only the agent knows whether a result matters,
so automatic turns are quiet and `Notify` is how the agent speaks up. The
rule keys on who started the turn, which the harness knows, rather than on
judging content. A user-started turn keeps its push because the user is
waiting for that answer, and `Notify` in it adds rather than replaces so
expectations for one's own turns do not change. Failures cannot rely on the
agent, since a failed turn may never reach `Notify`, so the harness sends
them itself. The 24-hour duplicate filter stops a polling loop from
repeating "CI still red" while letting a changed message through. Lobby
subscriptions outlive the session rooms whose subscriptions died with them,
so a restarted server can still reach the user.

Single firing for workspace schedules uses an exclusive run record instead of
a renewable workspace holder lease. A holder lease must be renewed while idle
and taken over when its holder sleeps; the run record needs no renewal.
It identifies the claimant so a restart can distinguish a proven death
from a possibly suspended client. A later firing can retire a claim that
never launched: the launcher checks the same record before submitting work.
The same store lock serializes edits and firings. After launch, overlap
across Emacs instances relies on first-request settlement and the run
session's own lease, since only the owner sees the active request.
