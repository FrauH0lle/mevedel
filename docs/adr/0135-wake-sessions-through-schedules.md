# Wake sessions through schedules

Status: accepted.

## Decision

mevedel can act later through one concept, the **schedule**: a due time plus a
note or prompt with exactly one target. The implemented target is the session
that created it, a **wake-up**: "check CI in 20 minutes and tell me only if it
failed" makes the conversation continue itself when the time comes.

- **Interface.** One `Schedule` model tool with an `action` of `create`, `list`
  or `cancel`. `create` takes `when` (a duration such as `20m` or a local
  time `YYYY-MM-DD HH:MM` for one wake-up, `every 20m` or a 5-field cron
  expression in local time to repeat), a note the woken turn receives, and an
  optional `expires`. The tool is offered to root requests only; a child
  agent asks the root, so ownership, cost and notification stay with the root
  conversation.
  Wake-ups need no approval card: a wake-up runs under the session's current
  permissions, so it grants nothing the conversation does not already hold.
  Invalid timing, a time not in the future, an expiry not after the time and a
  51st pending wake-up are tool errors at creation, not failures at fire time.
- **Recurring timing.** An interval has a one-minute minimum and its next run
  is timed from the actual firing, so missed runs never catch up: a laptop that
  slept through five runs receives one, annotated as overdue. Cron supports
  values, `*`, ranges, steps and lists with the classic rule that a day
  matching either restricted day field fires; names, `L`, `W` and `?` are
  rejected rather than half-supported. A recurring wake-up expires 7 days
  after creation by default and never later than 30, so a forgotten loop
  cannot run and spend indefinitely.
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
- **Expiry and lateness.** A firing past its expiry costs no model turn: the
  entry stays in the store as `expired` until cancelled, and the 20 most recent
  expired entries are kept. A late but unexpired firing is delivered once with
  its overdue annotation; the model judges whether it still matters.
- **Sessions that are not open.** Closing a session stops scheduling its
  wake-ups without removing them; resuming it with the session lease re-arms
  them. A wake-up never fires in two conversations: forks start without
  wake-ups and Save As moves them to the new session.

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
annotation meaningful.

Delivering the note as a queued follow-up was rejected because follow-ups are
user input and run first; a background notification must not acquire user
authority or overtake the user's own queue. A deferred tool call that ends the
turn and resumes from a later answer was rejected because it ties a provider
request to an arbitrary delay and cannot survive the session closing.

Committing the firing with its mailbox record means a crash between the two
cannot lose the wake-up or deliver it twice in this Emacs. A failed commit keeps
the wake-up pending and retries that session after a minute rather than
spinning on an immediately due timer.

The daylight-saving rule deliberately differs from Claude Code's scheduler,
which skips a time that falls in a gap: a daily 02:30 job silently misses the
spring-forward night, and a pinned one-shot in the gap can move a year ahead
to the next date where that wall time exists. A user who asked for 02:30
expects the work done that night, so firing at the gap's end keeps the run
and its day, at the cost of running half an hour "early" in wall terms on one
night a year. Firing a repeated wall time only once matches what a calendar
entry means; firing twice would duplicate work with no request for it.

Restoring wake-ups of sessions that are not open and workspace targets are
outside this decision.
