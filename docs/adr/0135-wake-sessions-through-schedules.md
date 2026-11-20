# Wake sessions through schedules

Status: accepted.

## Decision

mevedel can act later through one concept, the **schedule**: a due time plus a
note or prompt with exactly one target. The implemented target is the session
that created it, a **wake-up**: "check CI in 20 minutes and tell me only if it
failed" makes the conversation continue itself when the time comes.

- **Interface.** One `Schedule` model tool with an `action` of `create`, `list`
  or `cancel`. `create` takes `when` (a duration such as `20m` or a local
  time `YYYY-MM-DD HH:MM`), a note the woken turn receives, and an optional
  `expires`. The tool is offered to root requests only; a child agent asks the
  root, so ownership, cost and notification stay with the root conversation.
  Wake-ups need no approval card: a wake-up runs under the session's current
  permissions, so it grants nothing the conversation does not already hold.
  Invalid timing, a time not in the future, an expiry not after the time and a
  51st pending wake-up are tool errors at creation, not failures at fire time.
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
  background-event trigger, never the Goal continuation text, so it neither
  resumes a paused or blocked Goal nor counts as a continuation. Root turns
  started while a Goal is active are still charged to that Goal, as every root
  turn is.
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

Restoring wake-ups of sessions that are not open, recurring timing and
workspace targets are outside this decision.
