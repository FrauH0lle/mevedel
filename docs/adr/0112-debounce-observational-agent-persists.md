# Debounce observational agent persists

Status: accepted

## Current decision

Acknowledged agent mutations remain synchronous: spawn registration, mailbox
append, RESULT publication, and settlement return only after their publication
commits. They refuse reentrant queueing. Tentative records are hidden from
delivery until that acknowledgement, so a nested WAIT cannot consume a RESULT
whose terminal transaction later rolls back. Observational activity changes and
explicit mailbox discards schedule one sidecar-only registry save per session, with
a two-second default debounce through the idle-transport scheduler.

One acknowledged mutation is asynchronous, for warm local sessions only:
delivering a background command's completion or monitor batch to its owner's
mailbox. The producer passes an acknowledgement, and the append commits as a
session-owned publication job whose callbacks run across event-loop turns. The
acknowledgement still means what it meant: it runs once, after the new head is
authoritative and adopted (success) or after a definitive rollback (failure).
Until then the records are tentative and hidden, and the producer keeps the
output it would otherwise have consumed. Every other acknowledged mutation, and
every cold, remote, file-backed or PID-lock session, keeps the synchronous
path above. [Sessions](../sessions.md#warm-local-asynchronous-mailbox-publication)
owns the job's phases, gate and recovery. Automatic warm-local root and agent
transcript checkpoints also use that job. Explicit saves and settlement still
wait synchronously for their commits.

While a job is accepted, its session's mutation gate is held. Mutations that
would change acknowledged state or freeze a sidecar -- background mailbox
arrivals, deferred terminal settlement, autosave, the observational registry
save -- queue as preparation continuations that run before they mutate or
capture anything, so no frozen sidecar can outlive a rollback and resurrect its
rolled-back mail, and no sidecar frozen before a commit can erase it. Queued
entries hold no snapshot. A caller that must be synchronous (agent mail, spawn
and activation, manual save, transcript writes, settlement publication or a
required-commit registry save) drains the gate first and
never reports queued work as committed. Transcript streaming and composer edits
are not gated.

Gate ownership is explicit, not ambient. A queued preparation runs with a
fresh lexical token, also stored in a session slot for exactly the duration of
that preparation, and the preparation receives it as an argument. Only a call
that passes that token may reenter the gate; there is no dynamically bound
exemption, so an unrelated timer or callback that fires during the preparation
inherits nothing and defers or drains like any other caller. The drain is
fail-fast where waiting cannot help: it signals instead of waiting when a
nested transport frame is in flight, when a preparation is running on the
stack, or when a synchronous publication on this stack owns the gate. Its
30-second limit is checked between event-loop pumps, so it bounds the number
of pumps, not the time a single blocking operation inside one may take.

Waking a waiter is the boundary where this matters. Its callback resumes
arbitrary tool code, so notifications (waiter wakes, root and agent wake
offers, and the reoffer after a vetoed close) never enter the mutation
preparation queue and never run under a preparation's token. Each session owns
one coalesced FIFO of pending notifications and at most one timer. A
notification runs at once only when the publication gate is idle (no job,
preparation or active publication), the transport is idle, and nothing is
already pending; otherwise it joins the FIFO, so a later acknowledgement never
overtakes an earlier one. The timer starts at 50 ms and doubles up to 500 ms
while the gate or transport stays busy, then flushes in order, and only while
the root buffer is live and the session is not stopping. Every callback starts
with fresh transaction observations, including an immediate notification.
Temporary stopping retains actual suppressed offers without a timer, so a
vetoed close re-arms the same FIFO once rather than inventing another wake.
An idle close schedules nothing. A close that proceeds cancels the timer and
discards the offers during teardown, without invoking them; the mail remains
durable.

Acknowledged registry and mailbox mutations publish only the sidecar once the
first root snapshot exists. `mevedel-session-checkpoint.el` owns the root fields
as of the last committed transcript: segment, turn count, prompt index, file
snapshots, chapters, and first/latest user previews. Ordinary metadata writes
project those committed fields rather than tentative live values. Terminal
agent settlement commits its transcript with its metadata.

Goal accounting, Plan decisions, and native-provider conversation identity are
independently durable control state, not root transcript projections. Goal
budgets account for incurred work and restore active Goals paused; Plan metadata
refers to already published hash-bound artifacts; provider conversation metadata
names external history and restores in-flight state as uncertain. These fields
may advance without claiming that the local root transcript has advanced.

Delivered mail is consumed in memory after successful transcript insertion,
without a synchronous save. Every record has a durable ID and its transcript
has a hidden receipt, allowing repeated delivery to be deduplicated. The owner
retains consumed records in a per-conversation ledger and merges them back into
ordinary sidecars until that receiving transcript commits. Transcript writers
capture an opaque candidate at byte freeze; only actual head adoption installs
it, including retry. A root checkpoint cannot retire an agent's consumption,
and arrivals or consumption after capture remain pending. File sessions write
the transcript before the matching sidecar and install after that sidecar write.
Settled ToolCall audit rows use the same per-conversation deferred-removal rule.

Queued sidecars are reprojected through the current committed owner before
publication. Retirement proofs survive only while publication work is active,
queued, or pending retry. This prevents an older queued sidecar from resurrecting
mail or ToolCall rows after their receipts move to an archived segment, without
retaining session-lifetime identity history. Restore and intentional replacement
seed a new owner epoch; stale candidates cannot install into it.

Preparation excludes overlapping transcript serializers, but ordinary sidecar
writes from save hooks are safe through the projection. Terminal settlement
still excludes nested writers while its tentative registry and RESULT state
must commit together. Failures unwind this exclusion. A queued or failed save
does not install its candidate merely because the save function returned.
Authority admission, sidecar capture, and publication share the existing
transaction observations; path and instruction I/O precede ancillary capture.
Otherwise a nested acknowledged arrival during a second recovery read could
be overwritten by an already-frozen sidecar. Segment transitions identify
their own adoption through the committed root projection, not unrelated head
movement from a nested mailbox publication.
Writers clear a buffer's modified flag only if its character-change tick still
matches the frozen bytes; delivery during target I/O remains dirty for the next
checkpoint rather than losing its unsaved receipt.

New retained transcript text and its transcript metadata share a strict
publication when the portable sidecar already exists. The session root supplies
that sidecar even for nested agents. Provider setup reuses the transcript saved
after frozen configuration was installed, without forcing another write.
Registry admission still commits before spawn acknowledgement.

Once terminal settlement has committed the answer, gptel's post-response hooks
run and their extra transcript checkpoint uses the existing conversation-save
debounce. A crash in that window can lose only post-commit hook changes, not the
acknowledged answer. Pending settlement still checkpoints synchronously, and
explicit saves and teardown flush deferred text normally.

The registry save does not save the root transcript, rebuild its prompt index,
or scan snapshots. Portable publication overlays the
sidecar while retaining other committed artifacts. A session without a committed
sidecar waits for its next critical commit. Synchronous commit absorbs a pending
save; active publication defers it; Emacs exit flushes retained transcripts
through their agent writer before pending registry saves and lease release.

Mid-turn checkpoints use the native auto-save path. Because Emacs auto-saves
only after input, a repeating timer drives that same path while any root or
agent request is in flight, and stops once none is. Warm local portable
checkpoints submit a preparation to the session-owned publication job. It
rechecks modified state, save failure and retained batches under the gate;
only then does the shared root or agent writer freeze the transcript and its
checkpoint candidate. One accepted checkpoint per buffer coalesces repeated
opportunities. Nil preparation is a successful no-op when an earlier save
absorbed the edit. An agent without a canonical transcript instead falls back
to its synchronous writer after gate release, without clearing the save-failed
cue for an unsaved transcript. The sidecar marker is last, carries the opaque checkpoint
candidate and is projected through the current owner. The candidate is
validated before the head commit and installed only after authoritative head
adoption, including reconciliation. Success runs the shared post-save finish,
clearing modified state only if no edit followed capture. Pre-head failure
retains the frozen batch as pending publication before reporting save-failed;
automatic checkpoints never blindly retry that recovery. Explicit retry
installs its candidate without repeating the failed acknowledgement.
Save-hook cancellation is caught at the asynchronous boundary, not allowed to
escape its timer and strand the gate. Before a batch exists it leaves modified
text and the save-failed cue without recovery bytes, permitting a later checkpoint.
Explicit abandonment deletes staged recovery and fails any unresolved job's
acknowledgement once; unlike an ordinary checkpoint failure, it never retains
that job's discarded batch again.
Close and exit drain accepted work before their synchronous save and lease release.
Already-pending recovery blocks teardown even when no job remains in flight.
Retained-agent buffers also query unpublished transcript state before a kill,
saving first and refusing failed or unresolved publication. Emacs exit uses the
existing session query, not a parallel agent query. If a caller bypasses the
buffer query and the kill hook cannot save, it preserves unpublished transcript
bytes in a temporary recovery file and reports the path. Lease continuations,
producer acknowledgements and private carrier cleanup catch cancellation at
their asynchronous boundary so a quit cannot strand a job or its successor.
The synchronous agent writer drains before checkpoint exclusion and capture,
not after freezing an overlapping job's tentative state.
Cold, remote and file-backed checkpoints remain synchronous.
[Sessions](../sessions.md) owns the persistence and recovery contract.

## Rationale and consequences

Acknowledgement promises durable delivery; an activity flavor does not. Recovery
settles all abandoned active flavors identically. A crash during debounce can
therefore retain a stale activity flavor or repeat an explicit discard. Mail
delivery remains at-least-once: failure before its consumption checkpoint can
redeliver mail, but a committed removal never outruns its transcript text.
A single timer avoids publishing each transient permission-blocked/running pair.

Diagnostic logs also append only their delta through one pinned target operation.
A crash can tear the trailing line; logs are observational and never resume
state. Replacing them atomically on every append was rejected for its copying
cost. The manual describes this limitation rather than treating logs as a
transactional journal.

## Decision history

All revisions belong to ADR 0112:

- **2026-10-20 asynchronous automatic transcript checkpoints:** a live trace
  of the 30-second checkpoint on a 1.6 MB session still blocked the editor
  for 280-314 ms after mailbox delivery became asynchronous. The auto-save
  preparation now shares the session publication job rather than calling its
  synchronous writer from the idle callback. Capture remains under the gate,
  not at scheduling time, and synchronous and asynchronous writers share the
  same capture and success-only finish. The persisted format, checkpoint
  projection, receipt retirement and explicit-save contract are unchanged.
  A compiled replay on a full `cp -a` copy of the real session (95 files,
  27,795,709 bytes; restored root 1,069,346 bytes, selected agent about 1.34 MB)
  measured six samples per mode, draining pending batch and coalesced-render
  timers before each edit, not every observer queue. Root synchronous heartbeat gaps
  (5 ms timer, through the acknowledgement wait and 12 ms afterwards)
  were 94.42-104.04 ms, versus 40.35-46.18 ms asynchronously; async submission
  took 0.08-0.11 ms and acknowledgement 105.45-118.50 ms. Agent gaps were
  81.11-122.33 ms synchronously (41.35 ms GC in the largest sample), versus
  21.67-57.44 ms asynchronously, with submission 0.07-0.30 ms and
  acknowledgement 89.42-210.48 ms. No paired async sample collected garbage.
  The carrier does not remove capture: a separate profile measured root
  preparation at 37.61 ms, including prompt indexing 15.57 ms and gptel state
  saving 13.20 ms, and agent preparation at 14.06 ms. The root still exceeds
  the 20 ms target. Repeated agent instrumentation also attributed a 116.90 ms
  no-GC gap to a restored-view batch callback (113.55 ms, including a 74.79 ms
  full-rerender reset). Its archived continuation prompt hit a cold cache: this
  is restore/first-render work, not checkpoint publication (a warm live probe
  took 0.5-0.9 ms). It can run during or after the acknowledgement wait.
  These copied-session timer measurements neither bound all visible stalls
  nor establish graphical input latency. Capture semantics are unchanged;
  the gain is event-loop time between target phases, not earlier durability.
  These samples precede the cancellation, abandonment and buffer-kill guard
  corrections, validated separately without another performance run.
- **2026-10-20 warm-local asynchronous mailbox acknowledgement:** a live trace
  of one background-command delivery on a local session spent about 98 of its
  135 ms in synchronous control-program waits, and the user observed the
  resulting stalls of roughly 120 ms per completion while typing. After the
  programs were merged (ADR 0101) a delivery still took about 120 ms, most of
  it the operations' own subprocesses, which merging cannot remove. Waiting for
  them from the event loop can: the editor runs between phases. The constraint
  that shaped the design is the one this ADR already records. An append that
  starts a commit and lets another save freeze the tentative queue can
  publish mail a failed first commit has just removed, so serialization sits
  before mutation and capture, not before the subprocess. Two alternatives were
  rejected. An Emacs Lisp thread does not fix ordering or acknowledgement, its
  dynamic bindings and transport-depth guards are thread-local, and the
  main-thread timers cannot see its remote operations. An append-only mailbox
  journal would change the persisted format and need framing, replay,
  deduplication and retention coordinated with transcript commits. Scope is
  deliberately warm local sessions: remote targets, cold materialization,
  file-backed and PID-lock sessions stay synchronous until separately
  converted, so no remote safety is inferred from local results. No persisted
  format changes. Measured after the final source freeze on one fixture:
  compiled modules, an isolated test
  home, a local portable session holding 6000 saved turns, one 4000-character
  execution result delivered per sample, six samples per mode, a 5 ms timer
  recording the largest gap between runs as the heartbeat, and a composer
  draft whose text and point were checked afterwards and preserved in all 19
  samples. The synchronous path held the editor 59.9-73.8 ms, with a heartbeat
  gap of 60.0-73.9 ms. Asynchronous submission returned in 0.22-0.45 ms and
  the acknowledgement arrived 75.4-120.1 ms after submission, with a largest
  heartbeat gap of 8.2-10.2 ms. An explicit drain issued immediately after
  submission waited a further 89.8-115.0 ms (submission 0.19-0.46 ms,
  acknowledgement 85.0-110.2 ms, heartbeat gap 7.9-10.8 ms). A 0.25-second
  drain limit on a deliberately stalled admission signalled at 255.6 ms in the
  single sample taken, with acknowledgement still pending until the held
  callback was released. Acknowledgement latency did not improve: it is about
  the same as, or a little longer than, the synchronous commit; the gain is
  that the editor runs meanwhile. These are compiled in-process measurements
  on one fixture with small samples, not graphical input latency or
  live-session measurements, and not a hard bound on arbitrary filesystem
  latency, sidecar size or GC. The 98 ms figure is an upper bound on
  the waiting removed, not a measured saving.
- **2026-10-19 unified committed checkpoint:** the full-save fallback added
  120–132 ms to each root delivery batch in compiled private-copy replays.
  A single owner now projects committed root fields and retains consumed mail
  and removed ToolCall rows until the receiving transcript actually commits.
  This replaces both forced root saves on acknowledgement and synchronous
  consumption checkpoints, without pretending live metadata is committed.
  Durable identities and hidden transcript receipts support at-least-once
  delivery; failed callbacks leave mail queued. A workflow callback can
  checkpoint its effects before returning, so process loss before its receipt
  becomes durable can still repeat that callback's RESULT as ordinary mail.
- **Consumption checkpoint ordering:** review found that debounced consumption
  could commit an empty mailbox before the injected transcript text reached disk.
  A crash then lost both copies. The fallback reused the existing conversation
  checkpoint, preserving native response boundaries and prompt-index updates;
  raw text plus a sidecar would leave those offsets stale. This deliberately
  spent one synchronous checkpoint per delivered batch to close the loss window.
  Agent delivery replaces its per-block saves with this one checkpoint.
- **2026-10-19 acknowledged-save experiment reversed:** a sidecar-only path
  reduced median compiled acknowledgement from 145.2 to 55.2 ms on a private
  1.79 MB/27-agent session copy. A real-disk restore at the synchronous turn
  commit/deferred save seam then reproduced turn 1 metadata against a partial
  turn 0 transcript. The same hole followed failed root-save preparation.
  Full acknowledged checkpoints were retained rather than adding a separate
  committed-root schema and special cases for history and checkpoint writers.
  Speedup measurements from the rejected narrow-save variant are not evidence
  for the current design. Reentrant hook testing additionally reproduced lost
  mail after a nested commit; preparation exclusion closes that independent gap.
  With full acknowledgements restored, compiled root enqueue/consume cycles on
  the private session copy add 120–132 ms per delivery batch versus deferred
  consumption (four alternating observations per variant, batch sizes 1/3/6).
  This is a measured durability cost, not a performance improvement.
- **2026-09-22 agent transition capture:** a 565-ms graphical heartbeat delay
  coincided with three publications and GC at agent completion. Startup had a
  similar burst. Five alternating real-storage lifecycle replays reduced startup
  publications from four to two and median time from 457 to 239 ms. Completion
  changed from two immediate publications to one, reducing its median from 245
  to 145 ms; the remaining post-hook checkpoint took about 100 ms separately.
  These measurements cover the agent lifecycle, not the entire parent tool
  pipeline or graphical input latency. The change combines initial text and
  metadata, removes forced re-saving of already configured text, and defers
  only changes after a successful terminal commit.
- **2026-08-25 profile:** 434 publication generations occupied 227 MB, peaking at
  21/minute. Publication accounted for roughly 30% of 8.6 GB allocations in a
  31-minute window; GC took 60% of CPU samples. The session had 763 permission
  decisions, each producing blocked/running observational saves. Debouncing
  replaced full synchronous saves for those unacknowledged transitions.
- **2026-08-26 follow-up:** deferred full saves still allocated over 2 GB and
  visibly rewrote the modified transcript every few seconds. Sidecar-only
  registry saves removed that repeated segment work.
- **September 2026 portable correction:** the initial change kept full saves for
  portable sessions, assuming byte comparison made unchanged saves free. A
  2h45m profile attributed 37% of all allocations to those saves: segment copies,
  artifact reads, and instruction serialization preceded comparison, while the
  changed registry prevented elision. Project sessions are portable, so the
  optimization had missed the default case. Portable registry saves now publish
  only the sidecar as well.
- **Diagnostic delta append:** a 5.2 MB telemetry log caused approximately
  10–14 MB base64 traffic per flush. Pinned append replaced complete-file
  read/rewrite while retaining parent and symlink proofs. The accepted tradeoff
  is a possibly torn diagnostic tail, not weaker critical publication.
- **Exit transcript flush:** a regression test showed that exiting inside a
  transcript's debounce window lost its latest text. The exit loop passed agent
  buffers to the root writer, which deliberately rejects them. Exit now flushes
  agent transcripts explicitly before root state and lease release; native Emacs
  auto-save also checkpoints modified conversations through the correct writer.
  Reading the immutable publication exposed a second gap: an agent write without
  a sidecar commit remained invisible to resume. These checkpoints commit the
  sidecar too, and root auto-save includes retained publication batches even
  when no transcript buffer remains modified.
- **In-flight checkpoints:** the sessions and agents manuals promised that
  native auto-save checkpoints modified conversations mid-turn, but Emacs runs
  `auto-save-hook` only after keyboard input. An unattended Goal or agent run
  receives none, so its conversations reached disk only at settlement. A timer
  started at request admission now runs the same coalesced, input-yielding
  checkpoint every `mevedel-session-checkpoint-interval` seconds until no
  request is in flight.
