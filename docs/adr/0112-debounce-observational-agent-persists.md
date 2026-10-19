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
agent request is in flight, and stops once none is.
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
