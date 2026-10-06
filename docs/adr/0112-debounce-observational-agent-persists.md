# Debounce observational agent persists

Status: accepted

## Current decision

Acknowledged agent mutations remain synchronous: spawn registration, mailbox
append, RESULT publication, and settlement return only after their publication
commits. They refuse reentrant queueing. Observational activity changes and
mailbox consumption schedule one sidecar-only registry save per session, with
a two-second default debounce through the idle-transport scheduler.

New retained transcript text and dirty transcript metadata share a strict
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
scan snapshots, or read the artifact folder. Portable publication overlays the
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
settles all abandoned active flavors identically, and mail delivery already has
at-least-once semantics. A crash during debounce can therefore retain a stale
activity flavor or redeliver consumed mail without losing an acknowledged send.
A single timer avoids publishing each transient permission-blocked/running pair.

Diagnostic logs also append only their delta through one pinned target operation.
A crash can tear the trailing line; logs are observational and never resume
state. Replacing them atomically on every append was rejected for its copying
cost. The manual describes this limitation rather than treating logs as a
transactional journal.

## Decision history

All revisions belong to ADR 0112:

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
