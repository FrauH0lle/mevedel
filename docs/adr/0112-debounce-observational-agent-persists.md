# Debounce observational agent persists

Status: accepted

## Current decision

Acknowledged agent mutations remain synchronous: spawn registration, mailbox
append, RESULT publication, and settlement return only after their publication
commits. They refuse reentrant queueing. Observational activity changes and
mailbox consumption schedule one sidecar-only registry save per session, with
a two-second default debounce through the idle-transport scheduler.

The registry save does not save the root transcript, rebuild its prompt index,
scan snapshots, or read the artifact folder. Portable publication overlays the
sidecar while retaining other committed artifacts. A session without a committed
sidecar waits for its next critical commit. Synchronous commit absorbs a pending
save; active publication defers it; Emacs exit flushes retained transcripts
through their agent writer before pending registry saves and lease release.
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
