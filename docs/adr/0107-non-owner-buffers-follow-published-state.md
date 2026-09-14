# Non-owner buffers follow published state

Status: accepted

## Current decision

A read-only session buffer follows the lease owner's committed publications by
default. The rule applies both to a joining client and to a former owner that
has handed control away. Owner and non-owner are lease positions, not permanent
client roles.

Following advances by complete publications, never by streaming partial turns.
An unchanged head costs one lease observation and no artifact reads. A changed
head triggers validated publication and sidecar loading, staged transcript
installation, and session-state replacement. A locally modified buffer is left
alone; a background observer cannot discard the user's edits.

`mevedel-session-follow-published` can be set per buffer. Explicit refresh can
bypass that opt-out. Following does not require a pending control-transfer
request and grants no write authority. The [session manual](../sessions.md)
owns the user workflow and transfer procedures.

## Rationale and consequences

Committed publication is the shared consistency boundary. Observing partial
owner output would require another transport and state contract. Following
reuses the staging and installation path used by control adoption, without
acquiring the lease or enabling writes. It remains useful during a transfer:
the owner can publish while draining, and the requester can observe that state.

Default-on following trades a bounded lease observation for keeping an open
read-only conversation current. New artifacts are read only when the head
changes. Local edits and unavailable publications can prevent advancement;
following is not authority to resolve those conflicts.

## Decision history

ADR 0107 replaced static joined-session snapshots. The existing requester poll
returned immediately when no transfer request was outstanding, so an idle
joined buffer performed no target I/O and silently became stale after the
owner's next completed turn. Former owners had the same problem after granting
control. A common publication follower replaced both gaps. Head comparison
bounded the added idle cost without inventing a second publication protocol.
