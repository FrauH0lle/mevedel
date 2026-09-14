# Store durable sessions with their execution target

Status: accepted

## Current decision

Durable session state stays with its workspace on the execution target so
another compatible client can resume the same conversation without a
client-local mirror or target-to-cache mapping.  Transient process spools,
publication batches, media copies, and pending recovery may be staged locally,
but durability-critical turns are not reported as published and the next turn
cannot start until their remote transaction succeeds.

A renewable, generation-based session lease gives one client mutation
authority while allowing other clients to inspect the last published state.
Every ownership claim exclusively creates the next generation and activates
only after validating its unchanged predecessor, so a stale renew or release
cannot overwrite or delete a newer owner.  Validation covers the record's
shape and its timestamps: a record whose renewal or expiry is not a finite
nonnegative number carries no authority, because an infinity never expires
and a NaN fails every comparison it is put to.  Complete lease decisions bypass
remote file caches so external generations cannot remain invisible.  Each
generation preserves either nil or a validated
`.publications/.../manifest.el` head.  Only the exact current publishing
generation, with the expected previous head, may replace it, and the
exclusive creation of the next generation -- not the record replacement --
is the atomic step that elects a winner.

Fixed session files are non-authoritative caches.  A sidecar-marked transaction
merges retained and current session-local artifacts in order, writes unique
immutable copies and their SHA-256 values below `.publications/`, writes the
manifest last, and commits only by changing the lease head.  Readers therefore
observe the complete old or new logical snapshot; they validate the manifest
and sidecar eagerly and verify other artifact bytes only when selected.  A
replacement marker starts from an empty logical snapshot.  Portable Save As
materializes the parent's allowlisted logical artifacts without copying its
manifest or control history, then the rewritten child sidecar performs that
child's first durable commit. Adoption transfers the already-owned child lease into the live session before
releasing the parent path.

Serialized publication uses a bounded publishing lease synchronously renewed
before and checked after every artifact rather than target I/O from timer
callbacks.  Pre-commit failure retains one local retry transaction.  A
successful head commit is terminal even if later lease normalization
fails, avoiding republishing already committed bytes.  Expired publishing
takeover warns that a write may still be in flight and requires confirmation
that the prior client is stopped. Frozen journal checkpoint recovery is a
separate storage-only operation: it may fence an expired ordinary lease without
resuming the session, running tools, or changing the publication head. Requiring
interactive conversation takeover here would strand completed evidence after
a crash. Recovery still refuses publishing leases, unsettled mutation, live
owners, and reserved control transfers, and releases its bounded reservation
before inference. See [ADR 0117](0117-publish-journal-results-from-fenced-outcomes.md).
Publication collection retains settled-turn heads, recent generations, and
referenced artifact generations, as specified by
[ADR 0072](0072-make-rewind-in-place-undo.md). There is no read-pin protocol.

Rebinding through a different client-specific TRAMP spelling uses durable
workspace identity.  A changed target incarnation remains an unacknowledged
observation while session resource grants are revoked, then a sidecar marker
atomically publishes the replacement identity with empty session resource
authority. Workspace resource grants remain configuration rather than incarnation-bound
session authority. Only a successful marker acknowledges the replacement; failure blocks the next
request for explicit publication recovery.  This accepts remote-write latency,
unavailability, and immutable-snapshot storage growth in exchange for portable,
co-located session history, and requires serialized publication rather than
asynchronous callbacks writing directly through TRAMP.

## Decision history

An injected adoption-time acquisition failure
showed that reacquiring after live mutation could leave neither parent nor
child fully bound.  Adoption therefore verifies and transfers the already-owned
child lease into the live session before releasing the parent path.

The fence originally also emptied the workspace store's resource
grants; that was dropped on 2026-09-04 after a routine reboot wiped grants
committed to version control and shared with a second machine.  The workspace
store is configuration, not incarnation-bound authority.

The original record omitted publication collection. Current collection retains
settled-turn heads, recent generations, and referenced artifacts under ADR 0072;
it does not introduce read pins. Frozen journal checkpoint recovery also gained
a storage-only expired-lease path so completed evidence is not stranded behind
interactive conversation takeover, while publishing leases and unsettled mutation
remain refused.
