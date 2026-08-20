# Store durable sessions with their execution target

Status: accepted

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
cannot overwrite or delete a newer owner.  Complete lease decisions bypass
remote file caches so external generations cannot remain invisible.  Each
generation preserves either nil or a validated
`.publications/.../manifest.el` head.  Only the exact current publishing
generation, with the expected previous head, may compare-and-set a new one.

Fixed session files are non-authoritative caches.  A sidecar-marked transaction
merges retained and current session-local artifacts in order, writes unique
immutable copies and their SHA-256 values below `.publications/`, writes the
manifest last, and commits only by changing the lease head.  Readers therefore
observe the complete old or new logical snapshot; they validate the manifest
and sidecar eagerly and verify other artifact bytes only when selected.  A
replacement marker starts from an empty logical snapshot.  Portable Save As
materializes the parent's allowlisted logical artifacts without copying its
manifest or control history, then the rewritten child sidecar performs that
child's first durable commit.  An injected adoption-time acquisition failure
showed that reacquiring after live mutation could leave neither parent nor
child fully bound.  Adoption therefore verifies and transfers the already-owned
child lease into the live session before releasing the parent path.

Serialized publication uses a bounded publishing lease synchronously renewed
before and checked after every artifact rather than target I/O from timer
callbacks.  Pre-commit failure retains one local retry transaction.  A
successful head compare-and-set is terminal even if later lease normalization
fails, avoiding republishing already committed bytes.  Expired publishing
takeover warns that a write may still be in flight and requires confirmation
that the prior client is stopped.  Immutable publication generations remain
until session-directory cleanup; v1 deliberately has no garbage collection or
read-pin protocol.

Rebinding through a different client-specific TRAMP spelling uses durable
workspace identity.  A changed target incarnation remains an unacknowledged
observation while exact grants are revoked, then a sidecar marker atomically
publishes the replacement identity with empty exact session authority.  Only a
successful marker acknowledges the replacement; failure blocks the next
request for explicit publication recovery.  This accepts remote-write latency,
unavailability, and immutable-snapshot storage growth in exchange for portable,
co-located session history, and requires serialized publication rather than
asynchronous callbacks writing directly through TRAMP.
