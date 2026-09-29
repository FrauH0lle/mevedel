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
Transfer path resolution is read-only; protocol writers create their required
mailbox and fence directories explicitly.
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

Unchanged saves avoid a target transaction by comparing candidate bytes with
the captured manifest. Payloads are compared once per save; the resulting
changed-artifact selection supplies the publication. The sidecar is compared
before stamping `updated-at` and rebuilt only when a publication is needed.
Explicit forced saves still publish a marker. Pending recovery or retained
batches prevent comparison from bypassing their required transaction.

The fixed sidecar and ordinary session artifacts retain non-authoritative cache
copies. Root and canonical agent transcripts, file history, and instruction
snapshots skip duplicate fixed writes; their logical paths resolve through
publications or owned staging. Each fixed write costs an ownership proof and a
write program: the fixed copies of 60 instruction snapshots that no reader
opened took 0.9 s of a large Save As's 4.3 s. Old fixed transcript
files are ignored, including their timestamps, and are not deleted automatically.
Numbered agent compaction archives retain physical recovery copies before live
rewrites. A sidecar-marked transaction
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

The final September 23 pass found that root publication still encoded the same
transcript for comparison and staging. Root saves now freeze encoded bytes once
after save hooks. Together with forward-only property-range searches in tool
discovery, a paired byte-compiled replay of a captured 1.65 MB transcript reduced
median save time from 194 to 183 ms and profiler-reported temporary allocation
from 37.0 to 32.0 MB. The 11 target programs and ownership proofs remain intact.
Unicode and non-default line-ending tests verify unchanged authoritative bytes.
These are offline results, not a measurement of graphical typing latency.

The September 23 follow-up replay reproduced a roughly 566 ms pause when native
auto-save synchronously saved a root and two agents. Agent checkpoints published
transcript and sidecar separately. They now use the existing combined publication,
and native auto-save queues separate opportunities through the existing transport
scheduler. Each save remains synchronous and retains its durability checks;
settlement and exit still wait for their commits. The scheduler keys retries by
work identity, so separate buffers on one target cannot overwrite each other's
queued work, and waits for pending input between opportunities.

The same replay reduced five publications to three, 65 target programs to 39,
and temporary allocation from about 69 to 44 MB after also dropping duplicate
transcript copies and scanning fork records in place. These are offline paired
measurements; graphical input latency requires a rebuilt live capture. The fork
reader no longer caches only by character tick: restriction and provenance changes
must affect which records it returns.

The September 23 multi-agent replay found that direct agent publications did
not share the root save's bounded transaction scope across admission,
reservation and commit. The publication entry point now uses the existing
durability transaction helper. This removes repeated clock and recovery reads
within the call while retaining fresh lease-byte proofs, generation checks,
the one-second clock reuse bound and the same durable commit point. A storage
regression counts one target clock rather than four per direct publication,
requires a fresh observation on the next call, and checks authoritative bytes.

A September 22 multi-agent capture showed substantial allocation during saves.
The save admission check encoded and hashed the changed transcript, then the
publication filter repeated it. Sharing the changed-artifact selection inside
one save reduced a captured-transcript replay's median from 264 to 245 ms and
59.8 to 54.7 MB allocated bytes, without changing the 13 target programs or
publication ownership checks. These are isolated paired measurements; they do
not establish graphical input latency.

The simplification audit reproduced lease-status and decision reads creating
absent control directories through shared path helpers. Directory creation now
belongs to protocol writes, removing that hidden effect from observation.

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
