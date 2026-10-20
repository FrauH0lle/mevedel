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

Serialized does not mean the editor must wait. For warm local portable sessions
an acknowledged mailbox delivery or automatic root/agent transcript checkpoint
runs as one session-owned publication job
(admission, freeze, reservation, generation, head commit, adoption), each phase
a pinned control program launched without waiting, and each continuation
scheduled from a timer after the process sentinel returns, never from the
sentinel. The job keeps the session's `publication-active-p` set for its whole
life, so renewal timers stay suppressed and cooperative transfer stays blocked;
it performs its own bounded lease proofs between phases and fails closed after
expiry or ownership loss. The commit point is unchanged: the lease head. A crash
before it leaves the old snapshot authoritative with nothing acknowledged; a
crash after it but before the acknowledgement leaves committed mail that
restore finds, and durable mail identities with hidden transcript receipts
keep delivery at-least-once. An uncertain head outcome is decided by reading
the authoritative head, not by guessing: the candidate head means adopt, the
prior head means roll back, anything else, or an unreadable head, retains the
job as pending publication for explicit retry or abandonment. Checkpoints
retain definitive pre-head failures as frozen recovery too; later automatic
opportunities do not retry them blindly. Their opaque checkpoint candidate
installs only after head adoption, and their success-only finish clears a
buffer's modified flag only if its tick still matches the captured bytes.
Explicit saves, settlement and teardown remain synchronous and drain accepted
work first, before transcript capture or checkpoint exclusion. Pending recovery
blocks teardown even without an in-flight job. Capture itself remains
main-thread Lisp under the mutation gate. Disposable per-buffer change proofs
reuse unchanged transcript structure, bounds, prompt units and audit spans;
they do not cache lease authority or replace the complete frozen payload.
Unknown edits or unresolved grammar dependencies fail closed to derivation.
Hidden canonical transcript buffers disable unused font-lock/JIT machinery;
an isolated compiled replay proved that JIT's late silent `fontified` property
write invalidated all prefix proofs on every checkpoint. Removing that one
hook, or disabling the modes through their APIs, restored suffix derivation
with identical authoritative bytes. This removes irrelevant storage work,
not the proof checks protecting against arbitrary hook edits.
The transcript gptel-enable boundary also undoes gptel's Org JIT registration,
which otherwise silently re-enables JIT during restore and agent activation.
Raw transcript inspection and gptel's temporary menu buffer stay unfontified;
normal presentation remains the rendered view's responsibility.
Cancellation is reported through the
asynchronous failure acknowledgement without escaping a timer; explicit
abandonment discards staging without retaining the discarded checkpoint again.
Retained-agent buffer queries save or refuse unpublished bytes; a bypassed
query's failed kill-hook save leaves a warned temporary recovery file. The
session exit query remains the single exit gate.
Remote publication, cold materialization and file-backed sessions are not
converted. [Sessions](../sessions.md#warm-local-asynchronous-mailbox-publication)
owns the contract; [ADR 0112](0112-debounce-observational-agent-persists.md)
owns the gate and acknowledgement semantics.

## Decision history

Phase B of the same work moved the remaining main-thread cost of a warm-local
transcript checkpoint out of its freeze: freeze captures native characters and
frozen state only, one staging write per artifact goes straight from the string
with an explicit coding system, a control-program `file-hash` digests the
staged files, and a selector over the frozen snapshot prunes unchanged payloads
and stamps the small sidecar before the lease reservation. The commit point and
the lease proofs are unchanged. This removes the eager encoded transcript
payload and its main-thread SHA-256, not the staging write's encoding or
allocation. A compiled-copy replay of the 1,069,346-byte real root and a
3,504,146-byte synthetic root reduced preparation medians from
34.77/34.85 and 78.14/77.37 ms to 9.07/10.13 and 10.55/11.89 ms
(tail/new turn). Twenty non-GC latency samples stayed below 20 ms, largest
17.94 ms; four samples with observed GC reached 51.90-61.05 ms, and a
separate instrumented run placed a 40.35 ms GC inside preparation. The hard
20 ms goal remains unmet. ACK medians increase 32.33-37.16 ms versus prefix
reuse alone because the digest adds worker/idle handoffs; that latency cost
buys shorter non-GC blocking. Allocation is not reduced overall, and native
encoding remains on the main thread. The immutable capture and staging encoding
are the remaining transcript-sized copies; a byte-checked frozen-buffer region
write still allocates that encoding, so it showed no low-risk copy reduction.
[Sessions](../sessions.md#warm-local-asynchronous-mailbox-publication)
records the full comparison and limitations. The figures below predate this
phase split.

The 2026-10-20 follow-up found that the 30-second automatic checkpoint still
blocked the editor for 280-314 ms on a live 1.6 MB session. Scheduling a
synchronous save from idle transport does not make that save non-blocking.
Warm-local automatic checkpoints now use the same continuation carrier as
mailbox publication, with shared capture/finish logic and retained frozen
recovery on failure. No persisted format or remote publication contract changes.
On a compiled full-copy replay of that session, six samples per mode, draining
pending batch/coalesced-render timers before each edit, reduced root heartbeat
gaps from 94.42-104.04 ms to 40.35-46.18 ms;
asynchronous submission took 0.08-0.11 ms and acknowledgement 105.45-118.50 ms.
Agent gaps were 81.11-122.33 ms synchronously and 21.67-57.44 ms asynchronously,
with acknowledgement 89.42-210.48 ms. Main-thread capture remains material
(37.61 ms root, 14.06 ms agent in a separate profile), so this is not a 20 ms
bound. Repeated instrumentation separately found a 116.90 ms no-GC gap in a
113.55 ms restored-view batch callback during the acknowledgement wait. That
callback resolved an archived continuation prompt on a cold cache: it is a
restore/first-render cost, not checkpoint publication; warm live probes took
0.5-0.9 ms.
[ADR 0112](0112-debounce-observational-agent-persists.md) records the fixture,
GC caveat and phase attribution; these are not graphical latency measurements.
The checkpoint samples precede the cancellation, abandonment and buffer-kill
guard corrections, which were validated separately without remeasurement.

On 2026-10-20 the synchronous waiting for target programs in a background
completion's publication was measured at about 98 of 135 ms on a local session
(see [ADR 0101](0101-carry-control-operations-as-one-pinned-program.md)), the
source of the roughly 120 ms stalls observed while typing. The assumption that
serialized publication requires the caller to wait turned out not to hold:
serialization is a property of ordering and the lease, which a continuation
chain preserves. A compiled in-process rerun after the checkpoint preparation
changes (six samples, one synthetic local portable session of 6000 saved turns)
held the editor 63.907-70.177 ms synchronously; the asynchronous job returned its
submission in 0.196-0.460 ms, acknowledged after 105.028-115.540 ms, and left a
largest heartbeat gap of 6.533-7.159 ms. Earlier async ranges were 8.2-10.2 ms
gap and 75.4-120.1 ms ACK, so no blocking regression was observed on this
workload; acknowledgement is not earlier. The original recipe collects garbage
before each sample and none contained GC; possible independent verifier load
was not excluded. These are not live-session or graphical measurements, nor a
hard bound on arbitrary filesystem latency, sidecar size or GC.

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
