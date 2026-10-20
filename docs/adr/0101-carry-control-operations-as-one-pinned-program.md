# Carry control operations as one pinned program

Status: accepted

## Current decision

One target process carries a program of control-filesystem operations. Every
operation opens its own parent descriptor and proves its physical location with
Bash `cd -P -e` and `PWD`. Symlinked parents/leaves and redirected pathnames fail
closed. Single-operation wrappers use the same program entry point.

Programs stop at the first non-optional unsuccessful operation and report the
remaining operations as skipped. Results use `ok`, `conflict`, `absent`,
`mismatch`, `failed`, and `skipped`; wrappers retain their operation-specific
nil-versus-error behavior. Target checks fail explicitly because the dispatcher's
status capture suppresses shell `errexit` inside an operation.

A `verify` followed by a write is a precondition, not compare-and-set. Without
an explicit lock, another client can interleave between operations. Exclusive
`create` remains the generation-election primitive. Callers requiring guarded
mutual exclusion may supply a pinned lock directory; the program holds its
`flock` across all operations. Journal settlement and curated-memory mutation
use that boundary under [ADR 0117](0117-publish-journal-results-from-fenced-outcomes.md).

Creating a directory with missing parents takes one program. Each ancestor
below the root is an optional creation through its own pinned parent. An
existing ancestor reports a conflict and does not stop the program.

`verify-latest` rejects a claim candidate when the pinned parent contains a
lexicographically newer leaf with the requested suffix. Used inside the claim
lock before exclusive creation, it prevents stale acquirers from recreating
pruned generations. `delete-empty-directory` uses native `rmdir`; nonempty
directories remain untouched. Domain collectors choose retention and references.

`hash` and `verify-hash` hash a regular leaf on the target through a descriptor
proved to be that leaf, so large files are compared without transferring
their bytes. `capture-blob` copies a named source into a content-addressed blob
in the pinned parent: the copy is proved against its SHA-256 name before an
exclusive link publishes it, and an existing blob with matching bytes makes
capture idempotent. `restore-blob` copies a blob beside the pinned leaf, proves
the copy against the blob's name, sets its mode and renames it into place. The
source paths of both are not pinned; the content proof is their guarantee, so
a redirected path can only fail. Callers state the expected current leaf as
an earlier operation, as with every other mutation.

Requests use five fields per operation and NUL framing. Arbitrary content and
response bytes use base64; numeric limits, modes, and deadlines use validated
digit strings. On a local target, every `write` and `create` payload travels
instead as a private staged file the editor writes, or copies from a
file-backed payload, in the destination's nearest existing ancestor: a
directory on the same filesystem as anything the program creates below it.
The `write-staged` and `create-staged` verbs prove the destination's parent
exactly as their base64 counterparts do, then rename or link the staged file
into it. The program names that file relative to the pinned parent, with one
`../` per missing level, so it resolves through the opened directory's real
ancestors: a parent pathname swapped after the proof cannot redirect it.
Staged files a stopped program did not consume are removed afterwards.
Remote targets keep the base64 carriers. Diagnostic bytes are encoded through a separate pipe and returned
in a distinct trailing record. A killed process without that record reports
its raw captured failure output.

ASCII fields travel as arguments within the shell-quoted limits: 3 KiB per
physical line, 96 KiB per field, and 512 KiB total. Wrapped content can span
many short physical lines. Other requests travel as UTF-8 bytes on stdin of the
same process. Where direct-async spawns qualify (single-hop ssh or scp), a
private channel carries a short bootstrap command. Its stdin carries the
NUL-terminated script, then the request; both use 1024-character pipe writes.
Synchronous and asynchronous carriers share the same bounded-write seam under
the existing remote-operation guard. Quitting an asynchronous dispatch tears
down its child and buffers, reports the failed program, and propagates quit.
Elsewhere TRAMP copies an input file to the target. Delivery changes do not split
a program into per-file calls. Whole-process failure invalidates cached
interpreter paths for later lookup; an ordinary refused operation does not.

`run-program-async` runs a local program as its own process too, and
nothing waits for it. It keeps the synchronous carrier's staged payload files,
so no payload is base64-encoded or sent through a pipe; a request that fits the
argument bounds travels as arguments and writes nothing to stdin, and a larger
one streams as above. The job owns its staged files until delivery. Cleanup
runs from the delivery timer before the callback, never from the sentinel;
it removes unconsumed payloads even when a program stopped early. Failed or
quit launches clean up before reporting failure. Cleanup errors are diagnostic
and cannot replace the stored outcome. Each private buffer is cleaned up
independently, including after another buffer's kill hook fails. An exit hook
removes files belonging to jobs whose delivery never ran and retries files
whose earlier deletion failed; a hard process kill cannot run that hook.
Remote targets use the pipe
carrier only where direct-async qualifies and otherwise run synchronously;
while `--async-wait` is bound the program also runs synchronously, and
`mevedel-session-control-fs-wait-async` waits for programs already running
(exit and buffer close call it before releasing a lease).

The process sentinel only decodes and stores the outcome. The callback is
always dispatched afterwards through the idle transport with a zero delay,
never inline in the sentinel, because a sentinel can sit inside another remote
command and a callback may start the next target phase. It therefore runs once
no remote operation is in flight. If the transport is disabled or the queued
dispatch is cancelled, the stored outcome is still delivered once, from a
zero-second transport timer: neither lost nor replaced by an invented error.
A request that fails to launch calls the callback before returning, once.
Nothing orders an asynchronous program against others on the same target;
callers serialize the ones that touch the same files.

Two to 32 independent unbounded reads may use GNU tar in the same process, keeping
all proved parents open, refusing symlink leaves, and disabling inherited
`TAR_OPTIONS`. Emacs decodes expected ordered regular members and checks headers
without extraction. Failed transfer or rejected archives discard the batch and
retry fresh ordinary reads. Bounded reads and mixed programs use the ordinary
path; byte/hash validation stays with callers.

The [session manual](../sessions.md) describes the ownership and publication
contracts that use these operations. This layer reduces their execution cost;
it does not change their proof cadence or add a cross-operation authority cache.

## Rationale and consequences

Remote durability is sensitive to process and transport round trips. Adjacent
operations share one dispatch while retaining per-operation descriptor proof.
A steady-state renewal can carry verification, writes, listing, and an optional
clock observation together. Cold, contested, or stale-clock renewal still needs
a preceding observation. Publication continues to prove ownership before every
artifact write and after the last, even when those operations share a process;
an adjacent lease commit with no target write in between -- the reservation, or
a committing batch's head commit -- is that proof. A diagnostic append carries
its own proof as the first operation of its program instead of a reservation.
A collection deletion does too: its program first verifies the lease generation
bytes, a target clock before the lease expiry, and the planned journal pin
directory's entries and bytes.
A lease commit can carry adjacent publication steps as riders in its own
program: the reservation is followed by the first fixed write, behind a
`verify-latest` of the lease directory, and the head commit is preceded by the
immutable generation. Order and proofs are those of the separate programs; only
their process starts are shared. A prelude delays the lease write past the
clock reading the commit was decided from, so its program also proves the
target clock is still before the earlier of the held and the replacement
lease's expiry; when that fails, the completed generation's head is committed
again from a fresh reading, failing closed only once the held window lapsed.
A local payload that cannot be staged travels inline, so the target, not
a local staging error, refuses the operation and earlier operations still run.
Identical lease bytes need verification and listing but no replacement write.
Renewals omit the trailing clock operation while the transaction's existing
reading is fresh, without refreshing its age. Once that reading expires, the
ordinary target-clock observation is required again.

The argument bounds address different limits. TRAMP's canonical PTY line can
truncate above 4 KiB and wedge `process-send-string`; ordinary timeout handling
does not unwind that write. The 3-KiB physical-line allowance reserves room for
TRAMP's prefix and the script's final line. Per-field and aggregate bounds also
leave margin below kernel exec limits. Non-ASCII fields use the file carrier to
avoid connection-coding conversion. A request-file carrier is preferable to a
failed or wedged connection. The direct channel has its own command-line bound,
the target's `PIPE_BUF` (4096 bytes on the measured target), which TRAMP
enforces. The script therefore travels on stdin rather than as an argument.
Streamed payloads read with `dd iflag=fullblock`, because a pipe delivers short
reads that a request file never did.

The target requires Linux descriptor facilities, Bash, stat, and base64; locked
programs also require flock, and content hashing and blob verbs require
sha256sum. Missing required utilities fail visibly. GNU tar is
an optional read optimization with an ordinary-read fallback. No new caller
protocol, extraction directory, or generic resolver cache is needed.

## Decision history

- **Programs still blocked the editor while the target worked (2026-10-20).**
  The merged background-command delivery (three programs, about 120 ms) left
  the editor waiting in each program; the earlier trace put 98 of 135 ms in
  such waits, and the user observed the pauses as stalls of about 120 ms.
  The existing asynchronous carrier was test-only locally, and inlined the
  source-file payload into its pipe request, which would have reintroduced the
  encoding and write work staging removed. Local async now stages as the
  synchronous carrier does. The first version delivered callbacks inline when
  the transport was idle; the sentinel can be inside another remote command and
  the callback launches the next phase, so dispatch became unconditional and
  deferred, including the disabled-transport fallback. On a compiled
  in-process fixture rerun after checkpoint preparation changes (a synthetic
  local portable session of 6000 saved turns, one 4000-character result per
  sample, six samples), the publication that held the editor 63.907-70.177 ms
  synchronously returned its submission in 0.196-0.460 ms and acknowledged
  105.028-115.540 ms later with a largest 5 ms-timer heartbeat gap of
  6.533-7.159 ms. Earlier async ranges were 8.2-10.2 ms gap and
  75.4-120.1 ms ACK: no blocking regression was observed, not a general ACK
  improvement. Separate phase instrumentation measured at most 1.674 ms per
  async advance. The original recipe collects garbage before each sample;
  none contained GC, and possible independent verifier load was not excluded.
  Launch, staging and parsing stay on the main thread, so
  the 98 ms is an upper bound on waiting removed, not a measured saving, and
  acknowledgement latency is not shorter. One fixture and small samples, not a
  live or graphical measurement, nor a hard bound on arbitrary filesystem
  latency, sidecar size or GC.

- **Publication still split adjacent proof and write steps.** A live trace of
  one background-command delivery on a local session (2026-10-20) found six
  programs in its sidecar publication, about 98 of its 135 ms: admission,
  reservation, the fixed sidecar write, a pinned read of the client's own
  staged sidecar, the generation, and the head commit, plus two `uname`
  processes. The reservation now carries the first fixed write, the head
  commit carries the generation, the staged sidecar is read locally and the
  local hostname comes from `/proc`: three programs, about 120 ms. Merged
  programs cost close to the sum of their parts, so most of the remaining
  time is the operations' own subprocesses, not program starts. The write
  that rides the reservation is guarded by `verify-latest` because Lisp no
  longer sees the reservation's listing before it; a stopped write falls back
  to the previous separate program. Review found that the merged head commit
  could otherwise commit an already-expired lease after a slow generation;
  real-clock probes confirmed the guard and the fresh-clock retry. A
  read-only agent directory also showed a local staging error ending the
  reservation's program before it ran, reported as a lease failure.

- **Batched discovery could outlive its lease listing.** A deterministic
  two-client interleaving reproduced an activated successor pruning every
  generation from the reader's earlier listing. The later batched read then
  reported no publication for a saved session. Discovery now refreshes once
  when the observed generation set changed and no live record was obtained.
  It reports an error if that nonempty replacement set also becomes unreadable;
  unchanged records, malformed leases and manifest failures are not retried.
  The normal successful batch pays no additional program. This reproduced
  correctness bug was found while investigating an intermittent SSH acceptance
  discovery failure; that original failure retained no incompatible-row detail,
  so its cause cannot be attributed to this interleaving.

- **Prompt admission still split adjacent observations.** The collaboration
  backlog recorded about 120 ms of synchronous admission controls on the
  deployed host. Current-code regression fixtures confirmed that cold local
  and remote admission used separate transfer and recovery programs, even
  though both belong to the same admission transaction. The transfer program
  now carries the recovery-root and PID-lock probes; the ordinary authority
  gate consumes their results without repeating them. Uncontested admission
  with an owned lease takes one program instead of two. A warm local mailbox
  already avoided the transfer program, so it remains at one. These are program-count results,
  not a new deployed-host latency measurement. Lease, recovery and incarnation
  checks remain synchronous authority requirements; no cross-admission cache
  or delayed safety check was introduced. A deterministic interleaving also
  reproduced an older poll overwriting a nested grant with `requested` and
  admitting work. Poll application now preserves newer local transfer state,
  and admission rechecks quiescing/released state and its live lease binding
  after yielding checks. A second regression reproduced manual lease release
  during those checks, which has no cooperative transfer state to inspect.
  Full-suite integration exposed first acquisition without a lease directory:
  its failed observation skips later probes, so those unexecuted probes now
  leave the ordinary authority gate responsible for fresh mandatory checks.
  A nested refresh of `requested` also reproduced the opposite ordering bug:
  the outer poll committed a timeout grant but kept the nested undecided state.
  Its durable decision now supersedes a refresh of the same immutable request,
  while newer terminal states and different requests remain protected.

- **Shared-note cleanup needed byte-preserving undo for arbitrary files.**
  Curated-memory write intents retain full UTF-8 snapshots of at most 32 KiB
  inside 4-MiB escaped Lisp records, and `verify` ships expected bytes in the
  request. Shared folders hold images, recordings and multi-megabyte results,
  which could neither be retained nor compared that way. Hash verification and
  content-addressed blob capture and restoration keep large bytes on the target.

- **Large pipe writes stalled remote image saves.** A provisioned SSH browser
  acceptance run measured a 12.5-second median for individual image moves.
  A smaller real-publication reproduction with a 1.4 MB image-bearing shared
  state reproduced 5.7-9.9-second saves on the base commit and 9.4-9.8 seconds
  on the reviewed branch; remote reads took only 0.2 seconds. Most time was
  spent sending the two publication payloads through the pipe carrier.
  Splitting each request into 1024-character writes, as the private editing
  helper already does, reduced those saves to 0.65-0.99 seconds. The request
  bytes, target program, completion checks, and mutation fences are unchanged.

- **Local payloads paid a remote carrier's encoding.** After a sidecar
  shrank from 941 KB to 259 KB, a local save still spent 116 of 185 ms in six
  programs; a 260 KB `write` took 31 ms against 8 ms for a small one, the
  difference being base64 in the editor, the request file, and `base64 -d`
  on the target. Staged local payloads took that write to 8.8 ms and a
  1.5 MB transcript write to 8.5 ms. Parent proof, early stop, and result
  semantics are unchanged; only the local payload transport moved. Staging
  every local write, not only large ones, also took a write from three
  processes to one and a create from four to two. The first version named
  staged files by absolute path; the parent-swap regressions showed that a
  pathname swapped after the proof redirected the rename, so the name became
  relative to the pinned parent.

- **Every lease generation claim took two programs after its observation.**
  The fencing create read the predecessors back, and activation was a second
  program. The same mock-target replay put an unsettled-mutation latch change
  at three programs. A remote Bash call pays two latch changes, arming and
  settling. When the claimant already holds the newest predecessor's exact
  bytes, the claim program now verifies them between the create and the
  activation write, and each latch change costs two programs. An unchanged
  byte string is at least as strict as the parsed-record equality it replaces.

- **The request file cost about twenty TRAMP commands per oversized program.**
  In the September 23 remote capture, the worst stall (2.4 s) was an autosave
  generation write that spent 43 commands copying its request file. Diagnostic
  appends through the same carrier took up to 1.4 s. Against the same LAN
  target, a 100 KB write plus read over the request file took 508-578 ms and
  20 commands. On a direct-async pipe it took 100 ms (178 ms cold) and 0
  commands, with the exit status intact. Direct-async qualifying targets now use
  the pipe; other targets keep the request file.

- **Remote sessions still blocked input on several programs per operation.** A
  September 23 capture of a real remote session (LAN target, 13-21 ms per bare
  TRAMP command, about 50 ms per control program) spent 76.5 s of a 384 s request
  blocked in 2,266 TRAMP round trips. A mock-target replay counting programs per
  operation found proofs repeated back to back rather than weak ones: a save used
  11 programs, a diagnostic flush 9 for one append, an agent transcript 10 to stage
  bytes it never wrote to the target, and each tool admission 5. The recovery
  marker read now carries the PID-lock proof and, inside a transaction, the clock
  reading the ownership checks that follow need; a lease observation reads the
  clock only when the transaction's reading is stale; transfer requests are listed
  in the lease observation; admission is one transaction; a latch claim reuses its
  caller's observation. Publication counts the reservation as the proof before the
  first fixed write and the head commit's exact-generation check as the proof
  after the last write of a committing batch, because no target write intervenes
  in either case; an owned lease is reserved without a preceding renewal; a batch
  with no fixed write and no marker only stages. Diagnostics no longer reserve:
  each append runs behind a `verify` of the committed lease bytes and a listing in
  its own program, with publication held active so critical publishers queue. The
  replay moved a save from 11 to 6 programs, admission from 5 to 2, a transfer poll
  from 2 to 1, a latch change from 4 to 3, a diagnostic flush from 9 to 2 and agent
  staging from 10 to 1. Every fixed write still follows a proof with no target
  write in between, and there is still no cross-operation authority cache.

- **Repeated renewals still performed redundant target work.** A September 22
  captured-transcript replay spent 142 ms in publication and 211 ms in a small
  save. Five alternating comparisons of the old and new renewal programs reduced
  those medians to 101 and 164 ms. Program count stayed at 13, while median lease
  and artifact writes fell from nine to five and clock-marker operations from
  eight to one. The change removes redundant operations inside existing programs;
  it does not introduce a new batching protocol or relax ownership checks. Editor
  allocation stayed around 30.4 MB per save. These are isolated local replay
  measurements, not graphical input-latency measurements.

- **The original ADR 0101 claimed a program removed the race window and provided
  compare-and-set.** Tracing a concurrent exclusive creation of the next lease
  generation showed that it can land between verification and write. The write
  still happens and the caller observes who won afterward. The claim was corrected
  to a precondition with a narrower interval. Later journal mutation requirements
  added explicit directory locking; ordinary programs are still not atomic.
- **Per-operation process dispatch made durability expensive.** Grouping operations
  retained parent proof at each operation and moved execution mechanics behind
  one entry point. A later session-listing profile found it still using wrappers
  independently: 491 processes for 66 directories, taking 2.4 local seconds.
  Batched probes/listings and reuse within the caller's durable transaction reduced
  that listing to 87 processes and 0.82 seconds. A PID-lock memo and passed-down
  observed listing share evidence only in its owning observation.
- **Local stderr files caused remote copies.** A remote-turn profile attributed
  about one twelfth of time to that path and about twice as much to request files.
  Target-collected diagnostics with a distinct trailing frame and argument delivery
  removed those transfers for eligible calls. The full ERT run later took
  749.09 seconds; its slow memory-retention case spent 28.07 of 28.42 seconds in
  4,068 programs. Replacing the target diagnostic temporary file with a pipe
  removed creation/deletion subprocesses, reducing repeated local single-read
  cost by 28% and existence-probe cost by 37%, while preserving binary diagnostics
  and early-stop behavior.
- **Removing descriptor-path proof weakened authored writes.** A later latency
  optimization replaced the inline writer's `readlink` proof with a non-symlink
  check and inode equality. A deterministic race opened an outside file through
  a temporary symlink, then replaced that name with a hard link to the same
  inode. Equality accepted it and the write overwrote the outside file; the
  descriptor's opened path rejected it. The path proof is restored for writes
  and mode changes. The same review restored encoding stderr before command
  substitution: delaying encoding removed an encoder on quiet programs but
  dropped embedded NUL bytes from previously supported target diagnostics.
  Regression tests cover both refusals and preserve arbitrary diagnostic bytes.
- **Base64-encoded numeric scalars added a decoder process to each bounded read
  and guard.** Repeated local measurements put this at 18–22% after the diagnostic
  pipe change. Validated digit strings replaced that encoding. Bash substitution
  removes verification-payload wrapping; existing-directory conflict avoids a
  needless mkdir process. Arbitrary bytes remain encoded.
- **Batched reads still encoded each file separately.** A reused portable
  100-session search spent 1.536 seconds in 24 control calls. Bulk transfer reduced
  measured control time to 0.774 seconds with the same calls and source checks.
  These are instrumented local measurements, not remote latency guarantees.
  Native tests cover leaf replacement, missing carrier, corrupt archive retry,
  and binary/long-name decoding. The long UTF-8 filename case also exposed the
  request-file encoding failure corrected by explicit UTF-8 encoding.
- **Physical-parent proof launched a pwd subshell for every operation.** History
  profiling put 1.8 seconds of a reused 100-session query in control programs.
  Bash's physical cd and resulting PWD replaced those subshells while retaining
  failure when the opened directory's physical location cannot be determined.
  Symlink and parent-replacement tests cover the boundary.
- **The initial argument explanation bounded the whole quoted request to 3 KiB.**
  The current carrier bounds physical lines, individual fields, and total bytes
  separately. Wrapped payloads therefore need not use a request file solely
  because their total exceeds one PTY line. The original rationale—avoiding a
  connection-wedging write—still applies; the inspected record does not contain
  a separate measurement for this carrier refinement.
- **Admission probe cost:** recomputing environment, capability, and sandbox
  readiness at every mutation boundary cost roughly fifteen synchronous target
  round trips per admitted mutation. Admission now observes incarnation with one
  command and reuses connection-scoped readiness, with full probing on open,
  connection replacement, explicit retry, or observation failure. The original
  session manual supplied no separate measurement date.
- **Diagnostic append cost:** republishing complete diagnostic logs on every
  flush grew quadratically with stream size. Delta-only pinned append replaced
  that path; a crash can tear the last diagnostic line, which is not resume state.
- **Synchronous diagnostic appends:** each append program still waited on the
  target inside the request that produced it. Run 7 of the remote GUI capture
  (2026-09-23) put 12% of a 350 s request inside TRAMP. Appends are now
  collected per batch and run as one background direct-async program on
  capable targets; its callback performs no target I/O, and unwritten entries
  retry from an in-memory backlog.
- **Reserved collection deletions:** run 7 spent 102 of its 151 collection
  programs in 17 deleting steps of six synchronous programs each: a clock
  read, a pin listing and read, a reservation, the deletion, and the
  reservation's release. The deletion program now carries those proofs itself
  and runs in the background on direct-async targets.
- **Timer suspension did not stop sentinel reentrancy.** Projectile's advice on
  `delete-file` resolved remote project roots while native compilation or syntax
  checking deleted local temporary files. Those sentinels ran inside a remote
  control command despite JUST-THIS-ONE output waiting. TRAMP refused the nested
  call, preserving the running reply but producing an error in the sentinel.
  A slower remote transfer poll reduces exposure; package-specific suppression
  remains an operational workaround, not a guarantee against external sentinels.
