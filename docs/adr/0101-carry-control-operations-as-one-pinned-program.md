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

Requests use five fields per operation and NUL framing. Arbitrary content and
response bytes use base64; numeric limits, modes, and deadlines use validated
digit strings. Diagnostic bytes are encoded through a separate pipe and returned
in a distinct trailing record. A killed process without that record reports
its raw captured failure output.

ASCII fields travel as arguments within the shell-quoted limits: 3 KiB per
physical line, 96 KiB per field, and 512 KiB total. Wrapped content can span
many short physical lines. Other requests travel as UTF-8 bytes on stdin of the
same process. Where direct-async spawns qualify (single-hop ssh or scp), a
private channel carries a short bootstrap command. Its stdin carries the
NUL-terminated script, then the request. Elsewhere TRAMP copies an input file
to the target. Delivery changes do not split a program into per-file calls. Whole-process failure invalidates cached interpreter paths for later
lookup; an ordinary refused operation does not.

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
programs also require flock. Missing required utilities fail visibly. GNU tar is
an optional read optimization with an ordinary-read fallback. No new caller
protocol, extraction directory, or generic resolver cache is needed.

## Decision history

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
