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
many short physical lines. Other requests use an explicitly UTF-8 encoded input
file in the same process. Delivery changes do not split a program into per-file
calls. Whole-process failure invalidates cached interpreter paths for later
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
artifact write and after the last, even when those operations share a process.

The argument bounds address different limits. TRAMP's canonical PTY line can
truncate above 4 KiB and wedge `process-send-string`; ordinary timeout handling
does not unwind that write. The 3-KiB physical-line allowance reserves room for
TRAMP's prefix and the script's final line. Per-field and aggregate bounds also
leave margin below kernel exec limits. Non-ASCII fields use the file carrier to
avoid connection-coding conversion. A request-file carrier is preferable to a
failed or wedged connection.

The target requires Linux descriptor facilities, Bash, stat, and base64; locked
programs also require flock. Missing required utilities fail visibly. GNU tar is
an optional read optimization with an ordinary-read fallback. No new caller
protocol, extraction directory, or generic resolver cache is needed.

## Decision history

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
- **Timer suspension did not stop sentinel reentrancy.** Projectile's advice on
  `delete-file` resolved remote project roots while native compilation or syntax
  checking deleted local temporary files. Those sentinels ran inside a remote
  control command despite JUST-THIS-ONE output waiting. TRAMP refused the nested
  call, preserving the running reply but producing an error in the sentinel.
  A slower remote transfer poll reduces exposure; package-specific suppression
  remains an operational workaround, not a guarantee against external sentinels.
