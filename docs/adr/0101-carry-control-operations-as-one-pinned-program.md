# Carry control operations as one pinned program

Status: accepted

## Decision

One target process carries a program of session-control operations rather than
a single operation.  Each operation in a program opens and re-proves its own
parent descriptor, so a program is exactly as pinned as the same operations run
one at a time: a symlinked parent component or final entry still fails closed,
and a pathname swapped after the descriptor was opened still cannot redirect the
operation.

A program stops at the first operation that does not succeed and reports the
remaining ones as skipped.  A compare-and-set is therefore stated as a `verify`
operation that the writes after it depend on, and the proof and the write it
guards execute in one process.  This is strictly stronger than reading a record,
comparing it in Emacs, and writing: that sequence leaves a window between the
comparison and the write, and a program has none.

Every operation reports one of `ok`, `conflict`, `absent`, `mismatch`, `failed`
or `skipped`, so callers reproduce per operation the nil-versus-signal contract
of the single-operation wrappers.  An operation may be marked optional, meaning
its failure does not end the program; ensuring a directory that may already
exist is the case that needs it.  Requests and responses are NUL-framed with
base64 payloads, because filenames and content both contain bytes a shell cannot
carry through a command substitution.  Target diagnostics are captured apart
from the response, so a tool writing to stderr cannot present itself as a
result record.

A request reaches the target as arguments when it fits and through a stdin file
when it does not.  One argument carries one field, because NUL -- the framing
byte -- is the one byte a filename cannot contain and therefore cannot be
embedded in an argument.  An argument run whose shell-quoted size exceeds
3 KiB, or any field outside ASCII, sends the request to the file instead.
Either way a program is one target process; an oversized request changes how the
request travels, never how many calls carry it.

The size bound is far below the target's own, and it is not `ARG_MAX` — that is
megabytes.  It is one physical line of the command TRAMP writes to the
connection process, which talks over a pty: canonical mode truncates past
`N_TTY_BUF_SIZE` (4 KiB), and `process-send-string` then blocks inside the
write.  No timer interrupts that and no timeout unwinds it, so an over-long line
wedges the connection for the life of the process rather than failing.  Only the
arguments are budgeted, because only they are one unbroken line:
`tramp-send-string` preserves newlines, so the script — several times this
budget — arrives as a hundred short lines.  The remaining kilobyte covers the
script's last line and TRAMP's prefix.  The measurement is taken after shell
quoting, which is what lands on the line.  The ASCII bound is TRAMP's: it
encodes a command line with the connection coding system, while the request file
is written without conversion, so a name holding non-ASCII bytes is only
byte-transparent through the file.

Checks inside the target-side operation state their own failure explicitly
instead of relying on the shell's `errexit`.  The dispatcher runs each operation
on the left of a `||` so it can capture the status, and that suppresses
`errexit` for everything the operation does, which would otherwise let a
refused symlink or a failed descriptor proof continue into the operation it was
meant to prevent.

The single-operation entry point remains, and its wrappers keep their existing
error classification and interpreter-cache behaviour.

## Consequences

Durable session work is bounded by target round trips, and this is what makes
the count tractable.  A steady-state lease renewal is one round trip: the
commit program proves the precondition with its opening `verify`, writes,
lists, and refreshes the transaction clock with a trailing optional clock
operation, so the next renewal in the same transaction can assume its
observation instead of repeating it.  Only a cold, contested, or stale-clock
renewal pays a preceding observation program.  A publication generation with
its artifacts and manifest is one round trip rather than one per file.  The documented ownership-proof cadence is unchanged --
publication still proves ownership immediately before every artifact write and
once after the last -- because the cost of a proof, not its frequency, was the
problem.

The target must provide `base64` in addition to `bash` and `stat`.  It resolves
through the target `PATH` like `stat`, so a target without it fails the
operation rather than silently degrading.

Keeping diagnostics apart from the response does not mean keeping them in a
separate file.  A local stderr file makes TRAMP create a remote temporary and
rename it back on every program, and a CPU profile of a remote turn put that at
around a twelfth of it; the request file cost roughly twice as much again.  So
the program discards stderr at the target, collects it there, and emits it as
one trailing record whose header is a word where an operation's is a number --
which is the separation the property actually needed.  Argument delivery removes
the request file for the traffic that dominates the round-trip count -- leases,
clocks, transfers, recovery -- and leaves it for the programs that carry a whole
artifact or generation, which are the rare ones.

Reading a program's diagnostics now depends on its trailing record arriving.  A
program killed before its exit trap runs has none, and the failure is reported
from the raw output buffer instead.
