# Use one patch-oriented file mutation tool

Status: accepted

## Current decision

ApplyPatch is the single model-facing file mutation tool. One payload describes
a multi-file change set using Add, Update, Delete and Move operations. The user
reviews one hierarchy of files and changes, with individual selections and
feedback, before a rollback-backed transaction applies the accepted changes.
The [tool reference](../tools/applypatch.md) owns matching and operation syntax;
the [review manual](../preview.md) owns interaction and result behavior.

The shared file/buffer transaction also serves memory consolidation. Consumers
may supply expected-before snapshots and current target ownership. Recovery
restores only this transaction's exact intended after-state; intervening disk
or unsaved-buffer edits remain intact and produce incomplete-recovery reporting.
Ownership checks also fence rollback, and a quit rolls back before cancellation
propagates.

Memory application supplies a target mutation callback for forward writes and
rollback. It checks snapshots and claim authority under the target-side lock
used by takeover. The patch engine retains buffer synchronization and recovery
reporting.

## Rationale and alternatives

Separate Edit, Write and MkDir tools split a model's intended change set into
unrelated operations. One patch preserves that intent and supports aggregate
review. The cost is that permissions, snapshots and review state must understand
every affected source and destination rather than one path at a time.

A client-side ownership predicate alone cannot stop a filesystem operation
delayed past its claim's settlement. Target-side guarded mutations keep memory
application and takeover under the same authority boundary.

## Consequences

Selection changes the applied subset, but permissions and snapshots still cover
the affected paths. An application failure triggers recovery; an incomplete
rollback remains an explicit failure rather than a claim of atomic success.
Review must leave the user able to inspect conflicts and preserve intervening
work. [ADR 0109](0109-edit-patch-changes-side-by-side.md) defines staged editing.

## Decision history

ADR 0092 replaced the separate Edit, Write and MkDir model tools with ApplyPatch.
The original rollback restored snapshots unconditionally. Tests reproduced a
buffer synchronization hook writing an intervening edit and then failing;
unconditional recovery would erase that work. Exact-after-state checks replaced
that behavior, with incomplete-recovery reporting when bytes or buffers have
moved. Memory's target mutation callback then supplied the lock-bound ownership
check that a delayed client-side predicate could not provide.
