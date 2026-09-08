# Use one patch-oriented file mutation tool

Replace separate Edit, Write, and MkDir model tools with one `ApplyPatch` tool using Codex's patch grammar. A single multi-file patch proposal preserves the model's intended change set and enables one hierarchical, atomic patch review with per-change decisions; the trade-off is that permissions, snapshots, and preview state must understand every affected path rather than one file at a time.

The shared file/buffer transaction also serves memory consolidation. Consumers
can supply expected-before snapshots and current target ownership. A failed
transaction restores only its exact intended after-state; intervening disk or
unsaved buffer edits remain intact and are reported as incomplete recovery.
This strengthens the original unconditional rollback after tests reproduced a
synchronization hook writing an intervening edit and then failing. Ownership
checks also fence rollback, and a quit rolls back before cancellation propagates.

Memory application supplies a target mutation callback for both forward writes
and rollback. That callback checks exact snapshots and claim authority under
the same target-side lock used by takeover; the patch engine retains buffer
synchronization and recovery reporting. A client-side ownership predicate alone
cannot stop a filesystem operation delayed past its claim's settlement.
