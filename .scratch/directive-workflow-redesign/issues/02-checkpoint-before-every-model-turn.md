# 02 — Checkpoint before every model turn

**What to build:** Make Rewind capable of restoring the repository to immediately before the first or any later accepted model turn. The existing chronological recovery flow remains transactional and reports exactly what it can and cannot restore.

**Blocked by:** None — can start immediately

**Status:** resolved

- [x] Every accepted model turn owns a durable pre-turn checkpoint, including the first turn in a session.
- [x] The checkpoint represents known files that were modified, created, or deleted by the turn without relying on a reverse patch.
- [x] Rewinding to a selected turn restores the covered repository state from immediately before that turn and truncates that turn and every later session turn.
- [x] The impact preview distinguishes fully captured restoration from known checkpoint gaps before confirmation.
- [x] A checkpoint gap does not hide the Rewind action or claim that recovery will be complete.
- [x] Restoration remains transactional for covered files and rolls back the recovery operation if applying the captured state fails.
- [x] Existing safety checks for live executions and pending interactions still prevent unsafe Rewind.
- [x] Rewind remains linear and destructive; this ticket adds neither redo nor another history stack.
- [x] Tests use real temporary files and cover first-turn modification, creation, deletion, later-turn rewind, incomplete coverage, cancellation, and transactional failure.

## Answer

Resolved by commit `4af6d36`.
