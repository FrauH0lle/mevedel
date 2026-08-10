# 09 — Rewind directive implementation attempts

**What to build:** Make Rewind the directive’s sole implementation undo. From any implementation attempt, users can preview and confirm restoration to immediately before it, including the complete later session suffix, while authored directives survive and their lifecycle is recomputed from remaining activity.

**Blocked by:** 06 — Replace Revise with Request changes and Retry; 08 — Preserve directives when their source disappears

**Status:** resolved

- [x] Successful, failed, and aborted implementation attempts with file effects offer Rewind before this implementation from directive activity.
- [x] The action resolves the attempt’s execution session and pre-turn checkpoint rather than reverse-applying its stored patch.
- [x] The impact preview lists covered file restoration, known gaps, later ordinary chat turns, and later directive events that confirmation will discard.
- [x] Confirming Rewind truncates the selected attempt and every later turn in that execution session and removes their model-produced directive activity.
- [x] User-authored directive records and current request edits survive even when all of their attempts are discarded.
- [x] Directive state is recomputed from surviving activity: no activity is Ready, surviving discussion is Discussed, surviving success is Implemented, and surviving failure is Failed.
- [x] A current request differing from the latest surviving attempt is Ready with a request-changed qualifier.
- [x] Rewinding the newest serial directive preserves earlier directive attempts; rewinding an earlier directive discards every later dependent directive and chat turn in that session.
- [x] A later directive whose implementation is discarded survives as Ready rather than disappearing.
- [x] Restoring a deleted source file triggers the established safe reattachment behavior without making the overlay the owner of recovery.
- [x] The action remains available with incomplete coverage, but the preview prominently states every known gap and never promises complete undo.
- [x] Compact session events, directive activity, patches, and visible state agree after committed Rewind and after cancellation or failed restoration.
- [x] No Undo implementation alias, reverse-patch path, redo store, or attempt switching is added.
- [x] Tests use real serial directives, ordinary intervening chat turns, modified/created/deleted files, first-turn checkpoints, source restoration, cancellation, gaps, and transaction rollback.

## Answer

Implemented in `765ddc7`.
