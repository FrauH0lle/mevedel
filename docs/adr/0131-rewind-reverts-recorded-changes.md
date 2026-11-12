# Rewind reverts recorded changes

Status: accepted. Revises the file policy in ADR 0072 and the objection to
independent conversation sessions in ADR 0091.

## Decision

Rewind prepares the reverse of each discarded turn's recorded file changes,
newest first, using its existing before/after `file-history` contents. It
preserves current edits outside those changed line runs, including edits made
between discarded turns. The transcript boundary, session identity, local
directory transaction, portable publication commit, rollback and Redo contracts
remain those of [ADR 0072](0072-make-rewind-in-place-undo.md).

Preparation writes nothing. A conflict in any file refuses the whole Rewind,
listing the file, line range and discarded turn. For an older turn the line
range refers to the intermediate text after newer changes have been reversed.
The user can cancel or choose **force snapshot restore**, then explicitly
confirm overwriting newer edits. Force uses the previous whole-file restoration
plan. Redo continues to restore a selected published head's captured state;
it is not changed into replay of recorded changes.

Successful preparation passes one final replacement per file to the existing
transaction. The transaction recomputes the same kind of plan after confirmation
and each reverse-change write checks the current bytes again. Modified visiting
buffers retain their save/discard/abort prompt; a save recomputes the reverse
plan rather than silently switching to snapshot restoration. Creation, deletion
and binary changes require the complete recorded post-state to match. There is
no write when the current file already equals the recorded pre-state. There is
no inferred file-mode history: existing checkpoints record bytes and absence,
not permissions.

Capture coverage is unchanged. Shell, Eval and uncaptured agent effects are not
made reversible. Known gaps remain visible, not presented as restored paths.
New pre-change captures use literal bytes, matching post-change capture; the
human-facing final diff decodes them for presentation. Existing backup artifacts
and the persisted index format remain unchanged and readable. Old backups
already captured with decoding cannot recover their original byte encoding.

## Rationale and evidence

ADR 0072 selected snapshot restoration without comparing it to checked reverse
changes. Its concern about selective undo was silent loss of intervening work.
Refusing overlapping edits addresses that concern without restoring unrelated
lines. The existing capture boundary records ApplyPatch and SharedSaveImage;
whole-file snapshots cover no additional filesystem effects.

The Rewind regression reproduced the difference with two recorded edits to one
line and a later user edit elsewhere: snapshot restoration lost the user's
edit, whereas newest-first reversal preserves it. Another regression runs a
real ApplyPatch through the pipeline on a CRLF file: decoded pre-capture and
literal post-capture falsely conflict with an unrelated later edit. Literal
capture removes that mismatch without changing the storage format.

The rejection of a session per directive in
[ADR 0091](0091-render-directive-turns-in-the-shared-session-view.md) depended on
competing whole-file checkpoint restores. Checked reversal removes that
objection: a conversation may own a session without silently undoing another
session's unrelated work. This decision does not change directive rendering or
create note conversations. It also introduces no merge view.

## Decision history

The previous default restored whole captured files after confirmation, including
external edits. That behavior remains available only as the explicit force
choice. The shared transaction and published-head Redo remain independent of
the default file-change policy.
