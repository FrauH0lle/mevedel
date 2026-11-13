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
The user can cancel, merge in a separate smerge editor, or choose **force
snapshot restore**, then explicitly confirm overwriting newer edits. Merge
resolutions stay in memory until all discarded turns can be reversed. An older
conflict opens against the intermediate result of newer reversals, not the
original file. Confirmation rechecks source text, disk bytes and conversation
identity; conflict markers never enter source. Force uses the previous
whole-file restoration plan. Redo continues to restore a selected published head's captured state;
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
made reversible. After permission, the pipeline records each turn's Bash, Eval
and other unsnapshotted mutating tools, Agent and SendMessage as untracked
effects. Rewind and the impact report treat them, like capture failures, as
known gaps, never as restored paths, so coverage is reported incomplete.
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
session's unrelated work. Note conversations use those independent sessions.
Their applied changes belong to the preparing request's reserved turn, even
when the user applies them later. Rewinding before the first turn returns the
notes to unsent without deleting their text, anchors or session links. The
workspace store's existing Rewind intent settles that lifecycle change and
clears proposals from discarded turns, including after a crash. Sending an
unsent zero-turn note starts again in the same session. ADR 0132 later removes
directive-specific rendering; old saved directive turns are ordinary turns.

## Decision history

The previous default restored whole captured files after confirmation, including
external edits. That behavior remains available only as the explicit force
choice. The shared transaction and published-head Redo remain independent of
the default file-change policy.

The initial reversal implementation offered only cancel and force. The notes
redesign adds staged smerge resolutions so overlapping edits need not force a
choice between abandoning Rewind and overwriting unrelated work. It reuses the
prepared-change editor while retaining the all-or-nothing transaction.

Removing directive records also removed the pipeline step that recorded
untracked effects, because it was gated on a directive. A note turn running
`sed -i` then rewound as "Checkpoint coverage: complete" and silently kept the
shell change. The step now records for every request, and the effects persist
per turn on the session rather than on a directive attempt.
