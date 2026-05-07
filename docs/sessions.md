# Session Persistence

Sessions auto-save lazily and per-completed-turn. Compaction rotates
segments rather than rewriting in place.

Conversation compaction has its own doc in
[`compaction.md`](compaction.md). This page describes the session
persistence contract that compaction relies on.

## Session persistence

Sessions auto-save lazily and per-completed-turn under
`<workspace-root>/.mevedel/sessions/<name>-<timestamp>-<short-uuid>/`.
Layout:

```
.mevedel/sessions/main-2026-04-23T14-30-a9f2/
  session.meta.el                    ; sidecar plist (workspace, perms, tasks, ...)
  .lock                              ; PID + hostname + buffer name; released on kill
  segment-0001.chat.org              ; finalized at compact #1
  segment-0002.chat.org              ; finalized at compact #2
  segment-0003.chat.org              ; current/live
  file-history/                      ; per-session backup store
    4f1e8c9a3b2d6e57@v1
    4f1e8c9a3b2d6e57@v2
  agents/                            ; sub-agent transcript .chat.org files
  input-history.el                   ; optional view input history sidecar
```

The data buffer is locked to `org-mode` so `gptel-org--save-state`
can round-trip text-property bounds via `GPTEL_BOUNDS`. The sidecar
holds session-wide state that doesn't live in the buffer text:
permission rules, tasks, prompt-index (driving the rewind picker),
`:file-snapshots` (per-turn map of tracked files to backup names),
workspace identity, `:working-directory`, fork lineage
(`:forked-from-session-id` / `:forked-from-turn`), and
`:agent-transcripts` metadata. Older sidecars without
`:working-directory` restore at the workspace root.

For mevedel chat buffers with dynamic preset system prompts, save-time
advice around `gptel--save-state` removes frozen `GPTEL_SYSTEM`
metadata. Restored sessions keep the preset reference and rebuild the
system prompt dynamically.

### Resume contract

On-disk state always reflects a completed turn boundary. Mid-flight
requests are not recoverable; their pending tool calls are discarded by
virtue of never having been auto-saved.

### Rewind

`mevedel-rewind` picks any prior user prompt across all segments via
`completing-read`; selection truncates the live buffer to that turn's
response, sets `buffer-file-name` to nil so saves can't corrupt the
original, optionally restores tracked files to their state at that turn
(per-file plan with external-changes detection), and arms
`mevedel-session--fork-pending`.

### Fork

When the user sends in a buffer with `fork-pending` set,
`mevedel-session-persistence-fork-now` materializes a fresh fork
session — predecessor segment files copied verbatim, picked segment
truncated, file-history backups referenced by the target state copied,
referenced agent transcript files copied, and input history copied when
present — then the send proceeds onto the fork's segment file. The
parent session is never modified.

### Agent transcripts

Sub-agent transcript files live under `agents/`. The sidecar's
`:agent-transcripts` alist records each agent's id, type, description,
relative path, status, timestamps, parent turn, and call count. The
view uses this metadata to render handles and open terminal transcripts.

Running transcripts are coerced to `incomplete` on normal resume because
mid-flight sub-agent requests are not recoverable. Read-only attach
observes the on-disk state without rewriting it.

### Input history

The view input ring is persisted as `input-history.el` when the session
is writable and persistence is enabled. Missing files are normal.
Corrupt files are warned about once, renamed aside, and replaced with an
empty in-memory ring.

### Locking

`.lock` files prevent concurrent edits. Same-host live PID →
`user-error`; same-host stale PID → prompt to break; cross-host →
break / read-only / abort prompt.

### Auto-cleanup

`mevedel-session-max-age-days` (default 30) deletes expired sessions on
`mevedel-resume`, skipping locked sessions and throttled to once per
workspace per Emacs invocation. `nil` disables.

## Defcustoms

All in `mevedel-session-persistence.el`:

- `mevedel-sessions-directory` (default `.mevedel/sessions/`)
- `mevedel-session-persistence` (default `t`)
- `mevedel-session-max-age-days` (default 30)
- `mevedel-file-history-max-snapshots` (default 100)
- `mevedel-file-history-max-snapshot-bytes` (default 1 MB)
- `mevedel-view-input-history-size` (in `mevedel-view-history.el`,
  default 500)

Recommended `.gitignore` line: `.mevedel/sessions/` (or the broader
`.mevedel/`).
