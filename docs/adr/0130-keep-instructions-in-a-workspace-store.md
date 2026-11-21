# Keep instructions in a workspace store

Status: accepted

The record scope is now notes, as defined by
[ADR 0132](0132-notes-replace-references-directives-and-buddy-notes.md).
This record owns workspace storage, concurrency, activation and Rewind intent;
its original directive-activity scope is historical.

## Current decision

**The workspace owns its current notes in one store.** Notes, their text,
anchors, authorship, lifecycle, conversation links, waiting prepared changes,
recently-closed records and display-id state live in
`<workspace>/.niwa/instructions.el` on the
execution target. Sessions write no instruction snapshots. Ordinary resume loads
the store; it no longer loads or clears the workspace set from a session.
Conversations live in their sessions, not the store. There is no directive record
kind, frozen-history reader or old-data conversion.

**One whole-set record, refused when stale.** The store is read and written
through the pinned control-file programs. Writes hold a lock on
`.niwa/leases/instructions/` and name the hash they were based on. A stale
write is merged per instruction UUID against the record this client last
observed:

- a change to an instruction nobody else changed applies;
- a creation applies if its parent still exists, and a colliding display id
  is renumbered above every used and retired id;
- any other change to an instruction another client changed is refused, never
  reapplied. Its note text goes to the kill ring; other refused changes are
  reported as not recoverable.

A write is retried once; if the store changed again, this client keeps the
store's record and refuses its own changes.

Installing a merged or observed record reconciles notes by UUID, restoring
presentations in visited buffers and retaining unvisited records without
opening their source files. Recently-closed retention is applied as part of
the merged record. Conversation and Plan state belong to sessions, not a
second workspace activity object.

**Observation points.** The store is observed when a workspace is first
activated in an Emacs, when the notes listing opens, at resume, and on every
write. There is no watcher: a listing left open may show state another client
has since changed.

- An unchanged store costs one target-side hash.
- A workspace with nothing in memory loads the store, restoring presentations
  only in files already visited; the rest restore when visited.
- Any other difference merges as a write would.
- Activation skips a remote root without a live connection, since resolving a
  workspace must not open one; the next listing or resume observes it.
- Activation happens when `niwa-workspace-get-or-create` registers a new
  workspace. It costs a state-directory check plus at most one read or hash:
  bounded I/O, but I/O. Callers avoid creating workspaces on hot paths such as
  redisplay, timers or process filters.
- File visits are a separate, nonblocking activation path that the user enables
  with `niwa-notes-global-mode`. Presenting a file's notes first loads
  `niwa`, so the user's configuration of it, such as note keybindings,
  applies before any note overlay exists. A local roots-only
  index remembers remote workspaces whose observed or written store has notes,
  and removes roots when their store has none. The first visit under an indexed
  root queues at most one background read per Emacs run; the hook performs no
  remote I/O. This path suppresses ordinary workspace activation and installs
  only into already-visited source buffers, without source filesystem probes.
  The read requires the SSH master opened by the visit. It resolves its target
  interpreter inside the child, never waits for target program discovery, and
  disables SSH's reconnect fallback. Missing sharing, a lost master, or a failed
  read skips restoration with one debug-level diagnostic; the next explicit
  workspace operation remains the fallback. Unsupported targets are not read
  synchronously. No note content is mirrored locally.

**Format version.** The record carries `:format`, independent of the package
version. Installations sharing a workspace must understand that format. An
unknown format or an unreadable record refuses without clearing live state or
writing. An absent store is told apart from a corrupt one. An unreadable store
is reported once and read again only at the next observation point, not after
every command. A failed write is likewise not retried after every command: a
workspace waits `niwa-session-checkpoint-interval` seconds, while exit and
session saves still try. Exports carry the same `:format`, so an
export from another installation imports here. A file
written before they carried it imports only on the version that wrote it.

**Where writes come from.** A creation is written immediately; other changes
mark the workspace and are written after the command, at every session save,
and at exit. While a minibuffer is active the write waits: editing a request
there changes it on every keystroke, and it is written once the command that
read it returns. The first note in a recognized project root (VCS or project.el)
creates `.niwa/` and its store. Outside a project, an existing `.niwa/`
still permits automatic store creation; otherwise notes stay in memory with a
one-time notice that they are not saved, and no workspace state is created.
Imports do not create workspace state in arbitrary directories either.
`niwa-save-instructions` and `niwa-load-instructions`
remain export and import. An import replaces the store after its
confirmation. It is refused unless this client last observed exactly the
store's current record, so it cannot discard instructions it never saw. An
absent store, or an unreadable one being repaired, has nothing to lose.

**No old-data migration.** Resume ignores old per-session instruction snapshots.
Their presence does not block creation of a fresh current store. Old snapshots
are neither imported nor deleted; format-1 workspace stores are unsupported,
not converted or backed up by the loader. The standalone session migration
script has no instruction-store conversion step. Current-format explicit
imports retain their ordinary validation and replacement confirmation.

**Rewind.** Rewind records an intent in the store before its first
irreversible step: session, turn count before, surviving turn count, and first
discarded turn. It prunes the store only after its commit point. Before then
the existing in-memory rollback suffices, so nothing needs re-inserting. The
session's committed turn count settles the intent:

- equal to the surviving count, the Rewind committed and its note lifecycle
  is reconciled;
- equal to the count before, it did not, and the intent is dropped.

Resume settles the intent and marks the session as owing until it succeeds.
So does request admission in that session, which refuses while the intent
cannot be settled: otherwise a new turn would reuse a discarded turn number.
An unreadable store is such a failure, never "no intent". After commit,
rewinding a note conversation to zero surviving turns returns sent notes to
unsent and clears prepared changes from discarded turns. Text, anchors,
conversation links remain. Source presentations
come from the store. Without a store file, Rewind prunes the live notes in
memory the same way. Until
then Rewind holds back writes to its own workspace's store only. Changes in
that workspace are still recorded and written afterwards; other workspaces
keep writing.

**Authority is unchanged.** The store is editor-owned workspace state, like
the artifact store ([ADR 0124](0124-keep-artifacts-in-a-workspace-store.md)).
It adds no principal or publication path. NoteWrite operates through the note
API and its existing authority checks, not by granting raw store writes. Path
authorization and pinned checks stay as they were. The session lease is not
authority over workspace identities: instructions outlive every session, and
several sessions edit them at once. The store lock and hash preconditions
protect the record instead.

## Rationale

ADRs 0072 and 0091 already made directive identity workspace-owned. Keeping
the only durable copy in each session was a gap, not a boundary, and it lost
data. Resuming one session replaced the workspace's instructions with that
session's snapshot, and anything created after the last session save was lost
at exit.

One whole-set record was chosen before per-record files. Per-record files with
exclusive creation would avoid some stale merges, but they need a directory
protocol for deletions and display ids. They are deferred, not forbidden,
until evidence shows the whole-set record cannot serve the workflows.

Reconciliation after Rewind's commit point avoids exposing partially applied
workspace changes. Writes held back during Rewind are merged afterwards, so
another session's concurrent note changes survive both commit and rollback.
The intent distinguishes committed Rewind from interruption before commit.

## Consequences

- Two Emacs instances can edit one workspace's instructions without losing
  either's changes to different instructions; changes to the same one are
  refused visibly.
- Rewind does not roll back notes created by other sessions.
- A Rewind whose session is deleted before its intent settles leaves that
  intent unresolved in the store.
- Old saved sessions remain resumable as conversations, not as sources of
  workspace notes. No compatibility path reconstructs their former directives.

## Decision history

### 2026-11-12: Notes replace the record kinds and activity

Originally this store held references, directive requests, subdirective trees
and complete directive discussion, planning and attempt history. Per-UUID stale
merge had a special append rule for activity bound to the same execution
session, and committed Rewind pruned that activity. The old prune could restore
consumed subdirectives and re-derive plans, so insertion was not its inverse;
this motivated the durable intent and post-commit settlement.

ADR 0132 removes live directives and their append rule. Note conversations live
in sessions; the store retains only their links, current note state and waiting
changes. The whole-set record, lock, hash-based
merge, observation points and Rewind intent remain the storage protocol.

### 2026-11-12: Remove old-data migration

The initial store rollout waited for explicit session migration, preserving
per-session snapshots as its input rather than risking an empty replacement.
The first notes implementation also backed up and converted format-1 stores,
retaining frozen directive history. No old-format store existed in the user's
installation, and the user chose to remove old-data compatibility rather than
keep those loaders and inspectors. Resume now ignores old snapshots, a fresh
store may be created normally, and unsupported formats still refuse visibly.

### 2026-11-12: Restore known remote notes after restart

Memory-only remote activation left persisted notes invisible after restarting
Emacs until an explicit workspace operation. The roots-only local index enables
one connected-only background read without adding remote I/O to file visits or
changing steady-state note rendering. Ordinary activation and observation stay
synchronous explicit-operation boundaries.

### 2026-11-12: First-note activation in recognized projects

Previously all automatic store creation required an existing `.niwa/`
directory. Notes now persist without a start button: the first note may create
state at a recognized VCS or project.el root, and reports missing `.gitignore`
coverage without changing ignore policy. Arbitrary directories still do not
become workspaces merely because a note was created there; those notes stay in
memory and announce that limitation once.

### 2026-11-14: Remove session instruction snapshots

Sessions no longer write `instructions/current.el` or
`instructions/turn-NNNNNN.el`, publish them, copy them in a fork, or read them
at resume, Rewind or Redo. The snapshots duplicated the store as a second
source of truth: Rewind and Redo could load a historical set over the
workspace's current notes, and resume never read them. The old-data migration
that needed them was removed on 2026-11-12. Saving them cost about 3 ms at 10
annotated files and about 52 ms at 200. Rewind always settles and prunes
through the store; when no store file exists yet, it prunes the live notes in
memory. Redo settles the store for the restored turn count instead of loading
the head's snapshot. Old snapshot files stay on disk and are ignored. The user
approved the removal; `niwa-save-instructions` remains the export command.

### 2026-11-15: File-visit activation becomes an opt-in global mode

The visit hook was added to `find-file-hook` by an autoload cookie, so reading
the package autoloads changed every file visit, with no command to turn it off.
The Elisp coding conventions require that loading a package not change editing
behavior. It also let notes appear before `niwa` loaded, so keybindings set in
the user's `niwa` configuration missed the first note overlays. The hook now
belongs to `niwa-notes-global-mode`, which users enable in their init; its
visit check stays dependency-free and still needs no chat. Presenting notes now
loads `niwa` first. The user chose this over a default-on option that keeps
the autoloaded hook.
