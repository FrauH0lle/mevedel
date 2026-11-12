# Keep instructions in a workspace store

Status: accepted

Amends [ADR 0072](0072-make-rewind-in-place-undo.md) (where Rewind's
directive pruning is applied) and the per-session instruction snapshots that
[ADR 0091](0091-render-directive-turns-in-the-shared-session-view.md) left as
the only durable copy of workspace-owned directives.

## Current decision

**The workspace owns its current instructions in one store.** References,
directives, their requests, anchors, subdirective trees, activity and the
display-id state live in `<workspace>/.mevedel/instructions.el` on the
execution target. Sessions keep their `instructions/turn-NNNNNN.el` snapshots
as history, plus `current.el` until the store has proven itself. Resume loads
the store; it no longer loads or clears the workspace set from a session.

**One whole-set record, refused when stale.** The store is read and written
through the pinned control-file programs. Writes hold a lock on
`.mevedel/leases/instructions/` and name the hash they were based on. A stale
write is merged per instruction UUID against the record this client last
observed:

- a change to an instruction nobody else changed applies;
- a creation applies if its parent still exists, and a colliding display id
  is renumbered above every used and retired id;
- an activity append applies if the directive is still bound to the same
  execution session; an identical entry counts as appended, and the same
  sequence with different contents is a conflict;
- any other change to an instruction another client changed is refused, never
  reapplied. Its request text goes to the kill ring; other refused changes are
  reported as not recoverable.

A write is retried once; if the store changed again, this client keeps the
store's record and refuses its own changes.

Installing a merged or observed record keeps directive record identity. A
record already live under the same UUID takes the merged fields in place, and
subdirectives likewise. A record the store no longer holds leaves the
workspace's list. Running requests, open listings and Rewind hold record
objects, so they see the merged state. While a request is in flight on a
record, its transient status and Plan stay as they are.

**Observation points.** The store is observed when a workspace is first
activated in an Emacs, when a directive listing opens, at resume, and on every
write. There is no watcher: a listing left open may show state another client
has since changed.

- An unchanged store costs one target-side hash.
- A workspace with nothing in memory loads the store, restoring presentations
  only in files already visited; the rest restore when visited.
- Any other difference merges as a write would.
- Activation skips a remote root without a live connection, since resolving a
  workspace must not open one; the next listing or resume observes it.
- Activation happens when `mevedel-workspace-get-or-create` registers a new
  workspace. It costs a state-directory check plus at most one read or hash:
  bounded I/O, but I/O. Callers avoid creating workspaces on hot paths such as
  redisplay, timers or process filters.

**Format version.** The record carries `:format`, independent of the package
version, so two installations of different versions share one workspace. An
unknown format or an unreadable record refuses without clearing live state or
writing. An absent store is told apart from a corrupt one. An unreadable store
is reported once and read again only at the next observation point, not after
every command. A failed write is likewise not retried after every command: a
workspace waits `mevedel-session-checkpoint-interval` seconds, while exit and
session saves still try. Exports and session snapshots carry the same `:format`, so a
candidate staged on the other machine's installation imports here. A file
written before they carried it imports only on the version that wrote it.

**Where writes come from.** A creation is written immediately; other changes
mark the workspace and are written after the command, at every session save,
and at exit. While a minibuffer is active the write waits: editing a request
there changes it on every keystroke, and it is written once the command that
read it returns. The first note in a recognized project root (VCS or project.el)
creates `.mevedel/` and its store. Outside a project, an existing `.mevedel/`
still permits automatic store creation; otherwise notes stay in memory with a
one-time notice that they are not saved, and no workspace state is created.
Other instruction creation and imports do not create workspace state either.
`mevedel-save-instructions` and `mevedel-load-instructions`
remain export and import. An import replaces the store after its
confirmation. It is refused unless this client last observed exactly the
store's current record, so it cannot discard instructions it never saw. An
absent store, or an unreadable one being repaired, has nothing to lose.

**Waiting for migration.** A workspace without a store whose sessions exist,
or whose migration staged candidates, is pending. It keeps the behaviour from
before the store: resume restores the session snapshot, sessions keep writing
snapshots, and automatic store creation is refused with one warning.
`mevedel-load-instructions` on a candidate creates the store. Pending never
starts empty. If it did, the next session save would overwrite that session's
snapshot, the migration's only input for it, with an empty one. When another
client creates the store while this one is pending, this client exports its
instructions to `.mevedel/instruction-migration/` and then loads the store.

**Rewind.** Rewind records an intent in the store before its first
irreversible step: session, turn count before, surviving turn count, and first
discarded turn. It prunes the store only after its commit point. Before then
the existing in-memory rollback suffices, so nothing needs re-inserting. The
session's committed turn count settles the intent:

- equal to the surviving count, the Rewind committed and its activity is
  pruned;
- equal to the count before, it did not, and the intent is dropped.

Resume settles the intent and marks the session as owing until it succeeds.
So does request admission in that session, which refuses while the intent
cannot be settled: otherwise a new turn would reuse a discarded turn number.
An unreadable store is such a failure, never "no intent". After commit the
live directive records stay the objects Rewind pruned, and source
presentations come from the store rather than the historical snapshot. Until
then Rewind holds back writes to its own workspace's store only. Changes in
that workspace are still recorded and written afterwards; other workspaces
keep writing.

**Authority is unchanged.** The store is editor-owned workspace state, like
the artifact store ([ADR 0124](0124-keep-artifacts-in-a-workspace-store.md)).
It adds no principal, no model write tool and no publication path. Path
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

Pruning before Rewind's commit point, with re-insertion on rollback, was
investigated and rejected. The prune also returns consumed subdirectives and
re-derives the Plan, so re-inserting records is not its inverse. A concurrent
write between prune and rollback would defeat a byte restore. A session's
committed turn count alone cannot tell a discarded turn from a lost one, whose
attempt can carry the next turn number without a commit. The intent supplies
that distinction.

## Consequences

- Two Emacs instances can edit one workspace's instructions without losing
  either's changes to different instructions; changes to the same one are
  refused visibly.
- Rewind no longer rolls back references created by other sessions.
- A Rewind whose session is deleted before its intent settles leaves that
  intent and the discarded activity in the store.
- Existing instructions move once, through the standalone
  [session migration](../sessions.md#workspace-instructions), keyed by UUID.
  Differing revisions of one instruction, conflicting activity, activity
  without durable turn evidence, and inconsistent trees stage the workspace.
  The sessions are never changed. Activity from deleted sessions has no
  evidence, so it stages too.
- Activity is written when its turn's session save runs. If Emacs crashes
  between recording an attempt and that save, the attempt can reach the store
  without its turn committing. It then keeps a turn number the next turn
  reuses, as a lost turn's attempt already did in memory.

## Decision history

### 2026-11-12: First-note activation in recognized projects

Previously all automatic store creation required an existing `.mevedel/`
directory. Notes now persist without a start button: the first note may create
state at a recognized VCS or project.el root, and reports missing `.gitignore`
coverage without changing ignore policy. Arbitrary directories still do not
become workspaces merely because a note was created there; those notes stay in
memory and announce that limitation once.
