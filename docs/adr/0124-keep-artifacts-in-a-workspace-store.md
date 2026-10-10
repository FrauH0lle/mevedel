# Keep artifacts in a workspace store that sessions attach to

Status: accepted

Amends [ADR 0099](0099-project-live-collaboration-from-host-authoritative-state.md)
(what a room exposes), [ADR 0120](0120-edit-shared-content-through-the-session-host.md)
(where shared content is edited and stored) and
[ADR 0122](0122-let-full-links-change-project-files-directly.md) (the lobby as
the project's view).

## Current decision

**The workspace artifact store is the original.** Every artifact, whiteboard
and document has a stable id under `<workspace>/.mevedel/artifacts/ID/`.
Authored files live there; metadata, versions, comments and live editor state
live under the protected sibling `.mevedel/artifacts/.state/ID/`.
Sessions do not own artifacts; they attach to them.
One artifact can be attached to several sessions, and one session can attach
several artifacts. A session persists only its attached ids; Fork and Save As
carry that list, so a fork points at the same artifacts, and **Duplicate** is
the explicit way to get an independent copy. Deleting a session leaves its
artifacts.

**Attaching.** A session is attached when its model creates or edits the
artifact, when someone in its room creates or edits a whiteboard or document,
when a comment reaches it (from its room, or as the dedicated session), when
it duplicates the artifact, or when the user adds it (cockpit `a`, the room's
**Attach**).

**HTML, Markdown and images** change in discrete model writes with ApplyPatch.
ApplyPatch matches its hunks against current content, so a write planned on an
older copy fails and the model rereads: the concurrency check, without a
lease. A write into a new id directory creates the artifact; a turn that
wrote its primary file records one version when it settles.

**Whiteboards and documents** are one live object edited continuously by
people and the model, so they are edited in place in the store. One editing
queue per workspace in each Emacs serves every room and session there.
Across Emacs instances an item lease, with the session lease's generation
records, target clock, heartbeat and expiry, decides who commits. Lease
transitions and state/metadata writes share a target-side `flock` on the
stable `.mevedel/leases/artifacts/` directory; each write proves ownership,
latest generation and expiry while holding that lock. Another Emacs sees the item read-only
and asks the holder to hand it over, which happens once the holder's queue for
it is idle and the item has had no edit there for 10 seconds; a lease whose
holder stopped renewing, such as a suspended laptop's, is taken over by the
next edit. Writes replace the whole item and each is fenced by the lease, so
a late write from the old holder fails instead of overwriting, and its
browsers reload the item. A version is recorded when
a turn that edited the item settles, and on **Save version**; restoring one is
an ordinary, revertible edit, so the Yjs lineage and concurrent editors
survive.

**Versions.** Up to 20 per artifact, following Claude, and at most 64 MiB of
versions per artifact (configurable; Claude publishes no such cap). The
oldest go first; the latest always stays. Version publication compares the
previous index under the target mutation lock and retries concurrent changes.
ApplyPatch versions use the committed bytes from that write.

**Store contents are durable work, not machine state.** Whether the store is
committed to Git is the project's choice; mevedel adds no ignore rule. Item
leases therefore live outside it, under `.mevedel/leases/`. The store's
bookkeeping -- metadata, item state, comments, versions -- lives in the
separate `.state/` subtree. That subtree and the leases are read-only to model
tools by default, so edits cannot bypass leases, versions and validation.
Authored files remain writable with ApplyPatch and confined shell commands.

**Dedicated session.** Each artifact may have one dedicated session, created
on first use for conversation started outside any chat; the two name each
other, and only while they agree does the session count as the artifact's.
It is an ordinary session, hidden from session lists and the default chat target, reachable from
the artifact as its conversation, and exempt from expiry while the artifact
exists. Deleting the artifact closes and deletes it, unless a turn is running
there; one that cannot be deleted yet stays as an ordinary session.

**Comment routing** follows where the comment is written: in a chat's room, to
that chat, whose links speak for it alone; from the lobby, to the session
that answered the thread, shown as "answered in", or else the dedicated
session.

**Access.** View links list and open artifacts and list versions. Full and
owner links also edit, comment, restore, attach, duplicate, delete, create
whiteboards and documents, and open an artifact's conversation, which they
start on first use. ADR 0099 lets a full link start a new session only with
an owner's or Emacs's approval; an artifact's conversation is no new chat of
the user's but part of working on that artifact, which a full link may do in
full, so it needs no approval. It starts like any new session. The lobby
shows the store in an **Artifacts** tab, visible to view links unlike
**Files**. `shared://` lists every whiteboard and document of the workspace and
marks those attached to the session.

## Rationale

Artifacts, whiteboards and documents lived inside one session:
`<session>/artifacts/`, carried by that session's lease, publications, Fork
and Save As. They outlived the conversation that made them, but could not be
found, opened, commented on or continued from anywhere else; a deleted session
took them along, and the lobby, which is the project's view, could not show
them. Two rooms editing one board had two queues.

Two alternatives were rejected. Moving the folder to the workspace unchanged
broke the session lease model for concurrently edited boards and left comment
conversations without an owner. Keeping session originals with a one-way
project copy made two things to understand ("which one is real?") and needed
a staleness indicator on every published item.

Claude's artifact model, checked against support.claude.com and
code.claude.com/docs/en/artifacts in October 2026, was the reference: one
account-level object with a stable identity; sessions create or update it;
another session updates it only once attached; a publish built on an older
copy is refused and redone; comments live on the artifact and reach the
session working on it. Its Compliance API retains "up to roughly 20" versions.

The item lease reuses the session durability record codec and target clock,
with artifact-local generation transitions under the store mutation lock,
instead of a shared lease core. The session lease functions bind publication,
unsettled-mutation, release-pending and transfer state into about 610
race-critical lines; extracting a holder-neutral core would have put hooks
into all of them for an item lease that needs only acquire, renew, release,
hand-over and a fenced write. A plain lease file written by atomic rename was
also rejected: rename is not a compare-and-set, and local clocks disagree
across machines.

One editing queue per workspace, not per item, serializes unrelated items in
one Emacs. It is the smallest change from the per-session queue that gives
every room one queue per item, and nothing so far shows unrelated boards
waiting on each other.

Protected globs for the store's bookkeeping would each have cost a full
workspace walk before every Bash launch. Patterns below `**/.mevedel/`, which
exists only at a workspace root, therefore protect in the Bash sandbox the
whole directory their literal part names (`.mevedel/artifacts`,
`.mevedel/leases`) at each discovery root, without a walk; a `.mevedel` nested
deeper is covered by native tool checks but not by the Bash sandbox.

Versions are recorded once per turn, for files as for whiteboards and
documents. Recording one per ApplyPatch let a turn of twenty small patches
push out every earlier version, including the state before the turn.

## Consequences

- Session persistence carries no artifact bytes: the `artifacts/` subtree left
  portable publications, Fork staging, Resume and Save As. The migration drops
  a converted session's old `artifacts/...` entries.
- A room exposes artifacts by store identity, not only through its own
  transcript records: any link to the workspace can open any artifact. This
  deliberately widens ADR 0099's "published record is the authority".
- Deleting an artifact deletes its versions, comments, lease and dedicated
  session; deleting a session deletes none of them.
- Existing per-session artifacts move once with an explicit migration script, a
  user-requested exception to the no-migration rule, which also converts every
  session to the v0.5.11 sidecar format that records attached ids. Transcript
  cards in old segments that name pre-migration paths show as missing.
- Comments on an HTML artifact are not serialized across Emacs instances; the
  later write wins.

## Evidence

- Claude artifacts: https://support.claude.com/en/articles/17153992,
  https://code.claude.com/docs/en/artifacts
- Version retention: https://platform.claude.com/docs/en/api/compliance/code/artifacts
  (`versions`: "Up to roughly 20 most-recently-published versions").

## Decision history

- **Taking over an expired item lease.** At first an edit that met an
  expired foreign lease asked for confirmation when its caller could have
  asked. The question ran from the editing queue's timer, so it blocked
  every item of the workspace until answered, and a browser request, which
  cannot ask, was refused with "needs a decision in Emacs" that no Emacs
  command could make. A cockpit command then made that decision, but a
  suspended laptop -- the everyday two-machine setup -- still blocked every
  desktop edit of its boards, model edits included, until someone used it.
  Confirmation protects nothing here: unlike a session, an item has no
  unsettled mutations, its writes replace it whole, and each is fenced. The
  next edit now takes an expired lease over.
- **Hand-over while editing.** At first the holder handed an item over
  whenever its queue was empty at the renewal tick. Saves arrive every
  300 ms while someone draws, so the queue is empty most of the time: a
  person drawing lost the lease within one renewal of another Emacs's
  request, and two Emacs instances editing at once passed it back and
  forth. The holder now also waits until the item has had no edit for
  10 seconds.
- **Pin authored files during store operations.** A deterministic late-symlink
  probe made restore's ordinary file copy overwrite a file outside the workspace;
  replacing duplicate's destination parent similarly redirected its copy.
  Restore now writes the primary file and records the same captured bytes under
  the store mutation lock. Duplicate reads and writes use the existing pinned
  filesystem program, as do version reads without supplied ApplyPatch bytes.
  Authored writes use its inline writer: staging temporary payloads by pathname
  inside a mutable authored directory would reopen the race. A second probe
  redirected the inline writer's own temporary pathname, so that writer now
  proves its opened file descriptor before writing and changing modes through
  it. Duplicate preserves file modes and applies directory modes after copying
  descendants. Protected version and metadata writes retain staging, and
  supplied patch bytes avoid rereads. Concurrent authored edits remain allowed;
  these proofs prevent redirected access outside the authorized paths rather
  than locking authored files against other writers.
- **Prove shared reads on the target.** Initially a shared read walked the
  same physical paths eight times through TRAMP before opening the files. On
  provisioned SSH this cost 3.77 seconds per read, and two concurrent document
  edits exceeded the browser's convergence deadline. Reading metadata and state
  in one pinned target program reduced the same read to 0.155 seconds. Commits
  likewise use their target proofs instead of repeating host-side walks; tests
  still reject linked ancestors and linked metadata or state leaves.
- **Separate authored files from bookkeeping.** Initially bookkeeping lived
  beside authored files in each artifact directory. A real confined-shell probe
  demonstrated that per-file wildcard mounts protected existing metadata but
  allowed creating missing state, comments, versions and metadata for new ids.
  Mounting the entire authored directory read-only would also prevent new assets
  and atomic replacement. Bookkeeping now lives under one protected `.state/`
  directory, whose mount covers existing and future ids. Launch preparation
  creates missing protected directories and their parents and mounts only the
  leaf. These directories persist: a concurrent-shell probe showed that cleaning
  up the first launch's empty state root detached a second active sandbox's mount,
  allowing that sandbox to recreate a writable root. Permanent directories avoid
  cross-process cleanup coordination and preserve authored-file operations without
  a workspace scan or per-artifact mount list.
- **Fenced item commits.** Initially item writes verified the remembered
  generation's bytes before writing, using the session generation primitives
  for acquisition and renewal. Review found that a newer generation could
  appear between that proof and the state write, and that an unpruned newer
  generation or an expired lease did not prevent an old write. Item lease
  changes and commits now share the existing target-program lock capability,
  with explicit latest-generation and expiry proofs. The stable parent lock
  survives deleting an item, and metadata title changes join the same commit
  program so a previous holder cannot overwrite its successor's title.
- **Concurrent publication.** Two Emacs processes originally read the same
  version index and published the same next number, losing one writer's
  version. Version and metadata changes now compare their previous bytes
  while holding the target mutation lock. Dedicated conversations save before
  claiming their metadata slot; a losing creator discards its unused session
  and opens the winner. This preserves concurrent updates without adding a
  separate coordination service.
- **Deleting with an open conversation.** At first, deleting an artifact was
  refused while its dedicated session was open in Emacs or could not be
  deleted, so a buffer could not save the session straight back. In use, that
  made a whiteboard opened from the lobby undeletable from the browser:
  opening it opens its dedicated session for the room it is edited in. A
  conversation with captured turns also stays pinned until journal capture
  finishes, which blocked the artifact too. Deletion now closes an idle open
  conversation and keeps one it cannot delete yet as an ordinary session; only
  a running turn still refuses it.
- **Who records the conversation.** At first only `meta.el` named its
  dedicated session, and finding the dedicated sessions meant reading every
  artifact's metadata -- on each default-chat lookup, lobby listing and
  session chooser, 0.4 s at 50 artifacts over TRAMP. A `meta.el` copied
  outside mevedel also made deleting the copy delete the original's
  conversation. The session now records its artifact too, and only a pair
  that agrees counts.
- **Replies from another room.** A reply first followed the session that
  answered its thread from any room. Since comments reach every room, a full
  link to one chat could then prompt -- and resume -- any session that had
  ever answered a thread, under that session's permission mode, which ADR
  0099's one-room-per-session links do not allow. A room now answers in its
  own session; only the lobby, which may open any session, follows threads.
