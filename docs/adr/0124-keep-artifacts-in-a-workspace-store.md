# Keep artifacts in a workspace store that sessions attach to

Status: accepted

Amends [ADR 0099](0099-project-live-collaboration-from-host-authoritative-state.md)
(what a room exposes), [ADR 0120](0120-edit-shared-content-through-the-session-host.md)
(where shared content is edited and stored) and
[ADR 0122](0122-let-full-links-change-project-files-directly.md) (the lobby as
the project's view).

## Current decision

**The workspace artifact store is the original.** Every artifact, whiteboard
and document lives once in `<workspace>/.mevedel/artifacts/ID/`, the directory
name being its stable id. Sessions do not own artifacts; they attach to them.
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
records, target clock, heartbeat and expiry, decides who commits; each write
proves it in the same target program. Another Emacs sees the item read-only
and asks the holder to hand it over, which happens once the holder's queue for
it is idle and the item has had no edit there for 10 seconds; a lease whose
holder stopped renewing is taken over only by the artifacts cockpit's
takeover command, after confirmation, never by an edit. A version is recorded when
a turn that edited the item settles, and on **Save version**; restoring one is
an ordinary, revertible edit, so the Yjs lineage and concurrent editors
survive.

**Versions.** Up to 20 per artifact, following Claude, and at most 64 MiB of
versions per artifact (configurable; Claude publishes no such cap). The
oldest go first; the latest always stays.

**Store contents are durable work, not machine state.** Whether the store is
committed to Git is the project's choice; mevedel adds no ignore rule. Item
leases therefore live outside it, under `.mevedel/leases/`. The store's
bookkeeping -- metadata, item state, comments, versions -- and the leases are
read-only to model tools by default, so edits cannot bypass leases, versions
and validation; only an artifact's own files are written with ApplyPatch.
Confined Bash cannot write in the store at all.

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
whiteboards and documents, and open an artifact's conversation. The lobby
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

The item lease reuses the session durability generation primitives instead of
a shared lease core. The session lease functions bind publication,
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
  command could make. The queue now never asks; the cockpit's `T` makes
  that decision.
- **Hand-over while editing.** At first the holder handed an item over
  whenever its queue was empty at the renewal tick. Saves arrive every
  300 ms while someone draws, so the queue is empty most of the time: a
  person drawing lost the lease within one renewal of another Emacs's
  request, and two Emacs instances editing at once passed it back and
  forth. The holder now also waits until the item has had no edit for
  10 seconds.

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
