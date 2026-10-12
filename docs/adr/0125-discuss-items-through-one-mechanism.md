# Discuss items through one mechanism

Status: accepted

Amends [ADR 0120](0120-edit-shared-content-through-the-session-host.md)
(the editor's separate comments and assistant views, and resolution as an
explicit participant action) and
[ADR 0124](0124-keep-artifacts-in-a-workspace-store.md) (comment routing,
which now covers every item kind).

## Current decision

**One model.** Everything said about an HTML artifact, a whiteboard or a
document is a discussion: a target and its messages. A target is part of the
item or the whole item. Every item has one general discussion about the whole
item, which always exists and stays open; a discussion about part of an item
starts with its first message and can be resolved and reopened. A message
either stays between people or also asks the assistant, which answers beneath
it. A room message with an item selected is a message to the assistant in its
general discussion.

**One interface.** Every place that discusses an item offers the same
actions: **Discuss** picks the part, one composer writes about it with an
**Ask assistant** choice, replies carry the same choice, **Send to assistant**
sends a discussion as it stands, and **Resolve** hides it. The HTML artifact
panel and the whiteboard and document editor differ only in how a part is
picked.

**One owner in Emacs.** `mevedel-collaboration-discussions` performs every
guest action -- `list`, `post`, `reply`, `resolve`, `ask` -- for every item
kind, routes messages to the answering session, queues them with
model-invisible attribution naming the discussion and the message, and
resolves a selection sent alone once its turn completes. Storage follows the
kind: an HTML artifact's discussions live in the store's protected state as
JSON, a whiteboard's or document's in the item on its editing host, which
checks each target against the content the sender reviewed and keeps targets
current as content changes. The artifact panel reaches the module through a
room frame, the editor through its editing channel. The editor runs in its
own sandboxed frame that speaks only that channel, which already chunks
large payloads such as attachments; both channels end in the same function
with the same arguments.

**Routing** is ADR 0124's comment routing for every kind: in a chat's room, to
that chat; from the lobby, to the session that answered the discussion, or
else the item's dedicated session.

**A selection sent alone resolves itself.** Sent with no message, the
selection's own content is the request; the discussion is a task, and once
the turn that answered it completes it is resolved, attributed to the
assistant. A failed or cancelled turn leaves it open, and a discussion with a
written message is never resolved automatically.

## Rationale

The user found the two pathways confusing. The editor had **Add comment** and
**Ask about selection** for the same selection, **Comments** and **Assistant**
tabs with separate drafts, and two ways for an answer to appear; the HTML
artifact panel had a third variant. All three carried a target, reached the
same queue and the same item conversation, and differed in where the result
was shown. Behind them were three code paths: editor questions
(`mevedel-collaboration-editing--ask` with its own idempotency lookup and
selection wording), host-side comment actions with their own anchors and
limits, and `mevedel-collaboration-artifact-comments` with its own JSON
store. A participant had to choose a pathway before writing and could not
turn a question into a discussion afterwards.

One model makes "who should answer" a choice on the message, not a choice of
feature. The general discussion keeps whole-item questions with the item, as
artifact room messages already were.

Rejected alternatives:

- **Restyle both pathways but keep them.** Two concepts and two drafts remain
  to be learned, and the code paths stay duplicated.
- **Only assistant conversations, no people-only notes.** The user chose to
  keep notes between people, with replies and resolution, so a message can
  leave the assistant out.
- **Store every kind's discussions in Emacs JSON.** Document targets are Yjs
  relative positions and board targets need object fingerprints; only the
  editing host holds the live content to keep them current and to check the
  hash of what the sender reviewed. HTML artifacts have no such host, so their
  targets carry selectors and text fingerprints instead.

Auto-resolving selections sent alone reverses ADR 0120's "resolution remains
an explicit participant action" for that case only. Once every request about
a selection is a discussion, each handwritten "draw it" would otherwise leave
an open discussion and a board pin behind with nothing left to discuss; the
user accepted resolving those, and only those, on completion.

## Consequences

- Destructive: whiteboard and document comments are dropped on the item's
  next commit, and an HTML artifact's `comments.json` is removed when its
  discussions are first written. The user chose no migration.
- Earlier transcript records attributed to a comment carry no discussion and
  message identity; their answers show at the end of the general discussion.
- Answers are placed by the attribution's message identity, so the editor and
  the room no longer infer the answered message from the discussion's version.
- Retry idempotency checks the queue, the live transcript and the item's
  archived turns for every kind, as editor questions did before, so a retry
  after compaction is not queued twice.
- `shared://ID/discussions` replaces `shared://ID/comments` for models.
