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

**One interface.** Every place that discusses an item shows the same
discussions sidebar, one shared viewer component
(`relay/viewer/viewer-discussions.js`). It shows either the list of the item's
discussions -- the whole item first, then the open ones, then the resolved
ones folded into one row, with queued/running assistant work visible on the
affected row -- or one discussion with its messages, the
assistant's answers beneath them, and one box at its foot that replies to that
discussion only. **Discuss** picks the part and opens a new discussion that
looks like any other before its first message. Every message carries an **Ask
assistant** choice, the ⋯ menu resolves or reopens, and **Ask assistant
again** sends a discussion as it stands. An answer or message arriving in a
discussion that is not on screen marks its row, the back link, its marker and
the sidebar's toggle; it never switches the view. The HTML artifact panel and
the whiteboard and document editor differ only in how a part is picked and in
what each target offers, such as file attachments on the whole item.

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

The sidebar replaced a mixed layout after the user compared mocks of both
models. The editor had one composer at the panel's foot whose chip switched
between writing to the whole item and starting a discussion about the
selection, and each discussion's card carried its own reply box; the artifact
panel floated a composer and pinned cards over the frame. On a 1280 × 640
window the composer took about 40% of the sidebar, a long answer stretched the
card list, and three kinds of box made it unclear which one a message would
reach. Showing one discussion at a time gives every box one visible owner and
an answer the full height. Marking news rather than jumping to it keeps a
participant's place while they type; the user chose marks over jumping.

## Consequences

- Destructive: whiteboard and document comments are dropped on the item's
  next commit, and an HTML artifact's `comments.json` is removed when its
  discussions are first written. The user chose no migration.
- Earlier transcript records attributed to a comment carry no discussion and
  message identity; their answers show at the end of the general discussion.
- Answers are placed by the attribution's message identity, so the editor and
  the room no longer infer the answered message from the discussion's version.
- The HTML sidebar reads bounded canonical item history rather than relying
  on the room's live projection. Live-only input lost answers at compaction
  and provided none in the lobby. A room reads its own session's item turns;
  the lobby follows the selected discussion's recorded answering session or
  existing dedicated session, never an aggregation of project conversations.
  Inspection does not resume or create sessions, and view links retain read
  access. The newest complete turns fit within 128,000 characters, with explicit
  omission and unavailable-history notices. Five-second polls use cached source
  identities and digest/unchanged responses to avoid repeated archive projection.
- A lobby discussion whose dedicated conversation has a pending interaction
  shows **Waiting for a decision in NAME**, with an explicit **Open
  conversation** for the existing full/owner Conversation authority. A pending
  Ask or approval must not automatically share that session. History exposes
  only a pending flag and title, never the interaction body or a bearer;
  opening uses the existing store action at the requester's tier. The digest
  observes interaction changes independently from the cached transcript.
- Retry idempotency checks the queue, the live transcript and the archived
  segments for every kind, as editor questions did before, so a retry after
  compaction is not queued twice. Reading archives costs time proportional to
  the session's age -- 126 ms per request with ten 300 KB archives when every
  request paid it -- so the check runs only when the request may be a retry:
  its message was already stored before this action, which the artifact store
  sees directly and the editing host reports from its operation receipts. An
  explicit **Send to assistant** always checks. A match in any readable
  archive settles a retry. Without one, an unreadable archive leaves delivery
  unknown, so the retry is refused to its sender instead of being queued a
  second time; a new message never reads archives and is unaffected.
- A retried message is sent only while it is still its discussion's latest;
  otherwise the sender reviews the discussion again, for every kind.
- `shared://ID/discussions` replaces `shared://ID/comments` for models.
- A message to the whole item carries no reviewed capture: the sidebar shows
  no quote of the whole item, so the host captures it when the request is
  read. Only parts of an item, whose quote the sender sees, are checked
  against what was reviewed.
- Saved discussion drafts from the composer format are reported as
  unsupported once and reset; no migration.

## Decision history

- Until the shared sidebar, the editor's Discussions tab held one composer
  with a scope chip (**Whole whiteboard** or **Selected objects**), a
  **Resolved** filter, and a card per discussion with its own reply form and
  **Send to assistant**; the artifact panel floated its composer and thread
  cards over the frame, with a **Whole artifact** button for the general
  discussion. Replaced by the list-then-discussion sidebar for the reasons
  above.
