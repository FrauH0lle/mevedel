# Buddy remarks are workspace notes

Status: accepted

The shared note concept, conversations and closing lifecycle are defined by
[ADR 0132](0132-notes-replace-references-directives-and-buddy-notes.md).
This record owns Buddy's authorship, review authority and batched persistence.

## Current decision

Buddy remarks are ordinary workspace notes with author `buddy`. They use the
same identity, source anchor, label and workspace instruction-store record as
user notes. Buddy's request-local index is rebuilt from that canonical store;
it is not another durable database. The review tools still only read captured
source and maintain remarks. Buddy never edits source code or curated memory.

A subsequent review receives the existing remarks and can update or retract
them. Retraction removes the record outright: a resolved concern is not a user
deletion, and may be raised again if it returns. Keeping a remark changes its
author to `user`, preserving its identity,
text and kept-from-Buddy metadata. Buddy no longer owns it. A user edit or keep
while a review runs also invalidates the review's older mutation authority.
Sending or taking a Buddy remark first performs the same keep operation. The
conversation or taken goal therefore belongs to the user, even if a previously
started review later tries to retract the original remark.
Dismissal uses the shared closed-note lifecycle with reason `dismissed`, not
separate Buddy persistence. Recently closed count/age limits bound how long
that rejected advice remains available to the model.

## Rationale and alternatives

Purely ephemeral annotations repeat the same advice after every restart and
require a second set of source actions and rendering rules. Once notes have a
shared workspace identity and explicit authorship, that separation no longer
earns its cost. Persistence keeps earlier advice available to the next review;
an exact repeat on the same line reuses the existing note rather than adding
another label. Prompt context remains bounded, without capping open remarks.

Store writes are batched at the review boundary, including a review abandoned
after successful note tool calls. Measurements on a real SSH/TRAMP workspace
found five separate writes took 2,150 ms median versus 435 ms for five additions
and one checked sync (seven samples each). Per-remark writes would impose that
avoidable remote cost. Batching reuses the public store's lock, hash precondition,
merge and failure-retry behavior; it does not introduce another writer.

## Consequences

- Buddy remarks carry `niwa-instruction-type note` and persist through the
  ordinary note record codec; no directive activity codec or migration is used.
- The shared label renderer replaces Buddy's separate current-line/eol layout
  and its per-line relayout hook. No new rendering timer is introduced.
- Closing a source buffer drops only Buddy's transient review entries. The
  store retains the note for the next visit or Emacs process.
- Review ownership, exact-buffer allowlisting and bounded captured markers
  remain mandatory. Shared note storage grants no source-editing tool.

## Decision history

Originally Buddy notes deliberately omitted `niwa-instruction`, making them
invisible to instruction navigation and persistence. That avoided teaching
every binary reference/directive path to skip ephemeral annotations. The notes
redesign reverses that choice: one persistent note concept now has explicit
authorship, and restart-safe advice and common actions are required. The old
structural exclusion would prevent those capabilities rather than protect them.

Buddy was ported from [llm-buddy](https://github.com/ahyatt/llm-buddy). The port
avoided adding its `llm` provider abstraction beside gptel. At the time of that
comparison, the upstream forced-tool loop needed an explicit end tool, and its
buffer read/note placement lacked niwa's bounded buffer scope and captured
markers. Niwa chose ordinary no-more-tools settlement, request-local tools,
workspace scope from default-directory, and bounded marker-based reads/notes.
Those observations describe the port's motivation, not a claim about every
current upstream release.

The upstream replace-content auto-fix path was not ported because niwa already
has permission-gated patch proposals and review. Passive annotations also do not
write durable memory: the removed Tutor experiment had accumulated an unread
hints file indefinitely. Durable guidance uses explicit instruction/memory
workflows; Tutor removal is recorded in
[ADR 0070](0070-compose-system-prompts-from-ordered-profiles.md#decision-history).
