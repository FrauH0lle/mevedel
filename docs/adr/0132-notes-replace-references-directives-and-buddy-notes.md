# Notes replace references, directives and Buddy notes

Status: accepted. Supersedes the directive decisions of
[ADR 0087](0087-keep-directive-identity-outside-source-overlays.md),
ADR 0088 (recorded in
[ADR 0091's history](0091-render-directive-turns-in-the-shared-session-view.md#decision-history))
and [ADR 0090](0090-enforce-discussion-as-a-read-only-capability.md).
Complements [ADR 0108](0108-buddy-notes-are-not-instructions.md)'s Buddy
authorship and persistence decision and narrows
[ADR 0130](0130-keep-instructions-in-a-workspace-store.md)'s record scope to notes.

## Current decision

**There is one kind of mark: the note.** A note is marked code with optional
text, written by the user, an agent or Buddy. It can be a comment, context
through an explicit mention, or a request. Its state is unsent, sent, taken or
closed (done, deleted or dismissed). No command creates a reference or directive
record, and no live request path dispatches one.

**Notes are workspace records.** They use the workspace instruction store
of ADR 0130 and are visible across its sessions. In a recognized project, the
first note creates workspace state; outside a project without existing state,
notes stay in memory with a notice. A record holds identity, display number,
anchor, text, tags, author, state, conversation session identity once sent, at
most one waiting prepared change and Buddy metadata. Deleting marked text
leaves an empty anchor; a missing file shows as source missing. Identity and
text survive either case. Conversations stay in their sessions, not the store.

**A note reaches a prompt only when sent or mentioned.** `@note:N`,
`@note:{tags}` and equivalent `@ref` forms explicitly attach notes. Numeric
mentions bind identity; tag queries select current matching open notes at
dispatch. Empty or malformed queries attach nothing. Notes are not implicitly
included as chat context. Buddy separately receives notes as review context
for its invited watch and remark lifecycle.

**Send opens an isolated note conversation.** It is its own durable session,
starting with the note text, code captured at send time, explicit attachments
and normal project instructions and capabilities, never another conversation's
history. Journal digests, the edited-files reminder and an ambient Goal are not
inherited. Several notes can share one batch session. The initial provider input
is retained for inspection. Reopening resumes the same conversation.

The ordinary session view and composer appear in an anchored child frame, using
the presentation mechanism of [ADR 0106](0106-directive-frame-is-a-child-frame.md),
or a side window when child frames are unavailable. There is no per-note
Discuss, Implement or Plan action: session permissions and Plan mode govern
tools. Ask requires approval for mutations; it is not a read-only mode. Plan
mode restricts project writes through the normal tool boundary, while allowing
session-owned plan files and new review notes. The original note session's
preset is fixed; model, effort and permission mode remain changeable. Explicit
forks are ordinary chats and may change preset.

**Source ApplyPatch proposals wait as prepared changes.** Preparation returns
without writing source or holding the model turn open. The waiting record
retains the proposal and base file bytes and is shared by batch siblings. Apply
preserves disjoint later edits and refuses conflicting or unsaved edits. The
user can re-ask on current code or resolve in a separate three-way smerge
buffer; generated conflict markers never enter the source file. Confirmation
rechecks current source before writing. Resource-address patches retain their
ordinary ApplyPatch behavior; preparation is not a general sandbox for Bash,
Eval or other tools.

A newer proposal replaces the waiting one, while older proposals remain in
conversation history. Applying records the actual file changes under the
originating session turn, and clears the waiting record from batch siblings.
Applied changes can be reverted individually with the shared checked reversal
engine of [ADR 0131](0131-rewind-reverts-recorded-changes.md).

**Take hands implementation to the user.** Buddy watches the taken note's code
using its text as brief at every dial level, including Quiet. Drop ends the
watch without changing code or losing a conversation. Buddy may say the work
looks done, but closing is explicit. Sending or taking a Buddy remark first
keeps it as the user's note, so an older review cannot retract it.

**Closing replaces archive.** Done, deleted and dismissed notes share one
recently-closed list, bounded by count and age (100 notes and 30 days by
default), restorable with their conversation link. Deleting or expiring a note
never undoes file changes. Reverting a done note's change reopens it. Note
sessions in their original form stay out of the ordinary chat chooser. Cleanup retains them while
notes still reference them; after expiry, safe local reference checks allow
ordinary age-based collection. Uncertain or remote state retains conversations.

**Old directive history stays readable, not executable.** A format-1 workspace
store is converted once after a hash-verified original-byte backup. References
become notes with commentary and tags; directives become unsent notes with
their request and subdirective requests as text. UUIDs and valid unique display
numbers are kept, and at migration retired numbers are reserved against reuse. A directive
with activity keeps frozen history on its note. Archived directives become
closed notes with reason `archived`, exempt from expiry.

`mevedel-note-history` opens that frozen request, planning, discussion and
attempt material in a read-only inspector, with patch viewing and source
navigation. There is no discussion, retry, implementation or settings action;
reattachment, Rewind and archive are refused there. Sending a migrated note
starts a new conversation. Persisted directive-turn markers still exclude old
turns from ordinary-chat model context and show collapsed inspectable cards.
Session Rewind preserves frozen note history and reads its recorded capture
gaps. Legacy queued directive input is marked retired for retraction or discard,
not silently executed as ordinary chat; a saved discuss preset resumes with a diagnostic
fallback to the implementation preset. These are compatibility readers and
refusals, not a second live workflow.

## Rationale

Choosing a reference, directive or Buddy note before writing cost attention and
produced different behavior for similar marks. One mark removes that choice:
context is a mention, a request is a send, and a remark is a Buddy-authored note.

Directive activity and archives grew without bound inside whole-set store
writes. Keeping new conversations in sessions and bounding recently closed
removes that source of growth. A waiting prepared change still holds file bytes,
and frozen migrated histories remain; this is not a constant-size store. Buddy
and model mutations batch writes through the existing checked sync. ADR 0108
records the real SSH/TRAMP measurement motivating batching.

The removed directive workflow duplicated capabilities the rest of the system
now supplies. Dedicated discussion authority and directive planning are
replaced by session permissions and Plan mode. The directive composer scope,
transcript filter, execution-session binding and archive separated many
directives within one session; independent sessions now make that separation
structural. ADR 0131's checked reversal removes the competing-snapshot objection
to one session per conversation: normal Rewind refuses conflicts instead of
clobbering another conversation's later edits.

## Consequences

- User and model note operations share identity, authorship, storage and closing.
  Read uses the note resource family; NoteWrite performs note mutations. In
  Plan it may create review notes with optional prepared changes, not change or
  close existing notes. Sending and marking done require the user's request in
  the tool contract; this is not a new permission grant.
- Directive discussion, planning, batch planning, plan approval, retry, archive,
  execution binding and composer scope are gone. Follow-ups, alternatives and
  checks are requests in the note conversation, not new note features.
- Note conversations use normal session persistence, resume and Rewind.
  Rewinding to before their first turn returns sent notes to unsent without
  deleting them or their session link; discarded turns' prepared changes are
  cleared. User text, anchors and frozen legacy history remain.
- Old numeric `@ref` mentions can resolve migrated notes without changing the
  historical chat. Old directive activity can be inspected but not continued
  in place. No unbounded directive archive is created for new work.

## Decision history

**ADR 0087 kept directive identity outside source overlays.** Activity-bearing
directives survived source deletion and could be archived but not removed.
Notes retain workspace identity and survival of source loss; recently closed
and per-session conversations replace permanent live activity and archive.

**ADR 0088 isolated directive activity from main chat** and bound each directive
to an execution session; ADR 0091 later rendered full turns in that session's
shared view. Note conversations keep context isolation but replace shared
execution binding with independent sessions, enabled by ADR 0131's checked
Rewind reversal. Old turns keep their marker-based context exclusion.

**ADR 0090 enforced discussion as a read-only capability preset.** Its preset
and ceiling disappear with the Discuss/Implement split. Note conversations use
the existing session permissions and Plan boundary instead.

**ADR 0108 originally kept Buddy notes outside instructions and persistence.**
Its current decision already reverses that exclusion for persistent,
Buddy-authored workspace notes. This record places those remarks within the
same note conversation, take and closing lifecycle.

**ADR 0130 stored references, directives and complete activity.** It now stores
notes, recently closed and display-number state. Its whole-set record, lock,
hash-based stale merge, format versioning and durable Rewind intent remain;
directive activity append and pruning are replaced by note lifecycle
reconciliation.
