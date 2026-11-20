# Notes replace references, directives and Buddy notes

Status: accepted. Supersedes the directive decisions of
[ADR 0087](0087-keep-directive-identity-outside-source-overlays.md),
ADR 0088 (recorded in
[ADR 0091's history](0091-render-directive-turns-in-the-shared-session-view.md#decision-history))
and [ADR 0090](0090-enforce-discussion-as-a-read-only-capability.md), plus the
directive-specific revision and rendering of ADRs 0089 and 0091.
Complements [ADR 0108](0108-buddy-remarks-are-workspace-notes.md)'s Buddy
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
inherited. A send includes nested descendants, naming each immediate parent and
capturing code once for the outer note. A descendant in another conversation and
its subtree are excluded. A batch sends covered descendants only once. Preview
shows the generated text before the send; the actual provider input is retained
separately for inspection. Reopening resumes without adding newly nested notes.

The [docked transcript and composer](0136-dock-the-composer-in-its-own-buffer.md)
appear in the anchored child frame from [ADR 0106](0106-directive-frame-is-a-child-frame.md),
or in an ordinary right-hand split when child frames are unavailable. There is
no per-note Discuss, Implement or Plan action: session permissions and Plan
mode govern tools. Ask requires approval for mutations; it is not a read-only mode. Plan
mode restricts project writes through the normal tool boundary, while allowing
session-owned plan files and new review notes. The base preset defaults to
`mevedel-implement` and remains user-selectable, as do model, effort and permission
mode. A settings transient uses the existing chat model selector; choices persist
on an unsent note and update its live session after send. A separate durable note
layer restores isolation, input recording and keys after preset changes and
resume. It preserves the base preset's `:system` and tools, filtering only the
`journal`, `active-goal` and `goal-policy` prompt components. Explicit forks are
ordinary chats without the note layer.

The source menu is one inline **Actions** row: `s` send/reopen, `t` take/drop,
`i` edit, `g` tags, `x` done, `k` delete/dismiss, `b` keep a Buddy remark,
`w` copy, `p` preview, `S` settings and `e` expand/collapse. Apply/merge (`a`/`m`) appear only
for prepared changes, revert (`r`) only for applied changes. The conversation
hint above the prompt shows `C-c C-z close`, plus
`C-c C-a apply · C-c C-g merge` while a change waits.

**Source ApplyPatch proposals wait as prepared changes.** Preparation returns
without writing source or holding the model turn open. The waiting record
retains the proposal and base file bytes and is shared by batch siblings. Apply
preserves disjoint later edits and refuses conflicting or unsaved edits. The
user can request a revision in the ordinary composer or resolve in a separate
three-way smerge buffer; generated conflict markers never enter the source file. Confirmation
rechecks current source before writing. Resource-address patches retain their
ordinary ApplyPatch behavior; preparation is not a general sandbox for Bash,
Eval or other tools.

A newer proposal replaces the waiting one, while older proposals remain in
conversation history. Applying records the actual file changes under the
originating session turn, clears that waiting record and closes the notes in the
conversation as done. Excluded notes stay open and track source edits normally.
Applying an unlinked suggestion closes only that suggestion.
Applied changes can be reverted individually with the shared checked reversal
engine of [ADR 0131](0131-rewind-reverts-recorded-changes.md).
Suggestions from another session retain their application-specific backup names
on the note, so reverting them never reverses unrelated changes in that turn.
Those references preserve the backups, not the originating session's lifetime.

**Take hands implementation to the user.** Buddy watches the taken note's code
using its text as brief at every dial level, including Quiet. After an Emacs
restart a restored take stays taken but is labelled paused: visiting its source
neither starts Buddy nor reads the saved dial and asks no question. Taking it
again resumes that note; explicitly setting the workspace dial resumes its
takes for this Emacs session, including files visited later. At Active, nearby
authored notes, including unsent notes, also supply goals for ideas; elsewhere
Buddy behaves like Watching. Drop ends the watch without changing code or
losing a conversation. Buddy may say the work looks done but never closes a note.
Sending or taking a Buddy remark first
keeps it as the user's note, so an older review cannot retract it.

**Closing replaces archive.** Done, deleted and dismissed notes share one
recently-closed list, restorable with their conversation link. Done and deleted
notes share a count limit (100 by default); dismissed Buddy remarks and agent
notes do not count against it. All closed notes expire by age (30 days by
default). Deleting or expiring a note
never undoes file changes. Reverting a done note's change reopens it. Note
sessions in their original form stay out of the ordinary chat chooser. Cleanup retains them while
notes still reference them; after expiry, safe local reference checks allow
ordinary age-based collection. Uncertain or remote state retains conversations.

**There is no old-data migration.** Neither the runtime loader nor the standalone
session migration script converts old instruction data. Ordinary resume ignores
old per-session instruction snapshots, without importing or deleting them, and
they do not block creation of a fresh current note store. Format-1 stores are
unsupported and refused rather than converted. No preset swap, frozen directive
history inspector, legacy capture-gap reader or browser directive scope remains.

Old saved sessions still resume without directive-specific handling. Their old
directive headlines and replies are ordinary turns in chat and model context,
not excluded spans or collapsed directive cards. Generic Org drawers and unknown
audit records retain their ordinary presentation; removing directive decoding
does not turn hidden audit payloads into ordinary response text.

## Rationale

Choosing a reference, directive or Buddy note before writing cost attention and
produced different behavior for similar marks. One mark removes that choice:
context is a mention, a request is a send, and a remark is a Buddy-authored note.

Directive activity and archives grew without bound inside whole-set store
writes. Keeping new conversations in sessions and bounding recently closed
removes that source of growth. A waiting prepared change still holds file bytes,
so this is not a constant-size store. Buddy and model mutations batch writes
through the existing checked sync. ADR 0108
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
  cleared. User text, anchors and session links remain.
- `@ref` remains an authored mention spelling for current notes, not an import
  path for old reference data. No unbounded directive archive or legacy inspector
  is retained.

## Decision history

**The original note preset was fixed.** The first implementation used a dedicated
`mevedel-note` preset inheriting `mevedel-implement`, and refused ordinary preset
changes to keep note isolation intact. The user required a chosen base preset,
model, effort and permission mode before sending, with the same controls after
send. Making note behavior a durable layer instead of a preset identity meets
that requirement without discarding the chosen preset's system prompt or tools.
The dedicated re-ask command and duplicate close binding were also removed:
follow-ups use the ordinary composer and `C-c C-z` is the note close key.

**Dismissed notes originally shared the count limit.** High-volume remark
dismissals could evict the user's done or deleted notes and release their
conversations for cleanup. Dismissed notes now retain only the age limit, so
dismissing advice does not consume that restoration budget.

**ADR 0087 kept directive identity outside source overlays.** Activity-bearing
directives survived source deletion and could be archived but not removed.
Notes retain workspace identity and survival of source loss; recently closed
and per-session conversations replace permanent live activity and archive.

**ADR 0088 isolated directive activity from main chat** and bound each directive
to an execution session; ADR 0091 later rendered full turns in that session's
shared view. Note conversations keep context isolation but replace shared
execution binding with independent sessions, enabled by ADR 0131's checked
Rewind reversal. Old turns no longer have marker-based context exclusion.

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

**Old-data compatibility was initially retained, then removed.** The first notes
implementation backed up and converted format-1 stores, kept activity as frozen
history, exempted migrated archives from expiry, folded and excluded old
directive turns, and substituted the implementation preset for saved Discuss.
No old-format stores existed in the user's installation. The user chose the
clean current design without these readers or migration steps; old conversations
remain resumable with their directive turns treated as ordinary turns instead.

**Restoration originally restarted taken-note watching automatically.** The user
required a fresh invitation after restart instead of unasked background work on
file visit. Watch activation is now session-local while the taken state persists;
retaking or setting the dial resumes it without an interrupting visit prompt.
