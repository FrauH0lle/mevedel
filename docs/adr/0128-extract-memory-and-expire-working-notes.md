# Extract memory and expire working notes

Status: Proposed

## Scope and decision status

This records the lifecycle implemented on the `note-lifecycle` branch after the
user's 2026-10-29 scope decisions. It separates shared notes from the journal
publication and durable-memory transaction protocol in
[ADR 0117](0117-publish-journal-results-from-fenced-outcomes.md). Status remains
Proposed pending user review and cutover; no live cleanup or migration has been
performed. The runtime has no header codec, kind/status policy or note undo.

## Context

The review had become a second task tracker: it investigated whether every
finding, caveat, measurement and owner handoff was resolved before disposing of
a note. The third acceptance replay reviewed 40 notes and returned 38 keeps,
two condensations and no deletes. That is evidence of retention, not measured
capacity for the replacement design. The user wants temporary notes to disappear
after extracting qualifying knowledge, not proof that the work described in
them is finished. Open work cannot become memory merely to permit cleanup.

## Decision

### Three different stores

- **Journal:** automatic, dated transcript digests; evidence, not instructions,
  a task tracker, or a permanent store. Existing capture, recall and journal
  expiry contracts remain in ADR 0117.
- **Shared notes:** freely writable, machine-local temporary working material
  for any authorized session or agent. One topic per note; find and update a
  relevant existing note rather than maintaining a wiki or index by hand.
- **Memory:** the only permanent knowledge store in this lifecycle. Existing
  selective user/feedback/project/reference rules still apply. Tasks, progress,
  blockers and decisions belong in trackers, backlog or maintained documentation;
  continuation belongs in session context and compaction. This does not make
  maintained source, documentation or the project tracker disposable.

### Note format and write boundary

Markdown files (`.md`, case-insensitive) below `work://shared/` are notes. Other
files, including JSON reports, scripts and plain-text files, are attachments.
Both expire as described below; only readable UTF-8 Markdown enters model review.

There is **no note header**. ApplyPatch treats shared files as ordinary authored
content: it neither inserts nor validates frontmatter on creates, updates or
moves. There is no managed/unmanaged distinction, kind/status classification,
attribution field or creation timestamp in the lifecycle. Old header-shaped text
is ordinary content, not a format requiring interpretation or migration.
Guidance asks for one topic per file, a descriptive first heading and source/task
attribution in prose. Actionable work belongs in the tracker/backlog rather than
depending on a temporary handoff's survival. Normal permissions, selected-hunk
review, exact bytes, buffers and filesystem transactions remain unchanged.

### Generated discovery

`Read(work://shared/)` generates a listing from the current contained inventory:
canonical address, days since modification, age-readiness reason and the bounded
first Markdown heading (empty when absent). It is not a model-generated summary,
trusted guidance or a maintained file. Attachment rows have age and readiness
without a heading. Bare `work://` uses the same shared rows
beside session files; Glob/Grep retain their filename/content contracts.

Prefix reads and inventory limits are bounded; incomplete results say so.
Listing and search do not mark every returned file in use or reset its age.
Discovery creates no storage and never authorizes a later mutation. There is no
new hand-edited README, index, per-topic manifest or permanent note catalogue.

### Eligibility, not completion proof

Use the execution target's current time and the file's **last modification time**:

Every note and attachment uses one defcustom,
`mevedel-working-note-max-age-days`, default **14 days**; nil disables automatic
disposal. Empty-folder housekeeping uses the same age setting. Fourteen days
means elapsed multiples of 86,400 seconds, inclusive at the boundary. Reading,
listing and review time do not reset this clock; an actual modification does.
Future mtimes wait rather than implying negative-age eligibility.
Source words such as TODO, pending,
remaining or handoff have no effect on eligibility or deletion.

**Never mutate an observed in-use entry.** Keep exact-file resource-tool access
by live root sessions and their agents as local use evidence, with a separate
24-hour access window rather than the removed quiet-period setting. Recheck use
and unsaved visiting buffers at capture/admission and at the actual forward-write
boundary after preparatory I/O. Other sessions merely running, folder listings
and broad searches do not protect every note. Cancellation, original authority,
changed bytes/mode, linked paths and unsaved edits still prevent unsafe writes.

Local use evidence is not shared between Emacs clients or retained across a
restart, and shell/external reads are unobserved. This is an accepted limitation:
the fourteen-day modification threshold provides the chosen margin without
guaranteeing recovery. Do not add a cross-client usage lease registry or claim
global reader detection. Explicitly active collaboration files are excluded by the operator
when conducting a live inventory; filenames are not permanent runtime exemptions.

### One daily review, bounded batches

At idle maintenance, admit at most one automatic daily review cycle per workspace
per elapsed 24 hours on the target clock, under existing workspace ownership.
That cycle processes **all eligible notes**, in bounded successor batches, not
one sixteen-note request per day. Freeze a finite cohort of path/version identities
at cycle admission. Each identity is attempted at most once in that cycle;
changed, newly eligible and newly arrived versions wait for the next daily cycle.
Recheck current eligibility before every batch and mutation. The cycle records
attempts separately from confirmed deletion; a failed review is not service.

Keep the existing ordinary request bounds (16 notes, 96 KiB; oversized Markdown
alone in bounded chunks up to the configured limit), per-request deadlines,
output/tool budgets, cancellation and yielding between owned operations. Persist
enough cycle identity and attempt information to resume an interrupted cycle
without re-reviewing its completed attempts. A local busy owner postpones work;
a request/storage failure stops inference visibly instead of spinning. Held use
or extraction decisions can finish through checked application without re-running
the model. No catch-up chain, keep-as-progress test or rolling 48-batch cap may
prevent processing the rest of the finite daily cohort.

Note review requires no journal digest count and runs when journaling is disabled.
Journal-only consolidation keeps its separate existing admission policy; ordinary
turn/digest events need not launch a new note cycle. `manual`, `propose` and `auto`
remain distinct: manual requires an explicit request, propose leaves decisions
for approval, and auto applies extraction followed by disposal without prompts.
Explicit review/drain may bypass daily timing but not in-use or transaction safety.
An empty cycle makes no model request. No session or idle opportunity means no
background inference, not a promise of wall-clock deletion while Emacs is closed.

### Review contract: extract memory, then delete

The reviewer checks only for qualifying memory and proposes normal memory changes.
It does not enumerate open work, prove resolution, reproduce every measurement,
search Git for completion, or decide whether a note should become a tracker.
No qualifying memory is a successful extraction result and still permits deletion.

The model emits only ordinary qualifying memory/instruction proposals citing the
note ID in `evidence`; nothing else is needed for notes. There is no Notes section,
per-note acknowledgement, disposition, reason, keep/condense alternative or
resolution header. After a successful review the accepting store creates internal
deletion proposals for every exact admitted note, not every captured file.
Malformed memory/instruction proposals or ambiguous output reject the review
before acceptance and create no deletions. Chunk summaries carry potentially
qualifying lessons, attribution and uncertainty, not an unresolved-work inventory.

Each accepted deletion depends on all memory changes citing that note. Apply all
those changes first, then delete the exact reviewed version. Pending extraction
holds deletion; failed/rejected/stale extraction leaves the source available for
a later review, not an invented successful keep. Repair existing memory/instruction
citations to temporary sources through ordinary checked updates/removals in the
same review. A remaining citation is an explicit dependency hold/diagnostic, not
a semantic keep or permission to break a permanent topic. New memory must be
self-contained or point to an authoritative durable source. Freshly recheck the
complete applicable memory/instruction citation scope before the actual note
delete, including after approval/application waits. A new citation or incomplete
observation holds deletion; captured absence is not proof of current absence.

Attachments receive no model review. At the configured untouched age (default
14 days), delete them only when not in use and not referenced by any surviving
note or memory/instruction file.
Surviving readable text attachments also protect their references: the Markdown
boundary limits model review, not reference safety for scripts, JSON or plain text.
Keep conservative reference scanning, including complete large-file scans and
fresh reference checks before mutation; unknown/incomplete reads defer deletion.
Empty unreferenced directories can be pruned, preserving the shared root.
Attachments use the same permanent deletion path as notes, without an undo copy,
private-copy quota or system-trash alternative.

### Permanent disposal, not an undo store

Temporary files are not critical durable user data. A checked target operation
verifies ownership, reviewed hash/mode (or empty-directory state), then deletes
the entry and optionally prunes empty ancestors below the shared root. Fresh
extraction, reference, modification-age, local-use and unsaved-buffer checks
still guard actual forward mutation. There is no blob capture, quota accounting,
undo deadline, note restoration or note rollback. Reversing a memory extraction
does not restore its deleted source notes. Durable memory/instruction reversal
and interrupted-write reconciliation retain their separate existing contracts.

Prepared/accepted/write evidence can retain reviewed text under the ordinary
history lifecycle. This decision adds neither a per-file payload purge deadline
nor a secure-erasure guarantee. It does not scrub quoted fragments, backups,
authored memory or unrelated state. Do not rebuild payload receipts, caches,
audit indexes or recovery systems for temporary-file disposal.

### No header migration or compatibility path

No note conversion is needed or provided. Existing files keep their authored
bytes, modes and mtimes; old header-shaped text has no lifecycle meaning.

Follow `CODING_STANDARDS.md`'s no-backwards-compatibility rule: remove old APIs,
settings, prompt fields, dispositions, guards, counters and all their callers;
do not keep aliases or an old/new proposal reader. Recover active marked writes
and resolve or explicitly discard old pending note decisions using the old build
before cutover. Do not reinterpret old accepted keep/condense records as deletes
or mutate their authenticated bundles. There is no runtime format adapter or
version gate. Session serialization is unchanged; its migrator is not involved.

### What this supersedes in ADR 0117

ADR 0117's **Current decision** replaces the old paragraphs beginning **“Explicit
backlog drain coordinates”** (progress-qualified catch-up and 48-batch ceiling),
**“Acceptance converts a condense”** (identical-condense normalization), and
**“Shared working files are not journal input”** through the note lifecycle end
(three dispositions, open-item/anchor/numeric/source-marker guards and system trash).
The separate journal, ownership, exact-write, durable rollback and recovery
decisions remain.
Ordinary history retention remains distinct from disposable-file age and does
not make note reversal available.

The following **Decision history** choices are replaced, retaining why each
was tried: **“2026-10-29: Catch-up follows fresh arrivals; identical condensation
is retention”**, **“2026-10-29: Preserve numeric paragraphs structurally instead of
keeping whole notes”**, **“2026-10-28: Progress guards automatic catch-up; the cap is
a ceiling”**, and the system-trash alternative in **“Size alone no longer blocks
every shared-file cleanup path.”** The complete-note/chunk and attachment
reference-scan rationale remains. The failure behind **“Shared files left the journal for
a consolidation-owned lifecycle”** still supports direct note review; its
three-disposition remedy is replaced, not the separation from journal capture.

## Decision history

The first draft and implementation slices introduced lifecycle frontmatter,
scratch/open tool defaults, explicit handoff/report/done labels, and split
seven/fourteen-day ages. A later draft required thirty-day undo, single-preimage
storage and purge. On 2026-10-29 the user dropped undo and its scaling/recovery
offshoots: their machinery was disproportionate to losing temporary files.
A brief flat thirty-day age choice was superseded by **decision 15**: flat
fourteen days and no header at all. Edits already reset age; seven flat days
would age out idle handoffs, while a split depends on labels models may omit.
The header codec, normalization, classification, migration and undo paths were
removed rather than kept as compatibility machinery. The tracker/memory split,
selective extraction, finite cohorts and inexpensive deletion guards remain.

The next controlled eight-note extraction check failed before acceptance: the
reviewer put note acknowledgements under Promote and emitted no memory bodies,
despite calling the four positive lessons durable in No action. On 2026-10-30 the
user removed the acknowledgement contract entirely. Successful review admission
already determines which exact sources were processed, and extraction evidence
already determines their dependencies; a second model-written note roster adds
format failures without useful authority. The parser now handles memory and
instruction changes only; the store generates disposal from admitted IDs after
successful validation. Failed/rejected review still disposes of nothing.

## Consequences and acceptance evidence

Temporary notes can disappear while they still describe open work. That is the
intended change: owners must put actionable work in the tracker before it ages
out. Permanent deletion replaces the attempt to prove semantic completeness;
original-target and exact-byte protections against corrupt writes remain.
Missing labels cannot make notes immortal. Oversized/unreadable files, active
use, unsaved changes and unavailable references remain visible deferrals.

The target capacity is 50–100 notes per day, **not established by deterministic
tests**. The deterministic fifty-note native drain exercises actual request
serialization, response parsing, acceptance and permanent deletion, replacing
only URL transport. It checks rejected memory output, changed and late
versions, and a thirteen-day headerless handoff excluded from the frozen cohort.
Scripted transport proves mechanics, not inference quality, cost or throughput.
Any separately authorized real-provider replay uses a private copied corpus,
never the live shared folder, and reports confirmed deletions separately from
arrivals and memory extractions proposed/applied, format failures and wall time.
