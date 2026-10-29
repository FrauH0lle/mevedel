# Extract memory and expire working notes

Status: Proposed

## Scope and decision status

This is the requested design draft against `7570fb62`, not a description of
implemented behavior. It separates the shared-note lifecycle from the journal
publication and transaction protocol in [ADR 0117](0117-publish-journal-results-from-fenced-outcomes.md).
The user accepted the lifecycle direction and authorized sliced implementation on
2026-10-29. This draft remains Proposed until the corresponding runtime and
verification land; implementation choices and the legacy conversion below are
not yet current behavior. Acceptance must amend ADR 0117
and its index in the corresponding implementation, retaining its decision history.

## Context

The review had become a second task tracker: it investigated whether every
finding, caveat, measurement and owner handoff was resolved before disposing of
a note. The third acceptance replay reviewed 40 notes and returned 38 keeps,
two condensations and no deletes. That is evidence of retention, not measured
capacity for the replacement design. The user wants temporary notes to disappear
after extracting qualifying knowledge, not proof that the work described in
them is finished. Open work cannot become memory merely to permit cleanup.

## Proposed decision

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
files, including JSON reports, scripts and plain-text attachments, are attachments:
never prepend YAML to code or machine-readable data. They expire too, as described
below. This replaces the current classification of arbitrary UTF-8 files as notes.

A managed note starts with a YAML block containing exactly these lifecycle fields:

```yaml
---
kind: handoff
status: open
owner: "SESSION-ID/AGENT-PATH"
created: "2026-10-29T11:00:00Z"
---
```

`kind` is `handoff`, `report` or `scratch`; `status` is `open` or `done`.
`owner` is a nonempty attribution string, not an authority or retention lock.
`created` is a UTC RFC 3339 timestamp, not the age used for eligibility.
No summary, last-reviewed, unresolved-items, schema-version or expiry field is
authored into the note. The body stays ordinary Markdown.

The ApplyPatch write path owns normalization for creates, updates and moves into
the shared Markdown tree, whether addressed through `work://shared/` or its
contained physical path. New notes without an explicit kind get `scratch`, `open`, the
invoking session/agent identity, and the target time. Explicit valid kind/status
values are honored; updates preserve attribution and creation time unless the
author deliberately changes them. Existing unmanaged notes acquire the defaults
on their next authorized write, using their previous mtime as creation time.
Agent guidance tells models to mark real handoffs explicitly as `kind: handoff`
(and reports as `kind: report`); the default scratch/open note has a seven-day
modification-age window. Status defaults to open independently of kind.
Malformed supplied frontmatter is a visible
validation error, not silently replaced. Leaving the tree or writing session
`work://plans/`, memory, source, or attachments does not add note metadata.

Normalization happens after authorized resource resolution and while preparing
the selected changes, before preview/review and the exact transaction snapshot.
The visible diff includes generated metadata. Partial-hunk selection must not
bypass validation. The committed bytes, preview, rollback and buffer contents
must agree; never decorate a note after committing it. No permission exception,
new tool or model-side bookkeeping sequence is introduced.

External writers can bypass this boundary. **A Markdown file without valid
metadata is unmanaged:** it uses the 14-day rule, appears as
`unmanaged` in discovery, and produces a bounded format diagnostic. It is not
blocked forever. This is a rule for current external writes as well as old files,
not a second legacy-format reader or a compatibility shim. Headerless notes may
be user-authored or come from another client, which is why they receive the
longer window until their next tool write. Explicit legacy conversion remains
handoff/open with owner legacy; it does not use the tool-write scratch default.

### Generated discovery

`Read(work://shared/)` generates a listing from the current contained inventory:
canonical address, kind/status or `unmanaged`, owner, days since modification,
eligibility/defer reason and a one-line summary. The summary comes from the first
heading, falling back to the first nonempty body paragraph, with a bounded length;
it is not model-generated, trusted guidance or a maintained file. Attachments
have their own type/age/reason rows. Bare `work://` uses the same shared rows
beside session files; Glob/Grep retain their filename/content contracts.

Metadata reads and inventory limits are bounded; incomplete results say so.
Listing and search do not mark every returned file in use or reset its age.
Discovery creates no storage and never authorizes a later mutation. There is no
new hand-edited README, index, per-topic manifest or permanent note catalogue.

### Eligibility, not completion proof

Use the execution target's current time and the file's **last modification time**:

| Entry | Eligible when untouched for at least |
| --- | --- |
| Managed scratch, either status | 7 days |
| Any managed note with status done | 7 days |
| Open handoff or open report | 14 days |
| Unmanaged Markdown note | 14 days |
| Attachment | 14 days, with the reference exclusions below |

Seven/fourteen days mean elapsed multiples of 86,400 seconds, inclusive at the
boundary. Reading, listing, creation time and review time do not reset this clock;
an actual modification does, including a status edit. Future mtimes wait rather
than implying negative-age eligibility. Source words such as TODO, pending,
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
the 7/14-day modification thresholds and 30-day checked undo provide the chosen
margin. Do not add a cross-client usage lease registry or claim global reader
detection. Explicitly active collaboration files are excluded by the operator
when conducting a live inventory; filenames are not permanent runtime exemptions.

### One daily review, bounded batches

At idle maintenance, admit at most one automatic daily review cycle per workspace
per elapsed 24 hours on the target clock, under existing workspace ownership.
That cycle processes **all eligible notes**, in bounded successor batches, not
one sixteen-note request per day. Freeze a finite cohort of path/version identities
at cycle admission. Each identity is attempted at most once in that cycle;
changed, newly eligible and newly arrived versions wait for the next daily cycle.
Recheck current eligibility before every batch and mutation. The cycle records
attempts separately from confirmed deletion; malformed output is not service.

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

Keep a small per-admitted-note acknowledgement in the Notes output: only
`note: "ID"`, with deletion implied and an exactly empty body. There is no
`disposition` or `reason` field, no keep or condense alternative, and no open-items,
resolved-by, resolution-anchors or unresolved-items header. Invalid, missing or
duplicate acknowledgements leave that particular note
unchanged with a format diagnostic; they do not discard unrelated valid memory
proposals or cause same-cycle retries. A malformed memory proposal or ambiguous
target ownership still fails the review. Chunk summaries carry potentially
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

Attachments receive no model review. At 14 days untouched, delete them only when
not in use and not referenced by any surviving note or memory/instruction file.
Surviving readable text attachments also protect their references: the Markdown
boundary limits model review, not reference safety for scripts, JSON or plain text.
Keep conservative reference scanning, including complete large-file scans and
fresh reference checks before mutation; unknown/incomplete reads defer deletion.
Empty unreferenced directories can be pruned, preserving the shared root.
Attachments use the same private undo and retention rule, with no system-trash
fallback. If the private-copy quota would be exceeded, leave the file intact and
report why; never delete without its undo copy or discard unexpired undo early.

### Thirty-day undo, then purge

Deletion retains the existing private content-addressed preimage, exact mode and
path/parent information. Record an undo deadline **30 days after confirmed
application**, independent of review time and the ordinary memory-history window.
Until then, reversal requires the current expected absence/after-state and original
authority; it must not overwrite a recreated file or unsaved buffer. Reversing an
extraction still restores its dependent deleted notes first, or refuses visibly.

Use a single persisted preimage: prepared/accepted/write records retain its hash,
mode, size and reference, not duplicate note bodies. Hydrate it only for authorized
inspection/review/undo. At the deadline undo becomes unavailable; bounded idle
maintenance purges expired preimages and other retained raw copies independently
of unrelated pending proposals and ordinary review-history expiry. Content shared
by another unexpired undo or an active review remains until its last legitimate
reference ends; the expired disposal itself never regains undo eligibility.
Hash-only audit and recovery metadata may remain, never an indefinite plaintext
archive disguised as a recovery dependency. Expired rollback fails visibly rather
than reconstructing missing bytes or overwriting current files. Physical purge
occurs at the next available maintenance opportunity, not while the host is off.
This is application-level removal, not secure erasure of backups or storage media.

The proposed thirty-day guarantee covers retained **raw note preimage copies**:
captured scope, transaction intents, worker IPC payloads and rejected raw replies
must not become a second indefinite archive. It does not promise erasure of
model- or user-authored text. Memory proposals and No action bullets may quote
fragments of a note and follow ordinary review-history retention; deliberately
extracted durable memory follows its normal lifecycle. This is an explicit
limitation of the proposal, not a global text-scrubbing requirement. Do not add
quote-tracking sidecars or scrub unrelated authored text to implement it.

The single-preimage/deadline collector is a separate implementation slice after
the core lifecycle. **Until that slice lands, existing private preimage copies may
outlive 30 days.** Do not advertise the complete retention guarantee or enable
destructive rollout as though changing a retention default alone implements it.

### Migration and compatibility

Propose a one-shot, explicit conversion of existing headerless Markdown notes to
`handoff/open`, `owner: legacy`, `created: <old mtime>`, preserving their mtime and
permissions under checked before-state. Do not infer done/scratch from prose or
filenames, or restart the age clock by stamping migration time. This conservative
14-day classification avoids falsely assigning completion and avoids making old
notes immortal. In-use, changed or malformed files are reported/deferred; repeated
conversion is a no-op on valid managed notes. Runtime unmanaged handling means
conversion is helpful metadata normalization, not a prerequisite for cleanup.
Non-Markdown data is not rewritten. This design task performs no conversion.

Follow `CODING_STANDARDS.md`'s no-backwards-compatibility rule: remove old APIs,
settings, prompt fields, dispositions, guards, counters and all their callers;
do not keep aliases or an old/new proposal reader. Recover active marked writes
and resolve or explicitly discard old pending note decisions using the old build
before cutover. Do not reinterpret old accepted keep/condense records as deletes
or mutate their authenticated bundles. The standalone conversion is not a runtime
version gate; session format is unchanged, so the standalone session migrator is
not involved. If implementation changes session serialization, update that one
migrator and its tests instead of adding loader compatibility.

### What this supersedes in ADR 0117

In ADR 0117's **Current decision**, replace the paragraphs beginning **“Explicit
backlog drain coordinates”** (progress-qualified catch-up and 48-batch ceiling),
**“Acceptance converts a condense”** (identical-condense normalization), and
**“Shared working files are not journal input”** through the note lifecycle end
(three dispositions, open-item/anchor/numeric/source-marker guards and system trash).
Keep the separate journal, ownership, exact-write, rollback and recovery decisions.
The ordinary journal/memory history paragraph does not become a substitute for
the new per-disposal undo deadline.

Mark the following **Decision history** choices as replaced, retaining why each
was tried: **“2026-10-29: Catch-up follows fresh arrivals; identical condensation
is retention”**, **“2026-10-29: Preserve numeric paragraphs structurally instead of
keeping whole notes”**, **“2026-10-28: Progress guards automatic catch-up; the cap is
a ceiling”**, and the system-trash alternative in **“Size alone no longer blocks
every shared-file cleanup path.”** Preserve the complete-note/chunk and attachment
reference-scan rationale. The failure behind **“Shared files left the journal for
a consolidation-owned lifecycle”** still supports direct note review; its
three-disposition remedy is replaced, not the separation from journal capture.

## Consequences and acceptance evidence

Temporary notes can disappear while they still describe open work. That is the
intended change: owners must put actionable work in the tracker before it ages
out. Reversibility replaces the attempt to prove semantic completeness, not the
original-target and exact-byte protections against corrupt writes. Unmanaged
notes and attachments cannot accumulate solely because they missed the write path.
Long oversized notes, quota pressure and unavailable references remain visible
operational deferrals, not hidden success.

The target capacity is 50–100 notes per day, **not measured by this draft**.
Acceptance must measure the complete new path with 50-note and 100-note cohorts,
including ordinary and oversized notes, unmanaged files, mixed attachments,
in-use changes, invalid per-note output, interrupted application and expiry.
Report attempted, actually reviewed, extracted, confirmed deleted, held and failed
counts separately, plus calls, tokens/cost when available, cycle elapsed time and
editor lag under a stated configuration and sample count. Propose-only acceptance,
guard flags and published reviews are not deletion throughput. Verify exact undo
at the 30-day boundary and removal of every raw preimage copy after expiry in an
isolated fixture; do not use the live shared folder as a cleanup experiment.
