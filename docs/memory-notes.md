# Shared working notes

## Shared working-note lifecycle

Main and worker guidance asks for attributed factual lessons in
`work://shared/` when the request permits workspace writes. Agents search before
creating and update relevant existing files, using Markdown with one topic per
note and freely chosen filenames, a descriptive first heading and source/task
attribution in prose. There is no note header, lifecycle label, kind/status or
managed/unmanaged distinction. ApplyPatch treats shared files as ordinary authored
content: it neither inserts nor validates frontmatter. Existing header-shaped
text has no lifecycle meaning and needs no migration. Keep actionable work in the
tracker/backlog, not only in temporary notes. Normal edit permissions and
review/transaction protections still apply. Read-only agents
report lessons through their existing results or available SendMessage; they
gain no mutation tools. `$learn` considers available notes and digests as
additional evidence, verifies relevant claims, and acknowledges omissions.

Shared files (`.mevedel/shared/`) are untracked, machine-local and temporary.
Only files below `work://shared/` participate in this working-file expiry;
maintained source, documentation, trackers and curated memory do not. Journal
retention has its own separate policy.
`mevedel-memory-notes.el` owns their lifecycle: general consolidation reviews
extract lasting knowledge from age-ready files into memory and then dispose of
them. Focused reviews do not include notes. Accepted reviews and decisions are
the only processing record.

**Readiness.** Use the target clock and actual last modification time, not the
authored creation timestamp. One defcustom, `mevedel-working-note-max-age-days`,
sets a flat **14 elapsed days** by default for every note and attachment, and
empty-folder housekeeping. Fourteen days is the chosen safety margin for use
that the editor cannot observe, not a guarantee of recovery. Users may lower
this value for themselves; the default remains 14 days. Set it to nil to disable
automatic disposal. Note size never triggers deletion: size limits bound review
requests, and oversized files that cannot be reviewed remain blocked.
Boundaries are inclusive multiples of 86,400 seconds; future mtimes wait.
A note version is its path and content hash; modification age uses actual mtime,
including a same-byte touch. Note deletion freshly rechecks that age after
approval and preparatory IO; renewed age is a nonterminal hold, not authorization
from an older capture. A pending/recovery-required decision suppresses its exact version.
Prior completed review, rejection or reversal outcomes do not start an additional
seven-day reconsideration timer; rejection evidence remains available to later
reviews. A review leaves notes that open sessions are using for later. A note is in use while a
session whose buffer is live, or one of its sub-agents, named that file in a
`work://shared/` Read, Glob, Grep or ApplyPatch within a separate 24-hour access
window. Folder
listings and searches do not count, and neither does another session merely
running a turn. This evidence lives in the editor that ran the tool and is not
retained across restarts or shared between clients; access through ordinary
paths, such as a shell command, is not observed. Capture reads the folder
inventory first and authenticates prior disposition status only for paths
present in it: a deleted path cannot be admitted, and a recreated one is
checked like any other.

Shell access, use before an editor restart and use from another client are
accepted tracking limits for these temporary shared files, not gaps requiring a
cross-client use registry. To keep a note temporarily, edit it to renew its
modification age; merely reading it does not. For lasting value, promote its
content to curated memory or maintained documentation, and put actionable work
in the tracker/backlog. Do not rely on a temporary note's survival.

Shared-directory Read exposes generated bounded modification-age and age-readiness
rows with the first Markdown heading (empty when absent), not a model-generated
summary or a maintained index. It says capture/application safety checks remain pending;
these rows do not certify current dependency/reference absence. Incomplete
observations stay visible. Listing never writes an index or marks all entries
in use, and direct Read/Glob/Grep keep their existing contracts.

**Capture.** Each general review walks the folder without following links
(at most 4,096 entries). Only Markdown (`.md`, case-insensitive) enters model
review; complete UTF-8 without NUL bytes is required. All non-Markdown files are
attachments, including readable scripts, JSON and plain text. At most 16 ordinary text notes and 96 KiB are captured,
oldest first, each at most 32 KiB; the review request admits them beside
digests (one of each when both fit, then oldest first). A larger text note is
captured alone, leaving eligible digests unpinned for the next batch, up to
`mevedel-memory-notes-max-review-bytes` (default 512 KiB, about sixteen chunks).
It is reviewed sequentially in UTF-8 chunks of at most 32 KiB, carrying a
running summary capped at 16 KiB. The summary carries candidate durable lessons,
source attribution and uncertainty across chunks, not an open-work resolution
audit. The final review uses that evidence to extract qualifying knowledge;
successful acceptance generates the disposal through the normal decision path.
Whole-file bytes and hash remain the before-state, never a
truncated prefix. Each chunk and final settlement recheck that whole-file
state. A change discards the summary and reports `restart-required`; the
next review starts from the beginning once the new version is quiet. The
chunk sequence and final review share a 180-second deadline. The sequence's
reasoning and replies also share the ordinary cumulative
output budgets, guarded during streaming; cancellation retains incurred usage.
Files beyond the configured bound stay blocked with an explicit request to split, delete
or move them manually; unreadable files and links also stay blocked with a reason.
The latest observation of ready, waiting and blocked items is
recorded in `.mevedel/state/shared-notes/observation.el`; it counts every
blocked item and lists at most 64 with reasons. `s` in the memory cockpit
shows it, and review completion messages report the counts.

**Citations.** Memory topics and instruction files are captured first. A file
they cite, by `work://shared/` address or `.mevedel/shared/` path, including
through a named folder below the shared root, is never removed as housekeeping.
The generic shared-root address in policy or navigation is not a dependency on
every child; the shared root itself is retained. A cited note is
flagged `cited_by_memory`. The reviewer can repair its captured citing memory
with Update or Remove, or replace a citing instruction file with Instructions,
preserving unrelated guidance and citing the note as evidence. Remaining
citations hold deletion as an unresolved dependency, not an automatic keep.
The applicable memory and instruction citation scope is freshly rechecked at
the actual deletion boundary, including after approval waits. New citations or
an unavailable or incomplete scope hold deletion; capture cannot prove their
later absence.

**Decisions.** After a successful review the host creates a deletion proposal
for every admitted note. The reply has no Notes section, acknowledgement or
note-specific output. Failed/rejected reviews delete nothing. Extraction is selective:
a note may yield no qualifying memory. Tasks,
unresolved findings, measurements and possible future usefulness do not require
a completion audit or automatically retain a note. Open work belongs in its
tracker or maintained documentation, not in permanent memory merely to permit
cleanup. The old keep/condense, open-item resolution, quote/anchor and numeric
preservation rules are removed; their rationale remains in ADR history.

Extraction uses ordinary memory and
instruction proposals citing the note's ID as evidence. Acceptance records
all those extractions as the disposition's `:requires`. Deletion applies
only after every required extraction is applied; an undecided requirement
waits without a decision, and any other requirement outcome makes the
disposition stale, leaving the note for a later review.

Malformed memory/instruction proposals or ambiguous reply structure reject the
whole review before acceptance. There is no acknowledgement parser, skipped-note
diagnostic or note-format counter. Accepted passes prove attempts for their exact
admitted versions, while checked write outcomes prove actual deletion. A captured
but unadmitted note gets no disposal proposal. The finite attempt set replaces
capture-history ranking and the applied-progress catch-up guard; new versions and
later arrivals wait for the next cohort.

Confirmed deletions, held work and review failures remain distinct outcomes.
Extraction dependencies, target ownership, current-use and unsaved-buffer
checks, exact before-state, checked writes and interrupted-write reconciliation
still apply. Deletion is permanent; reversing an extraction never restores its
deleted source notes. A write confirmed before cancellation retains its count; a prepared
proposal or a deferred retry does not establish that a deletion occurred.

**Housekeeping.** Attachments at least the configured untouched age (default
fourteen elapsed days) that no
surviving text file, memory topic or instruction references receive delete
decisions without inference; age-ready empty folders receive prune decisions,
at most 128 housekeeping items per review. Matching is conservative across
root-relative, note-relative, URL-encoded and address spellings, including named
containing folders below the shared root. Generic root navigation does not
protect every child. All other surviving Markdown and readable attachments,
including scripts, JSON and plain text, participate, even if they have pending
deletion proposals; only the target itself is excluded. Reference scans stream large texts in
1 MiB chunks with overlapping filename spellings, so transcript copies above
1 MiB do not make the scan incomplete; references split across chunk
boundaries still protect attachments. Read failures, changing files and
incomplete inventories block attachment cleanup, not text review. Captured
permanent scope retains omissions and completeness, not merely flattened text;
unknown scope cannot prove reference absence. Before actual deletion, complete
fresh shared and applicable permanent scopes are checked again, including
backdated edits and unsaved drafts. Inventories, file versions and configuration
are revalidated after I/O. New references, renewed age, active use or unavailable
evidence hold the original without a terminal decision. These checks repeat
at final write admission for both synchronous and yielding application.
A review with only housekeeping starts no model request. The dated capture and
application observation explains holds in the cockpit; generated resource
discovery remains age-readiness-only with safety checks explicitly pending.

**Permanent application.** Note decisions use checked decisions, write
intents and interrupted-write reconciliation with a shared-notes target whose
coordination lives in `.mevedel/state/shared-notes/`, outside the folder it
cleans. Final use, unsaved-buffer, age, extraction and reference checks guard
actual forward mutation. A locked target program repeats ownership and the
reviewed hash/mode (or empty-directory state), then deletes plainly and removes
ancestors left empty, preserving the shared root. Notes and attachments have
**no undo**, blob capture, quota accounting, system-trash staging or restoration
path. Reversing a memory extraction does not restore deleted notes; durable
memory/instruction reversal retains its separate existing contract.
Prepared, accepted and write evidence may retain reviewed text under ordinary
history retention. This is not a per-note purge deadline or secure-erasure
guarantee, and it does not introduce payload receipts, audit caches or a second
undo store. No live conversion or header migration is required.

**Modes.** `manual` processes notes only through `/remember`; `propose` leaves
their decisions in the cockpit; `auto` applies extraction, deletion and
housekeeping without prompts. In `auto`, note decisions wait
while an open session uses their file (see Readiness), rechecked at the
mutation boundary; other notes and undecided decisions of earlier automatic
note reviews proceed. A one-minute retry applies the held ones once any of
their files is no longer in use.
That retry defers to a registered explicit drain, including its between-batch
waiting gaps; the drain applies earlier pending decisions through its normal
owned application path. A drain also waits for already-running local work.
After busy/freshness IO, a retry rechecks its exact registration and local
pass/drain observations before admission. Cancellation or failure of an older
retry cannot remove or overwrite its successor. These observations coordinate
this client only; native claims and original-target checks remain authoritative.
