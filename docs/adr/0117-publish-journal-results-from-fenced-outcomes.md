# Publish journal results from fenced outcomes

Status: accepted

## Current decision

Journal and memory-consolidation work use workspace-owned, bounded claims on the
existing pinned control filesystem, independently of session leases. A numbered
claim is elected by exclusive creation with a fixed target-clock deadline.
Completion, failure, cancellation, and expired-owner takeover compete for one
immutable outcome. A successor requires that outcome first. Clock checks and
process identity alone do not grant publication authority.

Accepted outcomes retain exact payloads or hashes of immutable private bundles.
Publication recovers an accepted unpublished result before replacement inference.
Public digests, completed reviews, and proposal decisions expose only their closed
record schemas; captured transcripts, memory bodies, before-state, and replacement
content stay in private control storage. Public text alone cannot prove acceptance
or decide a proposal. Source identities, original root/target/client authority,
exact bytes, and accepted hashes remain binding through recovery.

Successful completed-turn saves freeze one immutable evidence bundle and pin its
source before marking it ready. Compaction, clear, session-end, or idle sealing
admits digest work without capturing incomplete responses. Idle sealing ends a
configurable quiet period after the last completed root turn of a session that
stays open and runs no request, so long-running sessions produce digests. Clear sealing uses selected
pre-clear checkpoints, preserving their evidence and captured session title.
Capture, seal, and public metadata admit the same closed trigger vocabulary;
repeated sealing preserves the first trigger. Stable fork-point identities define
coverage, so Rewind's repeated turn numbers cannot alias old work. Capture coverage
survives public digest expiry. Discovery batches fresh expiry probes and bounded
public-entry reads in groups of 16; coverage reads are similarly batched and
still fail closed. Storage authority observations do not survive their inspection
call. Advisory prompt discovery separately reuses validated local entries while
public-record and expiry-marker source attributes remain unchanged, with a
ten-second refresh throttle and age filtering on every use. Remote discovery
retains throttled fresh reads. Digest inference uses frozen model policy without
a live session or tools. Only evidence supporting reusable user, feedback,
enduring project, or reference knowledge becomes a public note. Tasks, blockers,
deadlines, progress, and decisions belong in trackers or maintained documentation;
session continuation belongs in compaction. Easily recovered facts and debugging
fix recipes are excluded. Consolidation independently evaluates qualifying
knowledge, even for older progress-oriented notes. An accepted,
validated all-none digest instead records private turn coverage and retires its
capture without publication or a consolidation opportunity. Both paths retain
coverage before releasing source pins and recover without replacement inference.

Consolidation captures bounded memory, instructions, source observations, and
selected digest evidence. It uses scoped read-only tools in a sessionless request.
Only a terminal validated reply can be accepted. A published general review
advances coverage; focused reviews, failures, private results, and timestamps do
not. Proposals retain exact before-state and evidence until their decisions and
recovery dependencies are resolved. Propose is the default; auto applies memory
and instruction proposals through the same checked application.

Completed, durably saved root turns provide automatic opportunities for journal
recovery/cleanup and memory review/reconciliation. Publication and explicit
memory operations retain their existing opportunities. Opening a session only
queues journal recovery, digest processing and a consolidation offer; the
session chooser launches nothing, and cleanup and memory-decision recovery stay
off session startup. While a root session is open, one idle maintenance timer
gives each local Linux workspace a processing opportunity, a consolidation offer
and the hourly-throttled cleanup once Emacs has been without input briefly. A
capture that exhausts its three automatic attempts is reported once per Emacs
session and waits for explicit retry or discard. For local
Linux workspaces, deferred root-turn checkpoint preparation, scheduled journal
recovery, digest discovery/admission and retention execute in short-lived batch
Emacs workers. Checkpoint preparation reads a frozen committed publication without
source mutation authority; the editor checks its live request and unchanged head
before publishing and pinning under its own lease. Admission remains held through
this continuation, with cancellation and a bounded preparation deadline. Explicit
checkpointing and non-portable sessions remain synchronous. Scheduled memory publication recovery and local consolidation
recovery, admission, scope preparation and accepted-result publication use the same
boundary, with resolved roots, thresholds and original client identity supplied by
the editor. Marked-write reconciliation retains live-buffer checks in the editor;
root-turn consolidation is offered after that recovery finishes. They load no user init or serialized provider
configuration. They reuse the same claims, validation, and publication
paths. Digest preparation returns only the capture identity and fenced claims; the
editor rechecks ownership before inference. Model requests retain the configured
native backend. Remote scheduling remains
in the editor. Live-buffer artifact cleanup also remains there because its
retention proof includes active buffers and gptel context. Worker failure leaves
fenced claims and retained evidence, rather than inferring completion.

The main session menu only renders cached memory observations, explicitly marking missing
or old counts. Opening or refreshing the Memory table collects current evidence.

Application holds both workspace and original target-root claims. It persists
complete private intent and a hash-only target marker before mutations. Curated
writes, rollback, claim settlement, and marker retirement share a target-side
`flock` and guarded pinned program. Claim expiry alone cannot remove an unresolved
marker. Reconciliation compares exact before/after states; it does not infer a
successful write from an unmarked intent. Reversal is a separate accepted intent
and decision, preserving the original application as evidence.

Index freshness is checked per affected destination: the proposal's topic and
merge sources must retain their captured entries or absence. Unrelated manual
edits and changes from other reviews are preserved without replaying write
history. Application retains the complete current index snapshot as its expected
before-state; exact file checks protect preparation through commit. Topics and
instructions still require their original state, and reversal still requires
the exact recorded after-state.

Private journal records live at `.mevedel/state/journal/`, alongside other
generated workspace state; public journal entries stay at `.mevedel/journal/`.
Standard `.mevedel/memory/` roots coordinate through their sibling
`state/memory-write/`. Other memory and instruction roots use
`ROOT/.mevedel/state/memory-write/root/`. These locations depend only on the
original target, so separate workspaces still share ownership. The `root/`
child keeps workspace instruction claims separate from its standard memory
claims. Memory inventory and proposal paths exclude nested `.mevedel/` state.

Ordinary journal recall defaults to 14 days and is independent of physical
retention. Unreviewed digests and unresolved proposal/recovery dependencies remain
stored after recall ends. Fully reviewed public digests retire at idle once all
proposals are terminal and references end, regardless of creation age. Private
evidence and checked undo remain for `mevedel-memory-history-max-age-days`
(default 14) after the latest terminal decision. No-action reviews use completion
time; reversal restarts the window. The memory cockpit exposes Candidates,
Memories, and History; confirmed topic deletion uses a structured `user` producer
and the same accepted transaction/undo path without inference.

Obsolete settled control pairs are pruned after their deadlines while preserving
the newest generation and all retained references. Claim admission, settlement,
and pruning share the target lock and verify the generation within it. Idle
batches remove at most 200 pairs and 50 content groups, scheduling another batch
only after progress. A fresh listing can rule out pruning in a directory that
contains only the newest or referenced generations. It cannot authorize a deletion;
candidates still require fresh claim/deadline checks and locked byte verification.
Minimal coverage and retirement identities remain. Interactive decisions wait
for this client's active cleanup batch to release its claims, bounded by its
120-second lifetime and with quitting available. Bulk decisions defer further
cleanup until completion; foreign ownership still prevents conflicting actions.

Expiry accepts a hash-bound manifest before deletion,
hides expired entries before removing bytes, and deletes complete dependency
groups. Later mutations finish accepted expiry before pinning new evidence.
Curated memory is outside journal expiry. Public retrieval uses `memory://journal/`.

The [memory manual](../memory.md) owns configuration, limits, scheduling, proposal
syntax, commands, inspection, and recovery procedures. Control-program mechanics
remain in [ADR 0101](0101-carry-control-operations-as-one-pinned-program.md).

## Rationale, alternatives, and consequences

Inference can outlive its source buffer or client. Checking a deadline and then
writing leaves a stale-callback race. Session leases also own conversation
mutation and transfer; manufacturing a session solely to hold journal work would
couple unrelated lifecycles. Immutable outcome election separates acceptance from
publication and permits recovery without the original provider or process.

Claims do not renew. Expiration fences acceptance but cannot physically stop a
provider request on an unreachable client. Local owners cancel timed-out work and
ignore rejected callbacks. UI counts, scheduling caches, and telemetry remain
observations, never authority. Failed or unavailable records remain inspectable.

Private evidence can be sensitive despite credential-free claim records. Original
root authority gates body inspection and application on another client. Target
coordination requires Linux `flock` on the storage host. Retention is intentionally
longer than ordinary recall when evidence or recovery still needs it; the recall
limit is not an erasure guarantee.

## Decision history

- **Remote journal work ran one target program per check.** A September 23
  capture of a remote session found 117 control programs, about 12 s of
  blocking, in the journal and memory work that follows a request's
  completion. The user felt this as hangs while typing and scrolling. On a mock
  remote target, the same flow issued 37 programs for the save and checkpoint,
  14 for a seal, 40 for digest processing and 60 for a cleanup drain. Claim
  acquisition had used six programs; it now uses three. One program ensures the
  directory, lists it, and reads the target clock. A second reads the newest
  claim with its outcome, and the election is the third. Settlement relies on
  the election's own `verify` of the claim bytes instead of reading them first.
  Capture markers and descriptors are observed in batched programs. Missing
  parents are created in one program. Checkpoint and seal run as one durable
  transaction, so they share a single clock reading. The replay went to 28,
  11, 32 and 50 programs respectively. Proof cadence is unchanged: every
  resumed step still checks claim ownership afresh, and elections remain
  exclusive creates.

- **Retained-only claim directories caused needless cleanup work.** A copied
  September 22 multi-agent journal fixture took about 14 seconds per cleanup
  pass, even after all eligible pairs were gone. Filtering retained generations
  from a fresh listing before reading records and yielding removed 122 target
  programs and about 2.3 seconds per pass in the isolated replay. The first pass
  removed the same 17 pairs; later passes reported no further work. Ownership
  checks before actual resumed work and locked deletion remain unchanged. This
  reduces child-process work; it is not a measurement of editor input latency.


The amendments to **ADR 0117** are consolidated below. These are the failures,
measurements, and constraints that explain the current boundaries; the manual
contains the corresponding operational details.

### Graphical responsiveness

A shared graphical capture on 2026-09-22 measured an 891 ms journal checkpoint,
1.30 s recovery, and 668 ms processing admission after a request. Merely scheduling
work later still occupied the editor when callbacks ran. Isolated subprocess
experiments kept the parent event loop responsive during multi-second maintenance.
Local recovery and retention now run in separate Emacs processes, while native
provider configuration and live-buffer retention remain with their existing owner.
Batched marker discovery avoids per-marker process dispatch, and fully retired
capture tombstones no longer enter recovery. Ownership checks use the existing
target-side newest-generation proof rather than shipping full histories; claim
pruning stops at its budget instead of checking the rest of an already-full batch.

### Ownership and safe application

- **An ownership check followed by a client-side write was insufficient.** A
  client could pause after the check, resume after recovery retired its marker,
  and overwrite a successor. Workspace ownership also could not serialize two
  workspaces sharing a global root. Independent root claims, a durable pending
  marker, and target-side locking across guard plus mutation replaced that gap.
  Independent Emacs tests paused writers inside the program and demonstrated
  waiting takeover and rejection of later old writes/deletes locally and over
  TRAMP. Replacement mode is applied before rename to preserve recognizable
  crash states.
- **Unconditional rollback lost intervening edits.** A synchronization hook could
  write new content and fail, after which snapshot restoration erased it. Exact
  expected-after matching replaced unconditional restore. An ownership-transfer
  test admitted a successor during buffer synchronization and confirmed the old
  transaction could not undo its files.
- **PID-lock recovery could delete a normal resume's replacement.** Creation,
  replacement, release, and stale sweeping now share native Emacs file locking
  around expected-holder comparison and mutation, without prompting background
  recovery to break a live mutation lock.
- **One proposal's index update made another proposal in the same pass stale.**
  Initially, confirmed, hash-verified same-pass index transitions advanced the
  expected snapshot while external edits still blocked. On 2026-09-24, inspection
  of six stale candidates showed that five were blocked solely by unrelated
  index changes; bulk acceptance had made two older candidates stale itself.
  Same-pass replay therefore failed at the user-facing cross-review boundary.
  Entry-level comparisons replaced that replay: unrelated index changes from
  any source survive, affected-entry and topic conflicts still block, and the
  transaction retains exact current index bytes for commit, recovery, and undo.
- **A private intent alone could misattribute external writes.** Recovery now
  reconciles verified marked attempts after accepted publication recovery.
  Complete applications, untouched attempts, partial writes, and foreign state
  remain distinct. Reversal binds the original intent/hash and exact snapshots
  rather than selecting a new target or baseline.
- **Public decisions alone were not acceptance evidence.** A rejection's private
  hash is accepted before publication. Tests inserted an unaccepted public record
  and confirmed it could neither decide/reject a proposal nor enter later model
  evidence. Rejection checks original root authority without requiring unchanged
  topic bytes because it performs no edit.
- **Auto's old return value could count another call's idempotent result as new
  work.** A confirmation callback now reports paths written by that call; counts
  deduplicate shared indexes. Cancellation completes the current checked operation
  and holds the remaining proposals.

### Evidence and request boundaries

- **Filename absence did not prove safe creation.** A parent directory could be
  a symlink into another root. Captured inventories now retain ordinary directory
  names and reject non-directory parents. All configured memory roots remain
  excluded from workspace investigation even when their own capture failed;
  failed reads cannot enlarge source authority.
- **Ordinary ranged Read could accumulate a huge single line before truncation.**
  Source investigation now first obtains a pinned 512-KiB snapshot. Oversized
  content stays unknown. The reserved coordination directory is excluded both
  to avoid control-state disclosure and to prevent claim history exhausting the
  bounded inventory.
- **Sessionless searches lacked individual cancellation.** External helpers now
  return the existing process owner's cancellation function. An empty synchronous
  `.mevedel/**` Glob also left gptel's immediate follow-up in a deleted private
  copy directory. Native callback and local HTTP tool-loop tests reproduced the
  failure; restoring the caller directory fixed it without delayed callbacks,
  retained copies, provider retries, or new model instructions.
- **Raw evidence size and HTTP completion were insufficient request boundaries.**
  Admission measures the prepared provider payload, including schemas and roles,
  on initial and follow-up requests. Only terminal FSM completion validates the
  final reply. Scoped read tools suppress inherited interactive confirmation,
  which otherwise stalled sessionless work. A local HTTP test covered provider
  parsing, Read, follow-up preparation, final validation, and cumulative usage.
- **The first configured-model evaluation failed at a Unicode boundary before
  inference.** Embedding JSON encoder UTF-8 bytes as prompt text broke gptel's
  next serialization. Decoding to text fixes request input, rejection evidence,
  journal headers/bodies, and quoted topic metadata without a new persisted
  format. Regression cases check admission, inspection, metadata round trips,
  and equality of patch text and decoded bytes.
- **A long digest request spent all 4,000 output tokens on reasoning and returned
  no digest.** New captures disable reasoning when the selected model supports
  that control; otherwise they retain provider defaults. Existing captures keep
  their frozen policy. This does not establish a server ceiling on providers
  that lack an output-limit control.

### Capture, publication, and recovery

- **Clear separates completed work without ending the session.** Its distinct
  `clear` seal preserves that boundary in published metadata without depending on
  a later compaction or close. The existing frozen-checkpoint and first-seal
  rules retain the pre-clear title and evidence, while stable turn coverage
  prevents repeated clear, compaction, or close from duplicating that work.
- **Separate snapshot files could detach metadata from its notes after a crash.**
  One exclusively created descriptor/evidence/policy bundle replaced that
  interval. Source-local capture pins let session cleanup honor evidence without
  discovering a workspace journal. Pins precede readiness, and malformed pins
  block collection.
- **Turn numbers repeat after Rewind.** Coverage uses persisted fork-point
  identities. Deriving it solely from surviving public digests also allowed
  expired/deleted work to reappear in new captures. Identity-only coverage records
  now survive expiry; accepted-result recovery repairs the interval between public
  publication and coverage before releasing the source pin.
- **Relative deadlines could diverge.** Workspace digest admission and the
  capture's acceptance claim now share one exact target deadline, preventing a
  capture from remaining eligible after workspace admission expired.
- **Accepted expiry could be undone by late evidence pinning.** Preparation now
  recovers expiry under journal mutation ownership before validating and pinning
  selected bytes. A crash fixture with an accepted unapplied expiry confirms the
  old selection is refused. Another fixture expires an inference owner, admits a
  successor with its own pins, and delivers the old success; it cannot publish
  or disturb the successor.
- **Retired captures could leave pins behind or be recreated.** Recovery checks
  a successor's retained turn coverage before old pin release and raw-bundle
  deletion; retirement remains recorded. Discard accepts an omission containing
  original source identity before releasing the pin. A marker alone could not
  recover the source after descriptor corruption. Accepted digest output takes
  precedence over later discard.
- **The 24-KiB public entry cap failed on complete long-session turn IDs**, and
  decoder separator whitespace exhausted an otherwise valid 16-KiB body budget.
  Public entries now have an 8-MiB total bound, with full body allowance reserved
  at metadata admission and separators excluded from the body budget. Oversized
  descriptors are refused before pinning rather than dropping coverage IDs.
- **Interrupted write recovery initially depended on opening the proposal table.**
  Workspace activation then scheduled the same checked reconciliation independently
  of capture settings. It coalesces, respects transport/live ownership, creates no
  state in an unused workspace, and cancels queued work on exit. `/remember` uses
  that sessionless review owner and cockpit, replacing the report-only remember
  skill with one command route.

### Retention and cost

- **Opening the main menu synchronously refreshed memory evidence after ten
  seconds.** The September 21 profile attributed 351 of 359 menu CPU samples
  and about 65 MB of allocations to this collection. Cold checks took 3.36 s
  from the view and 3.40 s from the data buffer; warm checks took microseconds.
  The menu now reads observations only and labels unknown or stale counts.
  Explicit memory inspection retains fresh collection and checked recovery.

- **One-year age-only retention could delete unreviewed evidence.** A native
  cleanup regression reproduced that loss. On 2026-09-13 ordinary recall changed
  to 14 days while physical cleanup retained unreviewed work and recovery
  dependencies. Source-session exclusion also blocked long-lived-session review;
  completed immutable digests are now eligible without source-session authority.
  Sparse work gains an opportunity one day before recall expiry, subject to the
  general time gate. Manual mode and the propose default remain.
- **Per-file expiry limits split dependencies.** Reversal needs its original
  intent and accepted pass. Expiry now selects complete groups, uses pre-deletion
  observations for covering reviews, and records retirement before deletion.
  Tests expire 53 public records as one group and interrupt deletion after pass
  and decision bodies disappear. Accepted manifests finish the remaining work
  without resurrecting history; the 4-MiB manifest cap bounds admission.
- **Recovery repeatedly republished already-present decisions.** It now compares
  accepted records with one public observation and replays only missing/mismatched
  publications or incomplete pin release. Missing review recovery still requires
  admitted pins and digest bytes; tests remove a publication and restore a pin
  to exercise both paths.
- **The 52-decision cleanup fixture exposed redundant control round trips.**
  Setup/teardown, recovery, and cleanup calls fell from 2,131/321/632 to
  1,693/269/576 after batched fresh preconditions and decision reads. Three-run
  local medians fell from 14.10 to 12.35 seconds overall, 1.28 to 1.11 for
  recovery, and 2.74 to 2.53 for cleanup, retaining all 52 validated decisions.
  The settlement program's own deadline guard removed an earlier clock call.
- **A later 53-record profile found 4,068 control programs and 213
  authentications of one accepted pass.** Recovery/expiry took 2.72/5.56 seconds.
  One authenticated immutable pass is now reused within an observation, discarded
  after publication and between operations. Rejection evidence likewise reuses
  the decision reader's authenticated bundle, removing a second authentication;
  subsequent observations still detect changed bytes. Carrying authenticated hashes
  and reading each public decision once reduced those times to 1.34/2.89 seconds
  without shrinking history or parallelizing the test. Deletion still rechecks
  accepted hashes; tests verify later corruption and edits remain errors.
- **Cancellation and storage rejection lost received usage.** A read-only usage
  snapshot preserves costs before terminal publication, including oversized
  non-streaming replies. Telemetry retains counts and categorical outcomes, not
  private evidence or authority.
- **Fixed `notes.md` preference was replaced by freeform shared working files.**
  The shared-file evaluation recovered corrections without prescribed names or
  folders. Capture reads shared files before session files within one 32-KiB
  bound, retaining provenance without attributing other sessions' notes to this
  session. The journal namespace moved to `memory://journal/` without changing
  storage or granting curated-memory write permission; see
  [ADR 0104](0104-keep-resource-addresses-closed-and-capability-neutral.md).

### 2026-09-16: resolved history and coordination cleanup

Inspection of the live journal found roughly 450 mutation records and processed
source notes still occupying the public journal. Creation-age cleanup conflated
recall with review completion, and coordination outcomes had no collector. The
replacement separates immediate processed-note retirement, a resolution-based
14-day evidence/undo window, and dependency-aware bounded control collection.
The previous age gate for processed notes is removed. Native generation-race
tests and a 1,000-generation fixture exercise fencing and the 200-pair batch
limit. Shared tables remain; stored memory inspection and user deletion reuse
the existing checked write protocol.


### 2026-09-16: collect generated workspace state

The former `journal/state/` and `memory/.mevedel-memory-write/` locations mixed
internal bookkeeping with browsable evidence and curated topics. Grouping them
under `.mevedel/state/` makes their ownership explicit and supplies one generated
state ignore entry. This changes storage paths and private expiry-manifest paths;
old persisted control records are not read or automatically migrated. Public
journal retrieval and its domain-specific retention rules remain unchanged.

The state inventory also exposed missing cleanup owners for generated media,
review packages, and workspace diagnostics. The hourly idle opportunity now
collects old unreferenced generated artifacts after a complete retained-state
search; foreign session owners and uncertain reads postpone it. Workspace
diagnostics use size-based rotation with one archive. These policies do not
expire recovery evidence or plugin-owned data. See the current
[storage and retention contract](../architecture.md#generated-workspace-state).

### 2026-09-19: keep workspace maintenance out of session startup

The startup profile attributed 12.2% of CPU samples to journal cleanup and 4.2%
to memory decision recovery. Previously, the chooser and conversation setup
scheduled workspace-wide maintenance on immediate timers; those callbacks still
blocked the Emacs UI. These opportunities now run after completed, durably saved
root turns. Explicit memory operations retain checked recovery, and publication
still schedules its existing processing and cleanup. Merely opening a session
no longer launches consolidation or workspace recovery scans. (Partially
reversed on 2026-10-06 for journal processing; see below.)

The 2026-09-22 follow-up found a remaining synchronous journal-retention call
inside file-session expiry, also reached by project choosers and exit. A chooser
with 56 retired captures spent 4.27 seconds there. Removing that unrelated call
keeps file-session expiry intact and leaves journal retention to its own owner.

### 2026-09-21: yield during scheduled cleanup

A real configured-provider patch request spent 272 ms in journal cleanup after
its completion message. A separate backlog measurement showed that a bounded
record count still permits seconds of consecutive filesystem work. Scheduled
cleanup now advances native Lisp generators between ownership phases and
coordination records, using the same main-thread pattern as saved-history search.
Claim ownership is checked afresh before each resumption, including nested
coordination steps. Closing a pending iterator settles its acquired claims.
Explicit synchronous lifecycle cleanup drains those same steps.

The artifact-retention phase still checks live buffers and deletes as one unit;
yielding between its final live-reference check and deletion would permit loss
of an artifact newly referenced by unsaved input. Individual filesystem
operations and content dependency groups remain indivisible. This changes
scheduling, not the accepted-manifest, retention, or target-lock contracts.

### 2026-09-21: separate completion publication from checkpointing

After staged background cleanup, a second real patch request still delayed a
20-ms timer by 375 ms: session save and journal checkpoint together consumed
329 ms. Terminal settlement now returns to the event loop after an expensive
step; publication and journal checkpointing are distinct steps under the same
admission hold. The request and its file snapshots remain owned until the
checkpoint and terminal hooks finish. Transport cancellation and source death
retire the continuation. The existing durable publication and capture APIs keep
their atomicity and authority checks; this changes the sequencing of their calls.
Already-due journal and memory opportunities also yield between jobs instead of
running in one consecutive timer batch.

### 2026-09-22: isolate consolidation storage from the editor

Capturing 100 memory topics required 103 storage programs and 478 ms. Batching
reduced this to 14 programs and 263 ms, still too long for foreground work.
Local consolidation now hands explicitly resolved roots and instruction paths
to the maintenance child for scope capture, immutable preparation and rejection
history. The editor retains the originating buffer, configured provider and
claim deadline and checks ownership again before inference.

A 20-digest review then spent 930 ms accepting and publishing its result.
That storage phase now uses the same isolated child. Cancellation may stop an
unaccepted result; a completed claim remains durable recovery authority even
if its child dies before returning publication. Checked proposal application
remains in the editor. Remote roots keep their target-native path.

### 2026-09-22: include recovery and admission in the worker boundary

The shared graphical replay still paused for roughly 4.6 and 4.5 seconds at
completion. A copied store reproduced two publication-recovery scans at about
4.5 seconds and 884 control programs each, including automatic opportunities
that never started inference. Offloading scope capture alone left this work in
the editor. Scheduled recovery now owns a cancellable child, and consolidation's
existing preparation child also performs recovery, selection and automatic
admission. Its deadline starts before those operations. Skipped admission
returns a scheduling observation without a provider call.

The editor rechecks ownership, reconciles marked writes using live buffers and
only then offers root-turn consolidation. It releases ownership for that phase,
so the later preparation claim still recovers fresh state; the two scans are not
replaced with an unsafe completed-recovery cache. The first implementation still listed retained writes synchronously (about
250 ms in the copied-store probe); the follow-up below divides that editor phase.
Explicit recovery and remote workspaces keep their existing target-native path.

A 20-digest, 90-KB memory fixture measured 2.47 seconds maximum timer delay on
the synchronous path versus 36 ms with real preparation/publication children.
End-to-end time increased from 2.67 to 3.28 seconds. These isolated measurements
exclude provider inference and do not establish graphical typing latency.

### 2026-09-22: bound remaining completion inspections

The rebuilt graphical run confirmed that the 3.95-second recovery scan ran in a
child. Its remaining editor costs included a 681-ms checkpoint, 593-ms journal
admission and 224-ms retained-write inspection. A copied store showed 32 control
programs for 31 coverage records and 71 for 35 public entries. Fresh bounded
batching reduced these to three and seven programs, with median times falling
from 157 to 90 ms and 306 to 152 ms respectively. Existing path proofs, byte
limits, expiry checks and schema validation remain in force.

Scheduled recovery now checks one retained write per callback after settling the
publication claim. Every record still gets fresh original-root and live-buffer
checks; no child classifies unsaved editor state. The job retains cancellation
ownership and its deadline until inspections finish. Quit and late replies cannot
strand or restart it. Explicit synchronous recovery remains synchronous. The
individual marked-write reconciliation can still be expensive; splitting records
does not preempt an operation already in progress.

The same capture exposed a queued journal-recovery offer colliding with memory
recovery's claim. The busy return retained a ten-minute placeholder cooldown,
which then suppressed the root-turn post-recovery offer. A busy opportunity now
clears only its own placeholder; a fresh admission observation remains cached.
A regression holds a real claim for the first offer, releases it, and verifies
that the next activity opportunity admits exactly one review.

In six alternating copied-store trials, inspecting the retained writes together
produced a median 242-ms maximum timer delay; one record per callback reduced it
to 44 ms. Total child recovery and inspection rose from 4.00 to 4.05 seconds.
These are headless timer measurements, not graphical input-to-paint latency.

### 2026-09-22: prepare root checkpoints and digest admission outside the editor

The rebuilt graphical request still measured a 636-ms checkpoint and 558-ms
journal processing callback, with 251 and 181 ms of GC respectively. Deferring
those whole functions to timers did not divide their blocking work. Deferred
portable root settlement now suspends while a child prepares immutable evidence;
only the owner publishes and pins it. Scheduled digest opportunities also perform
discovery, accepted-outcome recovery and claim admission in the child. Live request
identity, current publication and source authority remain editor-side checks.

A three-trial replay of the captured 825,335-character transcript and copied public
journal entries produced identical 301,157-character evidence on both paths.
Median 20-ms timer delay fell from 182 to 110 ms, while warm editor allocation fell
from 30.4 to 14.5 MB. End-to-end checkpoint time increased from 202 to 561 ms;
publication and lease handling still run synchronously. A separate unsealed-capture
digest-opportunity replay reduced median timer delay from 175 ms to below 1 ms,
and editor allocation from 5.0 to 0.28 MB, while total time rose from 195 to 369 ms.
These are isolated storage replays, not a new graphical typing measurement.

### 2026-09-23: reuse unchanged local discovery snapshots

The next graphical capture recorded 79 journal-index refreshes with a median
128 ms. The ten-second throttle prevented per-event reads but still reread an
unchanged journal throughout long requests. Local discovery now compares source
attributes for public records and expiry markers before refreshing validated
entries; a second observation detects changes during the read. Access times are
excluded so reading does not invalidate the snapshot. Missing observations and
remote storage keep the ordinary read path. Age limits still apply on every use;
evidence and mutation authority still read storage freshly.

Alternating trials on a copied 40-entry journal reduced warm refreshes from
134 ms to 0.33 ms with identical results. Cold reads remained about 134 ms.
Regressions cover edits with restored modification times, replacements, expiry,
corruption, read failures, source races, root changes and cached-only discovery.

### 2026-09-24: defer decisions behind cleanup and allow no journal note

Accept/reject scheduled cleanup under the same consolidation claim needed by the
next decision. The editor could report no running review yet fail that decision
with "Memory consolidation is busy". A regression holding real cleanup claims
reproduced this. Decisions now wait for this client's active batch to settle,
without stealing claims or killing maintenance children; bulk commands defer
new cleanup until they finish.

The digest contract also encouraged a note on every eligible capture, and even
four empty sections published a public record. Generation now explicitly selects
noteworthy events. Validated all-none output is accepted completion without a
note: turn coverage precedes source-pin release, and interrupted retirement
recovers the accepted omission without another model call. Consolidation likewise
prefers No action when dated activity supplies no useful lasting context.

The initial selection rule included consequential decisions and unresolved
blockers. User review identified these as tracker or documentation state, not
persistent memory. Selection now retains evidence only for reusable knowledge
in the user, feedback, enduring project, and reference categories. The direct-save
prompt and manual follow the same boundary; the journal's Unfinished field holds
uncertainty about qualifying knowledge rather than pending work. Existing records
remain unchanged, while subsequent reviews apply the stricter criteria.

### 2026-10-06: make advertised automatic maintenance run

The user required that behavior described as automatic actually happen. An audit
found four gaps. Digest generation admits only sealed captures, and sealing
happened only at compaction, `/clear`, session close and Emacs exit, so a
session left open for days never produced a digest. Work sealed at exit started
no request, by design, and then waited for the next completed root turn in that
workspace. The "hourly idle cleanup opportunity" had no timer: one hour was only
the throttle on cleanup offered after root turns. And in every workspace type
except `project`, artifact cleanup triggered at turn settlement always found the
settling session's own PID lock and postponed itself.

Idle sealing now closes a checkpoint after a quiet period
(`mevedel-journal-seal-idle-minutes`, default 20) with the new closed trigger
`idle`, under the same root, read-only and lease rules as other seals. Opening
a session queues recovery, processing and a consolidation offer. This partially
reverses the 2026-09-19 decision: the startup cost it measured was journal
cleanup (12.2% of samples) and memory-decision recovery (4.2%) on immediate
timers in the editor. Both stay off session startup. Recovery and digest
admission have run in batch children on local workspaces since 2026-09-22, and
the remaining editor work only queues behind transport idleness. Remote
workspaces still do this work synchronously when the transport is idle, as they
already did after every root turn. A single maintenance timer
(`mevedel-journal-idle-maintenance-minutes`, default 10) offers local workspaces
processing, consolidation and cleanup while Emacs is quiet. It drains one digest
per opportunity and makes the hourly cleanup throttle describe a real schedule.
Remote workspaces are skipped because their editor-side storage work would block
input without a triggering user action. Artifact cleanup no longer treats locks
held by this Emacs, whose buffers it searches directly, or stale same-host locks
of dead holders as foreign owners. Exhausted captures previously disappeared
from automatic processing silently; they now warn once and point to the job
browser.

`auto` previously held instruction proposals, including `AGENTS.md` changes,
for approval. That made the mode only partly automatic. Instruction proposals
already use the checked decision path, with exact before-state, a durable intent
marker and checked reversal, so `auto` now applies them like memory proposals.
`propose` remains the default for users who want to approve every change.
