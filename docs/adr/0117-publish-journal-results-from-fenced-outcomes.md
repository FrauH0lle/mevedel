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
source before marking it ready. Compaction, clear, or session-end sealing admits
digest work without capturing incomplete responses. Clear sealing uses selected
pre-clear checkpoints, preserving their evidence and captured session title.
Capture, seal, and public metadata admit the same closed trigger vocabulary;
repeated sealing preserves the first trigger. Stable fork-point identities define
coverage, so Rewind's repeated turn numbers cannot alias old work. Capture coverage
survives public digest expiry. Digest inference uses frozen model policy without a
live session or tools; accepted output is published before source pins are released.

Consolidation captures bounded memory, instructions, source observations, and
selected digest evidence. It uses scoped read-only tools in a sessionless request.
Only a terminal validated reply can be accepted. A published general review
advances coverage; focused reviews, failures, private results, and timestamps do
not. Proposals retain exact before-state and evidence until their decisions and
recovery dependencies are resolved. Propose is the default; auto uses the same
checked application, while instruction changes always await approval.

Completed, durably saved root turns provide automatic opportunities for journal
recovery/cleanup and memory review/reconciliation. Publication and explicit
memory operations retain their existing opportunities. Opening a conversation
or the session chooser does not launch workspace-wide maintenance.

Application holds both workspace and original target-root claims. It persists
complete private intent and a hash-only target marker before mutations. Curated
writes, rollback, claim settlement, and marker retirement share a target-side
`flock` and guarded pinned program. Claim expiry alone cannot remove an unresolved
marker. Reconciliation compares exact before/after states; it does not infer a
successful write from an unmarked intent. Reversal is a separate accepted intent
and decision, preserving the original application as evidence.

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
only after progress. Minimal coverage and retirement identities remain.

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

The amendments to **ADR 0117** are consolidated below. These are the failures,
measurements, and constraints that explain the current boundaries; the manual
contains the corresponding operational details.

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
  Only confirmed, hash-verified same-pass index transitions now advance the next
  expected snapshot. Current target bytes are never adopted as a new baseline;
  external edits still block. Aborted reversals do not advance expectations.
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
no longer launches consolidation or workspace recovery scans.
