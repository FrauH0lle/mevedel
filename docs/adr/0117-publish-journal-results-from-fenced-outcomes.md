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

Recovery authenticates decision metadata in batches of eight, then groups the
bounded metadata by pass; every decision and write intent still undergoes
authentication. Each pass group shares its authenticated bundle and matching
coordinator proof with the write reader. Publication discards all pending
metadata and bundle observations. The read-only cockpit collector reuses its freshly
authenticated write intents when the identity, intent hash and source-bundle
hash match the decision; these observations never authorize a later action.
Pending automatic selection refreshes its public observation
and discards proofs after applying a proposal. Completed pass recovery still
authenticates every bundle and recovers accepted expiry under mutation ownership,
but avoids republication when exact public bytes match and no spare pins remain.
These operation-local observations do not survive a new application claim.
An automatic sequence may retain only its clean recovery predecessor's exact
claim/outcome identity, never those authenticated content observations.

Local automatic application uses one parent-owned fixed-deadline workspace
claim per proposal. The first maintenance worker performs publication/store
recovery. A clean operation may establish an ephemeral recovery predecessor for
the same automatic job. Its next claim must be exactly the following generation,
with both predecessor records freshly verified alongside the new claim's exact
identity, latest generation, absence of outcome and unexpired deadline in one
pinned program. Only that continuity proof omits repeated history recovery;
the next proposal, current status and target authorization are still read anew.
Intervening consolidation claims, context changes, failures, holds, cancellation,
expired/failed outcomes or incomplete accounting discard continuity. New jobs and
deferred retries begin with recovery. Pending discovery remains at the original
tail of the sequence; its read-only child independently authenticates the same
continuity proof before omitting recovery. Unsupported targets retain native
recovery when no continuity proof exists. The editor freshly checks the current
operation and ownership before acquiring
target ownership and committing with live-buffer protections. Pending selection
authenticates current status and omits every already-decided proposal, retaining
undecided extractions and their waiting dependencies. It returns identities only,
and each later operation reauthenticates its proposal
and status. Sequential callbacks yield between proposals, keeping the pass
registered through application completion rather than reporting the queue entry
as completion. Scheduled marked-write reconciliation uses the same owned
recovery prelude, but its live state classification remains in the editor.
Original-target ownership never crosses the child in this design. Automatic
note forward writes recheck that no open session uses the note, cancellation
and unsaved buffers at their mutation boundary; checked rollback keeps its
separate ownership proof.
Reentrant replies cannot enter an executing continuation. Reply identity, origin
binding and freshly read workspace identity fence delayed continuations.
Cancellation fences the child before stopping it; an executing editor transaction
retains its fixed, unrenewed workspace claim until unwind so checked rollback
remains possible. Expiry or takeover still prevents rollback and retains evidence.
Allowed or delivered quit during setup, continuation or cleanup remains visible
cancellation and prevents subsequent application. Interrupted mutation finishes
checked rollback
before cancellation delivery. Only local child launch/registration and the pure
parent handle handoff defer pending quit; request IO stays interruptible, and the
parent receives the handle before exact-token fencing and retirement.
Inherited outer quit inhibition and its pending flag remain unchanged.
Cleanup failure cannot strand the operation registry or suppress its callback.
Settlement failure is reported even after a successful idempotent retry; if no
fence is established, the original deadline performs terminal retirement only
after authoritative settlement/expiry proof. Continued IO failure stays visible
and does not authorize these lifecycle paths to delete an unfenced child.
A nil settlement is not proof:
release requires an accepted outcome or fresh authentication of that token's
durable outcome. A fresh target-clock expiry observation may elect expiry for
the original token, never renew or replace its authority. Scheduled publication
recovery uses the same proof before child deletion and live-write scheduling.
Generic maintenance-process teardown may still force-stop children without
storage settlement; that separate boundary leaves durable evidence for recovery.
Automatic admission reads the
live origin's policy rather than an incidental timer buffer's policy.
Deferred retries retain and freshly validate their original buffer, session,
workspace and identity context, including live policy at forward write boundaries.
Invalid context or explicit cancellation preserves pending decisions without
retrying from another actor; cancel also removes deferred work between passes.
Deferred application retries yield priority to a registered explicit drain,
including its waiting gaps, rather than starting a competing local owner. The
drain waits when local work is already running. After potentially reentrant
busy and identity IO, retry admission rechecks its exact registration and the
running/drain tables without intervening IO before retiring itself and registering
application work. An old callback's error teardown cannot remove a successor.
This addresses the local overlap that can make a drain encounter `Memory
consolidation is already running`; a forced ordering reproduces that mechanism,
not the precise ordering of the observed live interruption.
Scheduled publication recovery likewise retains its original callback and
origin/workspace/root/canonical-key context while yielding with a bounded,
nonzero delay to a registered pass/drain or admitted local cleanup. A drain waits
for already-admitted decision work, including claim-acquisition handoff, but not
queued recovery or its pre-admission freshness checks. Counting those checks as
admission would make recovery and drain mutually defer. Exact-owner checks after
IO prevent cancellation or a changed root/key from admitting replacement work;
teardown cannot erase a successor. This addresses a separately reproduced local
claim collision yielding `Memory consolidation is busy`, without identifying the
original live claimant or suppressing foreign contention and storage failures.
Foreign native claim contention still declines the unadmitted opportunity;
callbacks signify successful recovery or absent state, not attempted admission.
Later maintenance may offer again, without a local foreign-contention retry loop.
Publication recovery is not proposal application and does not inherit its
auto-mode or live-session admission requirements.
Unsupported workers use native recovery through the same guarded executor.
Scheduled cleanup yields priority to this client's registered memory pass,
explicit drain and owned asynchronous decision/recovery work, including gaps
between sequential proposal claims. Forced cleanup remains queued rather than
competing for mutation ownership during application. An exact-job observation
distinguishes actual local cleanup admission from queued timers or transport
callbacks; active cleanup postpones new automatic/drain admission and refuses
explicit review before its claim, without a synchronous editor wait. Neither
observation grants authority or replaces the existing cross-client claims.
This closes the publication-triggered cleanup/application race that could accept
a review but leave its first proposal pending with `Journal mutation is busy`.
Held admission and ordinary cancellation have distinct non-error callbacks;
timeouts, protocol, authentication, IO and cleanup failures stay errors.
Cancellation preserves an authenticated result and its written-file accounting
before stopping later proposals. A cancelled coordinator outcome proves fencing,
not the cause of termination.
Worker eligibility uses fresh authenticated retained-root metadata, not only
current configuration. Remote or uncertain historical state routes to native
editor recovery under the same claim, preserving accepted-expiry recovery and
original remote authority without returning accepted proofs. This conservative
preflight adds an all-history read and can increase total work.
Manual/unsupported remote operations remain synchronous. Remaining editor-side
intent, commit and finalization IO can still block; this is not an asynchronous
filesystem transaction or a guarantee of imperceptible background processing.

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
before publishing and pinning under its own lease. A newer publication of the same
session supersedes the prepared evidence: preparation restarts once against the new
head, and a second supersession leaves the turns to the next checkpoint without a
warning. Admission remains held through
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

The main session menu only renders cached memory observations, marking missing
or old counts. The Memory table requests checked workspace recovery and observes
its pending, dispatching and active owner without replacing callbacks or acquiring
authority. Initial collection does not wait for recovery: a display-only local
child collects committed evidence with existing
validators and returns detached metadata, not accepted bundles or write proofs.
Current-job, context, session, root, fresh workspace identity, client and
configuration checks reject stale replies. Closing the display retires its own
work, not recovery's owner. Candidates and History reuse snapshots and collect
history only when shown; Memories inventories independently, never waits for
history, and scans it in the background only while candidate counts are
unknown, to label the Candidates tab. The table never switches views by
itself. The topic inventory
is display metadata, not a transaction snapshot: an attribute listing validates
cached titles, and changed files are read by batched asynchronous pinned programs
after display. Refresh and completed actions request new observations;
empty views distinguish pending data, completed emptiness and scan failure with
static presentation-only notices, not synthetic rows or action targets. The header
labels committed snapshots and pending recovery. Recovery that changed committed
state starts a fresh scan while the bounded display observer is active; a no-op
recovery keeps the scan and counts. Its 180-second expiry
retires only display observation, retaining rows and any scan with an explicit
incomplete-recovery warning separate from scan errors. `g` refreshes committed
data without clearing that limitation; reopening renews the observer.
Details and mutations freshly authenticate existing evidence. Command return
and full-data readiness differ. Background printing can restore buffer point
while leaving a nonselected window on another row. The memory surface retains
each displayed window's per-tab row identity across loading gaps and restores
its window point without selecting it, preserving independent selections and
the active composer. Remote/uncertain inspection and topic directory listing
can still block; offloading a scan does not establish lower total IO.
Explicit inspection and decision commands retain synchronous authentication and
processing; display responsiveness does not establish nonblocking manual actions.

Explicit backlog drain coordinates successive bounded general reviews through
the same claim and settlement owner; it yields between batches and waits while
local consolidation, application or cleanup runs; other sessions' turns do not
stop it, and notes they use wait. Auto mode offers a remaining ready backlog at the
next idle opportunity instead of waiting another day only while each successful
general batch makes shared-note progress. An authenticated applied delete
(including verified trash relocation), condense, or a keep that suppresses the
reviewed version qualifies;
proposal counts, digest coverage and older pending applications do not.
A successful batch with zero progress ends the chain just like a failed batch:
later offers must pass the ordinary successful-review clock again. A rolling
24-hour ceiling remains (48 successful general reviews by default, including
explicit ones). Published and retired timestamps enforce admission under ownership;
explicit drain bypasses the automatic cap. A returned application error or
recovery requirement stops successor
drain batches even when review acceptance succeeded; the accepted outcome,
confirmed application counts and original diagnostic remain distinct and intact.
This replaces one-batch-per-interval catch-up: a roughly 250-note ready backlog
otherwise took weeks at sixteen
notes per daily review. Request bounds stay unchanged rather than making one
oversized request. The cap limits admissions, not provider billing.

Application holds both workspace and original target-root claims. It persists
complete private intent and a hash-only target marker before mutations. Curated
writes, rollback, claim settlement, and marker retirement share a target-side
`flock` and guarded pinned program. Claim expiry alone cannot remove an unresolved
marker. Reconciliation compares exact before/after states; it does not infer a
successful write from an unmarked intent. Reversal is a separate accepted intent
and decision, preserving the original application as evidence.
A known admission hold or cancellation may abandon an exact prepared intent
without deciding the proposal only after checked rollback and freshly proven
complete before-state. Original authority, tokens and live buffers are checked
before and after electing and authenticating the exact coordinator's cancelled
empty-payload outcome on its original host. The still-owned target's locked
program verifies its token, disk before-state and exact marker before deleting
that marker alone. The immutable unmarked intent remains, with no accepted public
decision. This immutable outcome is invocation-local evidence, not a cached proof
or permission to run paths on another host. After, mixed or uncertain states
retain normal accounted recovery; incomplete retirement failures stay visible.
Scheduled and synchronous recovery fully inspect only records whose marker
exists, plus any record this client has not yet read successfully; markers are
observed afresh in one program per target. A record's marker path and pass
retirement are immutable facts remembered per workspace object, never whether
the marker exists.
Read-only candidate inspection yields between records. Only a marked candidate
starts another owned recovery operation, which rereads its evidence; unmarked
records do not repeat full publication recovery. Unreadable records report their
diagnostic and remain inspectable without blocking independent work.

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
only after progress reaches either limit. A shorter batch stops rather than
feeding another scan with cleanup's own claim generations. A review's public
timestamp can rule out expiry while its history window remains open; candidates
still require the complete authenticated dependency checks.
A fresh listing can rule out pruning in a directory that
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

Shared working files are not journal input. They are untracked, machine-local
and temporary; general consolidation reviews quiet versions directly. Each
admitted note receives one disposition (delete, keep, or condense) beside
ordinary extraction proposals citing it, and a disposal applies only after
those extractions are applied. Quiet attachments no surviving note references
and quiet empty folders receive structured decisions without inference. Files
that memory or instructions cite are not removed silently. Accepted reviews
and decisions are the processing record. Note decisions reuse checked intents
and reversal through a shared-notes target whose coordination stays outside
the cleaned folder; the previous bytes are retained as content-addressed
blobs for the ordinary history window, within a quota. Over-quota attachments
instead retain a verified copy in local system trash before the locked original
removal; trash unavailable on the original target means no removal. Undo from
trash remains checked and depends on those bytes still existing. Oversized
UTF-8 notes within a configured upper bound are reviewed alone in sequential
chunks with a compact running summary, retaining whole-file before-state and
invalidating that summary if the file changes. Attachment reference scanning
streams large text rather than making size alone an incomplete scan.
Auto mode applies them
without prompts, holding any whose file an open session uses.

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

### 2026-10-28: Progress guards automatic catch-up; the cap is a ceiling

The 2026-10-24 catch-up change in `2d2b0991` used an eight-batch rolling-day
cap to bound unattended spend while draining large ready backlogs. Successful
publication alone could rearm catch-up even when every disposition was
rejected, deferred or left unchanged without suppression. Repeated successful
reviews could therefore spend the entire allowance without reducing ready work.

Catch-up now requires authenticated applied note dispositions from that batch,
including keep only when it actually suppresses the reviewed version. This
distinguishes useful retention from a no-op without trusting proposal or
application totals. Zero progress ends the chain and restores ordinary clock
admission. The default ceiling rises from eight to 48 to allow productive
backlogs to drain, but remains a rolling-day spend backstop; zero still disables
automatic review. It bounds successful batches, not provider calls or billing.
Explicit general reviews need progress to seed unattended catch-up, but a failed
progress observation only records its class in telemetry and prevents catch-up;
it does not change the review result. Explicit drain skips this scheduling-only
observation and retains its continuation behavior. Empty-directory pruning
removes no note content and does not qualify as progress. Claims, freshness,
recovery, cancellation and idempotent application are unchanged; the new
observation grants only a later scheduling opportunity, never write authority.

- **Display collection and retry admission gained separate ownership.** On
  2026-10-27, an authentic four-pass, 32-note fixture spent seconds opening and
  switching every Memory view because each repeated full history collection,
  even Memories. Opening also ran synchronous checked recovery. Display-only
  child observations replace command-side scans; mutations keep fresh native
  authentication. Full-recovery verification then exposed complete recovery
  children launched per unmarked write, missing a 45-second readiness bound.
  Read-only per-record inspection now admits owned recovery only for marked
  records. Separately, a real timer replay showed deferred auto work adopting
  an unrelated buffer's policy; a late policy change could publish unavailable
  for an untouched prepared write. Retained original context, typed outcomes
  and checked before-state abandonment preserve pending work without weakening
  original-target, rollback, recovery or accepted-outcome checks.

- **Cleanup no longer feeds its own continuation.** On 2026-10-25, a laptop
  sustained roughly 10% total CPU while cleanup spawned about 1,060 processes
  per second. Read-only selection over its journal took nearly two minutes:
  each recent review scanned hundreds of private records before checking its
  retention deadline. Recent reviews now skip those scans. Claim pruning had
  also counted as progress sufficient to force another full pass, even below
  every work limit; those passes created further claim generations. Immediate
  continuation now requires progress reaching a batch limit. Target ownership,
  accepted manifests, byte checks and history retention are unchanged.

- **Size alone no longer blocks every shared-file cleanup path.** On
  2026-10-24, a 2.6 MB transcript copy made the reference scan incomplete and
  blocked unrelated attachment cleanup; 14 shared items were blocked by size
  limits. Scanning now streams with filename overlap. Useful oversized notes
  can receive bounded chunk review while very large copies remain explicitly
  blocked for manual splitting or relocation. Attachments above the undo quota
  use verified system trash instead of permanent deletion or an unbounded blob
  exception. Remote and unknown platform trash remains fail-closed.

- **Shared files left the journal for a consolidation-owned lifecycle.**
  Capture read shared files before session files within one 32-KiB bound in
  filename order. With about a hundred shared files, every checkpoint repeated
  the same alphabetically first files (`artifact-store-review…`,
  `editor-cpu/…`) as possibly foreign context, and newer notes never reached a
  digest. Digests are lossy per-turn summaries that record no processed note
  version, so cleanup could not safely depend on them. Committing the folder
  had grown it to 57 MB over 66 commits, mostly lab output, although the user
  does not exchange shared files between machines. The user asked for notes
  to be extracted reliably and then removed, automatically and without prompts
  in `auto`. Consolidation now reads complete note versions and records their
  dispositions as decisions; session-local notes stay in capture.

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
publication claim. Every inspected record still gets fresh original-root and
live-buffer checks (since 2026-10-14 only marked or unread records are
inspected); no child classifies unsaved editor state. The job retains cancellation
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

### 2026-10-14: inspect only marked retained writes

Event-loop telemetry attributed recurring post-turn input lag partly to memory
recovery, which every durably saved root turn schedules. A live profile put the
retained-write inspection at 24% of editor allocation during an ordinary gpt6
session. This workspace retained 32 settled intents for reversal; none carried a
marker, yet each recovery read, hashed and validated all of them through about
eleven control programs apiece: 50 ms per callback and 1.56 s per recovery in
the live editor (1,665 ms in a compiled batch replay).

Only a marked record can produce a reconciliation. A record's marker path comes
from its immutable intent, which is created once under its claim identity and
never rewritten, and a write identity is never marked twice. Recovery therefore
remembers each successfully read identity's marker path, and `retired` for a
durably retired pass, and asks one batched existence program per target which
of those markers exist. Marked records, and records not yet read by this client,
get the unchanged full inspection with fresh authority and live-buffer checks.
An unreadable record is never remembered, so it keeps reporting its error.
Nothing about recovery completion is cached; the 2026-09-22 rejection of a
completed-recovery cache still holds. The trade is that an unmarked record whose
original root later became unavailable no longer yields an `unavailable` row
during recovery; no recovery caller consumes those rows, and the Memory table
still inspects every record.

The same replay now checks the 32 records in 29-31 ms: one listing and one
batched marker program.

### 2026-10-17: treat a newer session publication as supersession

After a background-event turn the editor warned "Journal capture checkpoint
failed: Checkpoint source changed during preparation" and captured nothing. The
session telemetry showed the turn save, then a `session-save :kind
agent-registry` publication 0.7 s later, inside the roughly 1.8-s preparation
window. The debounced agent-state save (two seconds after mailbox consumption
or an activity transition) had moved the head. A reproduction through
`mevedel--turn-checkpoint` showed the head check as the only failing condition.
Execution completion records publish the same way. The rejection was correct,
because the prepared head and source revision were stale. The warning was not:
the same live root and request still owned the source, and nothing had failed.
The owner now restarts preparation once against the current head. A second
supersession returns no capture without a warning, and the next checkpoint
covers those turns because preparation always takes every uncovered completed
turn. A replaced request, a dead or reassigned root, a changed save path, a
pending publication, disabled journaling, a worker error and the deadline still
report failure. The restart costs one more child preparation, only when this
race occurs.

### 2026-10-27: split automatic application recovery from editor mutation

Native automatic application stalled the editor for minutes while repeatedly
authenticating and republishing retained history. Operation-local authenticated
observations and exact-publication checks removed redundant body reads and
publication programs without retaining authority across attempts. Automatic local
application now runs recovery in an owned child and yields between editor-side
transactions; original-target ownership and live-buffer safeguards remain in the
editor. This reduces contiguous editor work, not all commit blocking or total IO.

An initial child split returned terminal historical proposal identities too.
An isolated replay with 64 retained decisions and eight fresh applications
spawned 73 recovery workers, including 64 that only skipped a decided proposal.
Pending discovery now authenticates status before returning identities, matching
the synchronous selector. A later editor attempt still authenticates its own
current status; neither the selection nor the eligibility preflight returns
reusable acceptance authority.

### 2026-10-28: do not withhold committed display data behind recovery

Initial history collection previously followed workspace recovery retirement.
Recovery correctly yields to a registered consolidation drain, so opening the
cockpit during a drain could leave Candidates and History blank for the whole
drain. Imposed display-observer expiry also exposed a persistent scan-error gate
despite no failed scan. Initial collection now reads committed display-only data
independently, visibly distinguishing it from completed reconciliation. Recovery
priority, callback ownership and action reauthentication are unchanged. Normal
observed recovery completion still requests a fresh scan; display-observer expiry
retains useful data and an explicit limitation instead of declaring recovery
complete or preventing further read-only refreshes.

### 2026-10-28: list stored memories without proposal history

Listing stored memories reused the transaction snapshot primitive: one pinned
helper program per topic, with hashing and a mode probe, then a full YAML parse.
Opening the cockpit also waited for the complete history scan before choosing a
view, and every recovery completion restarted that scan. On a copy of the real
workspace (50 topics, 211 completed history rows; compiled, local), listing took
0.46 s median and the history scan 21 s; over a same-host Podman TRAMP target the
listing took 2.3 s. YAML parsing was 0.18 s of a cold listing. A no-op recovery
(10.7 s) cancelled and restarted the in-flight scan.

Memories now uses an attribute listing to reuse cached metadata and reads only
changed files, batched and asynchronously; mevedel's own frontmatter form is read
without YAML when it provably equals the YAML result. Measured afterwards: 2.8 ms
for an unchanged local listing, 0.13 s cold; 0.033 s and 0.23 s over the TRAMP
target. Prior counts choose the opening view; without them the table opens on
Memories instead of waiting 15 s for the scan to choose. An automatic switch to
Candidates when the scan found proposals was tried and rejected by the user as
confusing; the Candidates tab label shows the count instead. Recovery reports
whether it changed committed state through a
presentation-only generation; only changes invalidate counts and rescan.
Inspection, deletion and decisions still read fresh transaction-grade evidence.

The collector groups retained writes by untrusted pass hints, then freshly reads
and authenticates bounded batches, sharing the source proof within each pass.
The read-only decision scan reuses those authenticated intents instead of
authenticating every write twice. Batch failures keep affected identities visible
as unavailable and leave independent batches inspectable. Paired claim/outcome
reads and batched recovery grouping hints reduce helper invocations without
retaining authorization proofs between operations.

Measurements on the same copied real journal with compiled code:

- Scenario: completed-history collector; metric: total latency; before: 21.03 s;
  after: 11.78 s; statistic: median; samples: 3 per implementation. All 211 rows
  remained present, with 210 applied and one rejected, and no errors.
- Scenario: retained-write listing inside that collector; metric: latency;
  before: 10.98 s; after: 5.23 s; statistic: median; samples: 3 per implementation.
- Scenario: completed-history collector; metric: pinned helper programs per scan;
  before: 4,485; after: 2,365; statistic: mean; samples: 3 per implementation.
- Scenario: synchronous decision publication recovery; metric: latency;
  before: 10.68 s; after: 9.84 s; statistic: single sample per implementation.

These are component timings, not complete cockpit-readiness measurements. The
Podman target shares the client host and does not measure physical SSH latency.

### 2026-10-28: protect notes in use rather than pausing for every turn

The workspace-wide turn gate (introduced with shared-note processing in
`5fe59aee`, made boolean in `2b4eeefa`) stopped capture, drain admission and
automatic note application whenever any conversation in the workspace ran or
settled a turn. Several open sessions are the normal case, so the backlog barely
moved: one 900-second native drain admitted nothing. The gate was also too weak
in the other direction: an old handoff that an idle session had just read was
not protected at all.

A note is now in use while a session whose buffer is live, including its
sub-agents, named that file through a `work://shared/` tool within the quiet
period. The resource layer records the fact; the notes module decides. Capture
leaves in-use notes for later, automatic application holds only those
decisions and rechecks at the mutation boundary, and the retry runs once any
held file is free. Other sessions' turns no longer stop the drain. The user
chose this lease over transcript mentions, accepting that shell access by
ordinary path, accesses before an editor restart and other clients' sessions
are not observed; the quiet period still protects recently changed files.

### 2026-10-28: account for open items before deleting a note

Audited reviews deleted notes whose unresolved findings, unverified leads or
acceptance limits had no later resolution or record, and a held proposal
named two unresolved findings while proposing deletion. Each of the last three
native reviews made no tool call. The prompt already said that unresolved
findings are continuing use, so another sentence would not suffice; "delete
when future usefulness is uncertain" also invited the inverted reading.

A delete disposition now lists `open-items` and, per item, a `resolved-by`
quote of the workspace text establishing its resolution or record. The
investigation records the text each workspace Read delivered; acceptance turns
a delete whose quotes are missing or absent from those reads into keep, with
the reason, as it already did for a cited note. Requiring only that the cited
file had been read was not enough: in a real-model evaluation on the audited
preimages, a reviewer deleted a note whose acceptance limits were unrecorded,
citing a decision record it had read that does not mention them. Truncated
Reads now name the continuation offset; earlier, reviewers read only the first
8 KiB of the backlog and the decision record and never saw the relevant entries.

A model can still omit an open item or quote text that discusses rather than
resolves it; that remains model quality. In seven real reviews per round
(Codex `gpt-6-luna`, medium, one note each, against private copies with and
without the backlog repairs), the last round made no unsupported deletion:
acceptance kept two over-claimed deletes, and the others condensed to dated
open items. The negative control, an old failed review later resolved in
ADR 0126, was also kept rather than deleted, so over-retention remains; one
reply per round still failed the output format and is retried by a later
review. Old completed history and historical failures with quoted later
resolution remain deletable.

### 2026-10-28: reduce note admission and retained-history overhead

A frozen real workspace copy contained 64 retained accepted passes and 247
decisions. Its configured capture bound admitted sixteen notes, but the native
review request admitted only five under its separate 32,000-token initial-input
cap. Reference observations repeated the same topic identity, scope and check
date for every token. Request-only grouping now shares those fields and retains
each token/result observation; the durable review schema, indexes, instructions,
input/output limits and oldest-first admission priorities are unchanged.

Note capture now inventories sources before reading disposition history. It
authenticates decision status only for paths present in that inventory; absent
sources cannot be admitted or suppressed. All retained pass bundles are still
authenticated, and a recreated source reenters the ordinary decision checks.

Decision recovery now reads up to eight metadata records in one pinned program,
then verifies their exact hashes alongside their coordinator claim/outcome reads
in a second program. Only closed, bounded authenticated metadata survives for
grouping, never raw record or proposal bodies. Each full decision still validates
its source bundle and write intent. A matching intent can reuse that read's
authenticated coordinator and source proof. A recovered write may have a newer
decision coordinator than its original intent; that mismatch takes the original
fresh authentication path. Publication invalidates every pending metadata and
bundle observation, and no proof survives the operation or authorizes a later
mutation. Regression tests change bytes between the initial read and hash proof,
and change a later decision during publication; both fail closed.

Measurements used fresh private copies, compiled package code, the same frozen
installed dependencies and concrete Codex OAuth dry-run transport. The host
filesystem was read-only and networking disabled. Baseline was `f7047e9a`;
application retained native ownership, validators and the 24-hour quiet period.

- Scenario native input admission, same sixteen-note candidate prefix; metric
  admitted notes; baseline 5, updated 9; unit notes; one sample per build.
- Scenario same input admission; metric reference projection size; baseline
  56,258, updated 19,567; unit UTF-8 JSON bytes; one sample per build. All 200
  dated observations survive, and MEMORY.md and AGENTS.md remain supplied.
- Scenario retained-history note capture; metric wall time; baseline median
  9.315, updated median 3.305; unit seconds; three samples per build.
- Scenario same capture; metric control programs; baseline 2,056, updated 587;
  unit programs; three samples per build.
- Scenario same capture; metric relevant decision reads; baseline 216,
  updated 7; unit reads; three samples per build. No present-path decision
  is omitted.
- Scenario accepted-decision recovery; metric wall time; baseline median
  10.841, updated median 9.102; unit seconds; three samples per build.
- Scenario same recovery; metric control programs; baseline 1,988,
  updated 1,279; unit programs; three samples per build. All 247 decisions
  remain authenticated.
- Scenario actual checked application of one held note deletion; metric total
  wall time; baseline 21.969, updated 20.189; unit seconds; one sample per build.
  Both copies have the source absent, exact authenticated intent before-state,
  and a retained undo blob matching the original bytes.

The remaining application recovery still dominates that last sample. Its
updated nested components are inclusive and must not be added together:

- Scenario updated checked application; metric editor application-body wall
  time; value 3.069; unit seconds; one sample.
- Scenario same application; metric publication wall time;
  value 1.389; unit seconds; one sample.
- Scenario same application; metric write-marker retirement wall time;
  value 0.031; unit seconds; one sample.

These do not measure provider latency, graphical responsiveness, cost or
sustained-drain throughput. No inference call was made for these measurements.

Phase durations now accompany the existing terminal consolidation telemetry
event, without adding one expensive workspace append per phase. Parent-local
queue dwell, preparation, review-request, publication, application and retirement
intervals remain distinct from child-local preparation recovery and nested
application recovery round trips. Review-request time includes prompt packing,
tools and chunk summaries, so it is not labelled provider time. Missing intervals
remain unknown rather than zero; cancellation closes only started intervals.

The acceptance instrumentation also reuses native provider-call transport
observations for sessionless reviews and their chunk summaries, correlated by
pass identity in workspace diagnostics. Terminal passes retain observed provider
wall-time totals separately from review-request time. The existing shared lag
heartbeat watches admitted memory passes through completion and its ordinary
tail, without creating a session or another sampling timer. This closes timing
and watching gaps, not a claim of sustained throughput, lower CPU, or graphical
input latency; external engines still lack native provider-call boundaries.

### 2026-10-28: accept unambiguous reply shapes instead of discarding reviews

After the open-item contract landed, three of five production reviews were
rejected whole: one reply ended without `## No action`, two were "Note
disposition fields are incomplete". No reply text was retained, so real
multi-note reviews of copied notes reproduced them. The parser required
exactly three fields on keep and condense, but reviewers usefully list open
items there too; they also wrote header-only deletes without `---`, four-dash
separators, and three-backtick closes for four-backtick openers. One
malformed block discarded every other valid disposition.

Keep and condense now accept validated `open-items` and are recorded with
them. A delete still requires `open-items`; a missing `resolved-by` means no
evidence, so acceptance keeps any delete that lists an open item. Empty-body
blocks may omit the separator or close with any backtick-only line, a section
heading or the next opener; separators may have three or more dashes; a
trailing `## No action` may be absent. A non-empty body still ends only at its
exact fence, so code blocks in condensed notes stay intact, and an unterminated
non-empty body, prose between blocks and every field check still reject the
reply. These rules accept stated grammar variants; they never rewrite a reply.
Rejected replies are retained, newest eight, under
`.mevedel/state/diagnostics/rejected-replies/`.

Matched real reviews (Codex `gpt-6-luna`, medium; the same twelve copied real
notes, ten admitted per review; eight reviews each):

- Scenario: committed 081ffce9; metric: rejected replies; value: 3 of 8.
- Scenario: this change; metric: rejected replies; value: 0 of 8; 15 accepted
  deletes, each quote-checked, and no delete downgraded.
- Replaying all ten retained real rejected replies: seven now parse; two with
  prose between blocks and one unterminated condensed body remain rejected.

Eight reviews per condition are a small sample, not a failure-rate estimate.
One candidate review deleted the audit folder's README, whose ownership section
calls itself historical, while the other seven kept it: deletion judgment
remains model quality.

### 2026-10-28: carry recovery continuity across adjacent application claims

The first implementation deliberately recovered the full retained history for
every automatic proposal. Production phase timing then attributed 269.910 seconds
of one 306.220-second application phase to recovery round trips for eleven
decisions, including older holds. Moving those scans to children had reduced
editor blocking, but not that repeated work.

Keeping a single decision claim for several writes would violate its immutable
one-outcome contract. Instead, automatic application retains only the exact
claim/outcome identity of its cleanly completed predecessor, within that job.
Each next operation still acquires its own fixed 180-second claim. A pinned
program must prove exact generation adjacency and authenticate both predecessor
records alongside the new claim's current, unsettled, unexpired ownership. A
new accepted pass, decision or write intent requires consolidation ownership;
an intervening operation, including cleanup, therefore breaks this continuity.
Public replay of already accepted immutable state does not mint a new decision;
expiry replay likewise applies a manifest created under consolidation ownership.

The predecessor is produced only after the operation's publication and marker
retirement, when needed, and successful fencing, not merely a completed
acceptance outcome.
Errors, held/cancelled work, incomplete recovery and failed/expired outcomes
cannot produce it. No proposal bodies, pass bundles, status observations or
target proofs cross the claim boundary. The next operation still freshly reads
its accepted proposal and current status, checks extraction requirements and
original-root ownership, and rechecks snapshots, unsaved buffers, note-access
protection and cancellation at the write boundary. Other workspaces can change a
shared target without advancing this workspace's generation; these target
checks remain mandatory. Deferred retries and separate jobs start recovery anew.

Pending discovery stays after the current accepted proposals. Its read-only
child receives only the predecessor's claim/outcome projection, independently
authenticates continuity, and selects fresh pending identities; the parent owns
the actor/context check. If continuity is absent, normal history recovery and
historical-remote fallback remain unchanged. Missing or altered proof never
authorizes the fast path.

A matched compiled replay used identical private real history, eight newly
constructed native fixture dispositions and two real older held proposals.
It made no provider call, mounted the host read-only and disabled networking.
Baseline was `081ffce9`; application source hashes match the measured candidate.

- Scenario native ten-decision asynchronous job; metric application wall time;
  baseline 272.426, updated 63.266; unit seconds; one sample per build.
- Scenario same job; metric recovery round-trip time included in application;
  baseline 240.930, updated 20.810; unit seconds; one sample per build.
- Scenario same job; metric full recovery worker launches;
  baseline 11, updated 1; unit workers; one sample per build.
- Scenario same job; metric total maintenance worker launches;
  baseline 11, updated 2; unit workers; one sample per build. The additional
  updated worker performs only checked pending selection.
- Scenario same job; metric applied decisions;
  baseline 10, updated 10; unit decisions; one sample per build.
- Scenario post-application audit; metric mutating decisions with source absence,
  exact authenticated before-state and byte-matching retained undo blob;
  baseline 10, updated 10; unit audited decisions; one sample per build.

Fresh selected-decision and accepted-bundle read counts are unchanged; the saving
is repeated all-history recovery, not reduced application authorization. This is
one controlled workload, not a sustained-drain rate or a measurement of provider
latency, editor responsiveness or live end-to-end throughput. Remaining
application, pending discovery, preparation and provider work still contribute.
The recovery round-trip field excludes the updated continuity checks and
read-only selection; application wall time includes both, so the full phase is
the matched measure of their net benefit.

### 2026-10-28: name residual details that reviewers miss

The first production batch after the recovery change condensed two audit notes
too vaguely: concrete residual costs (wrapped-draft Backspace, C-k/yank/
transpose) became "remaining exact-path editing costs", and an explicitly
retained, unexplained rise in exact-repair timings was dropped. Both were
reversed natively. Both residuals were in fact recorded in ADR 0126, so the best
disposition was a delete quoting that record; the reviewer had searched only the
backlog. Replaying those five notes in one review showed the same failure class
at the committed prompt, and more: reviewers deleted the performance note with
no open items, reasoning that work assigned to another owner was not this
note's concern. The quote check cannot catch an item that is never listed.

The prompt now says that work assigned elsewhere and observations retained
without an established cause are open items wherever they appear in a note,
that decision records hold measurements and residual costs, and that a
condensed body keeps each item's concrete operations, values with baselines and
conditions. A deterministic list of marker lines was measured and rejected: it
flagged a mean of 15 lines per real note, 48 of 147 notes over 12, mostly
provenance wording.

Real reviews (Codex `gpt-6-luna`, medium) of the five audited preimages in one
review, scored on the two affected notes; a disposition is bad when it loses
the residual details or deletes without listing open items:

- Scenario: committed b0493f9c; metric: bad dispositions; value: 19 of 28
  (14 reviews); performance-note deletes without open items: 10 of 14.
- Scenario: this change; metric: bad dispositions; value: 12 of 26
  (13 reviews; one provider reply was malformed JSON); deletes without open
  items: 2 of 13; deletes quoting ADR 0126: 5.
- Scenario: all five notes; metric: deletes per reviewed note; value: 46% at
  b0493f9c, 35% after; condensations rose from 24% to 45%.

Condensed bodies still lose concrete residual details in about a third of the
reviews: recall of residual detail remains model quality, bounded by the
condensation's undo window, not a guarantee.

### 2026-10-28: let reviews read Git history

Run14 kept five of seven notes after a review with no tool calls. Handoffs such
as "root owns full-suite validation and fast-forward integration" are usually
finished only in Git history, which the workspace-only tools could not read, so
reviewers could never establish their resolution and kept them indefinitely.
The user approved a bounded read-only Git tool. `log` searches commit messages,
`show` prints a commit's message, changed files and whether the current HEAD
contains it; the containment line names the commit so a quote cannot vouch for
another one. Output is recorded as `GIT-HISTORY` evidence and checked like
other quotes. Real replies also misused the format in unambiguous ways: a
`resolved-by` entry that is not `PATH: QUOTE` now counts as no evidence
instead of rejecting the whole reply, note dispositions may repeat a header
line with an identical value, and sections may appear in any order. Omitted
action sections remain an error: treating them as empty let a reply truncated
after its first section pass when no notes were admitted.

The run14 deletion of a completed final review with no open items was judged
defensible: its typing variance was bounded by a later probe in the same note,
and its limits were scope disclaimers without a next step.

Real reviews (Codex `gpt-6-luna`, medium) of run14's seven preimages in one
review, eight reviews per condition, with full workspace Git history:

- Scenario: committed d85e986d; metric: failed reviews; value: 1 of 8;
  deletes: 30 of 49 dispositions; composer2-progress Git-quoted deletes: 0 of 7.
- Scenario: this change; metric: failed reviews; value: 0 of 8; deletes: 39 of
  56; composer2-progress Git-quoted deletes: 3 of 8, one quoting
  "Commit d46b1d6f... contained in current HEAD: yes".
- Git-quoted tmp-git-leak deletes quote commit 1889fb64, whose message records
  the unproven-producer caveat.

Some Git-quoted deletes cite a commit that records a handoff as remaining with
its owner rather than completed; that is a durable record, but weak evidence.
Distinguishing those remains model quality.

### 2026-10-28: retire resolved items in chunk summaries

Run17 kept a 118 KB browser-review note, reviewed in sequential chunks, for three
open items that later sections of the same note had resolved (fixes 95920e3,
bf8f9b5 and corrected screenshot assertions). The final review sees only the
running summary, and the chunk guidance said to carry unresolved work forward
but never to retire it, so early items arrived stale. The guidance now has a
later chunk mark an earlier item RESOLVED with the note's own words and any
commit or file named, and the review prompt treats such items as not open.

Real reviews of that note (Codex `gpt-6-luna`, medium; four each): committed
d78ca3d4 deleted it once, condensed it twice and kept it once; this change
deleted it four times with no open items. Every finding in the note is marked
fixed with a commit that is an ancestor of HEAD (28 checked), decided, or a
status record; the two handoffs without an inline fix were confirmed separately
(the Goal-clear fix is recorded in the note; mailbox notices in 8e67a05a), so
deletion is correct for this note. Four reviews are a small sample.

### 2026-10-28: summarize long notes as notes, not as journal digests

Captured run20 summaries showed the cause of sparse chunked reviews: chunk
summaries used the journal digest format, whose system prompt makes "nothing
noteworthy" a successful outcome. For a 58 KB findings note the summary after
its first chunk was Done/Learned/Surprised/Unfinished all `- none`; the final
summary held three lessons and nothing open, although the note states that its
installation timeout "remains unresolved". Run20 deleted the note. Chunk
summaries now use the continuation format, which preserves unresolved work,
with guidance that the source is a note: open items with their location,
values and conditions go under Open Questions & Risks (RESOLVED ones shortened
to a line), findings, measurements and lessons under Work & Evidence and
Critical Context. Both formats resolve to the same configured model.

The new, heading-rich condensed bodies exposed a parser bug from the earlier
reply-shape change: a body starting with a Markdown heading was taken for the
next section, emptying the condensation. Headings now close only a header-only
block without a separator. A JSON array value spread over several lines is
joined until it parses. A shorter closing fence remains an error, as the
existing contract requires.

Real chunked reviews of the run20 note (Codex `gpt-6-luna`, medium):

- Scenario: committed 457a1275; metric: reviews listing the unresolved
  installation timeout as open; value: 1 of 4; three deleted it with no open
  items.
- Scenario: this change (12 KiB target); value: 4 of 4 completed reviews (two
  condense, two keep); two of six replies failed to parse (a shorter closing
  fence, a multi-line array, the latter now accepted). An earlier 16 KiB
  target overflowed once in six.

No maintained record of that timeout was found, so keep or condense is the
correct outcome. More open items mean more retention of long notes.
