# Journal capture, discovery and retention

## Journal and consolidation overview

Journal storage is separate from curated memory. The internal
`mevedel-journal-store` module publishes immutable workspace digests and
completed-review and proposal-decision records, and
`mevedel-journal-claim` provides bounded work ownership and recoverable
outcomes. Completed-turn capture and sealing are connected to session
lifecycle. Capture reads each turn from its committed segment; a turn whose
indexed bounds lie outside those bytes appears in the evidence as unavailable
and is covered with the rest, instead of failing the checkpoint. A session
damaged by an earlier rotation bug otherwise failed every later capture. Background generation and accepted-result recovery run from
lifecycle opportunities; completed, saved root turns, session opening and idle
maintenance recover abandoned checkpoints.
Public entries live in `.mevedel/journal/`; private bookkeeping and recovery
evidence live in `.mevedel/state/journal/`.
Public-entry discovery batches fresh expiry-marker checks and bounded reads in
groups of 16. Completed-turn coverage uses the same bounded read batching;
malformed coverage fails closed. These observations are not cached across calls.
`memory://journal/` supports ordinary Read, Glob, and Grep over validated published
records, with cached composer completion and request-time roster availability.
The main conversation receives a bounded recent-digest map as retained context.
Consolidation runs on demand through `/remember` or automatically in `propose` and `auto` modes;
the memory cockpit supports inspection, decisions, and recovery. See
[ADR 0117](adr/0117-publish-journal-results-from-fenced-outcomes.md) for the
storage contract and the [memory cockpit](memory-review.md#memory-cockpit) for the user surface.

## Journal discovery and context

The `journal` component is selected only by `main` and delivered as an independent
retained context update, leaving the system prefix stable. It shows at most
five recent digests with date, session name, canonical address, and the first
nonempty Learned bullet (falling back to Done). The entire component is bounded
to 2 KiB of UTF-8, including labels and addresses; long names and excerpts get
visible omission markers. These are dated evidence, not current instructions
or authority to resume unfinished work. Worker, explorer, verifier, reviewer,
guardian, buddy, and context-summary prompts omit the map. Review coverage is
the exact union of digest IDs in published general-review records. Focused
reviews retain their examined IDs without advancing general coverage. The
cached observation includes reviews, so additions or removals refresh the count
even when the newest digest filename stays unchanged. Changed maps or coverage
produce a new complete journal section; unchanged state is not repeated. Empty
state clears an earlier map, and outgoing-history loss causes redelivery through
the ordinary context acknowledgement mechanism. The resource roster advertises
journal retrieval only when validated public entries are available.

Discovery refreshes at most once per ten seconds. On local physical paths, it
reuses the last validated entries when directory and file attributes for public
records and expiry markers are unchanged. Access times are excluded; change
times, modification times, identity, size and permissions remain in the source
signature. A read whose before/after signatures differ is not reusable across
refreshes. Remote or unavailable observations use the ordinary validated read
path. Cached-only completion performs no filesystem operations, and every use
still applies the age limit. This snapshot is advisory: evidence reads,
consolidation admission and publication do not use it as storage authority.

A completed review records its pass ID, exact examined digest IDs, focus,
reference-check evidence and dates, and generated proposal IDs. Its public body
is derived from that metadata; it contains no proposal replacement bodies or
captured private topic content. Publication is the coverage commit point, so the
pass owner must persist its proposal batch before publishing the review. Review
records remain retained while any examined digest survives. Once their evidence
dependencies end, old reviews can expire with their private prepared/accepted
bodies and complete terminal decision/write history.

## Working notes and completed-turn capture

Shared files are not journal input: consolidation reviews them directly. A
checkpoint captures only the session's own `work://` notes and plan titles.

With `mevedel-journal-enabled` (default `t`), a successful completed root-turn
save freezes a checkpoint before generation collection. One immutable private
record contains projected evidence, notes, their hashes, stable completed-turn
identities, and serializable journal model selection. It pins the source
session and any committed publication head. A newer checkpoint supersedes an
unsealed predecessor only when it contains all that predecessor's turns.
Published and sealed turns stay outside later checkpoints; replacement
branches use distinct persisted fork-point identities even when turn numbers
repeat. A turn that spanned an auto-compaction is captured from the segment
where it finished, whose inherited summary stands for the earlier part
([Sessions](sessions.md)). Storage failure reports a warning without failing the conversation save.

For deferred root-turn settlement on local Linux portable sessions, a batch
Emacs child prepares that checkpoint from the committed publication and frozen
turn index, model policy and client identity. It receives no source mutation
lease or provider object. The editor retains request admission while allowing
input, then checks the same live root, request, saved head and source authority
before publishing and pinning. A newer publication of the same session, such as
a debounced agent-state save or an execution completion record, supersedes the
prepared evidence rather than failing it: preparation restarts once against the
new head, and a second supersession silently leaves the turns to the next
checkpoint. A replaced request or root, or a pending publication, still fails
with a warning. Changed or cancelled sources cannot publish late
results; preparation has a 120-second timeout. The child's request and reply
files use an explicit lossless coding: detection read evidence holding a NUL
byte, such as binary tool output, as raw bytes and broke every non-ASCII string.
Terminal hooks and queued requests
wait for settlement, and collection is offered afterward. Explicit checkpointing,
compaction/close, PID-lock sessions and remote roots use the same preparation
and publication operations synchronously.

Publication retains capture and completed-turn IDs in private immutable coverage
records. These contain no transcript or digest text and survive deletion or
expiry of public digests. A later checkpoint therefore excludes previously
published work even when its public digest is gone. This capture coverage is
separate from consolidation review coverage.

Public entries and private capture bundles have separate 8 MiB storage bounds;
digest bodies remain capped at 16 KiB. Metadata admission reserves the entire
body allowance, so long completed-turn ID lists are never silently truncated.
An oversized capture is rejected before publication or pinning and stays
eligible for a later successful capture.
Capture decoding also checks the complete source-reference schema, unique
segment names, their revision hash, and all frozen policy fields. Malformed
descriptors remain inspectable with their source pins intact. A saved reasoning
effort that the resolved model no longer supports stays unavailable before any
provider request.

Successful root compaction and `/clear` seal the completed-work checkpoint
selected before the transcript was replaced, with `compaction` and `clear`
triggers respectively. Clear retains the pre-clear evidence and captured title;
it does not capture unfinished output or create an empty digest. Pre-commit
failures or cancellations leave checkpoints unsealed. A committed clear still
seals its checkpoint if subsequent presentation or hook work fails. Agent
compactions create no journal work. Closing the root data buffer or exiting Emacs seals completed
checkpoints as session-end work. View-pair close seals before removing the
root registration. Read-only buffers do not seal, and a portable session must
still own its lease; closing does not reacquire authority. Neither path
snapshots incomplete streaming output. Exit starts no model request.

A session that stays open is sealed after it goes quiet. Each completed, saved
root turn restarts a per-session period of `mevedel-journal-seal-idle-minutes`
(default 20; nil disables). When it ends with no newer completed root turn and
no running request, the ready checkpoints are sealed with the `idle` trigger
once the workspace transport is idle, and a processing opportunity is queued. A
request still running at that moment restarts the period instead. Later turns
form a new checkpoint, so a long-running session produces digests without
compaction, clear, or close. The same ownership rules apply: the buffer must
still be the session's root, read-only buffers do not seal, and a portable
session must still own its lease. Closing the buffer or exiting cancels the
period.

The closed trigger vocabulary is `compaction`, `clear`, `session-end`, and
`idle`; capture descriptors, seal records, and public digest metadata reject
anything else. Sealing is immutable and preserves the first trigger. Disabling
journaling stops new capture and sealing while keeping existing snapshots and
pins.

### Digest generation

Completed turns, successful root compaction, successful `/clear` with captured
work, idle sealing, session close, and session opening schedule one background
processing opportunity. Opening or resuming a root session also queues
abandoned-capture recovery and a consolidation offer, so work sealed at exit is
processed without waiting for a turn; it only queues work. While any root
session is open, a maintenance timer runs every
`mevedel-journal-idle-maintenance-minutes` (default 10; nil disables) once Emacs
has had no input for 30 seconds. For each local Linux workspace with a live root
session it queues a processing opportunity with recovery, a consolidation
offer, and the hourly-throttled journal and artifact cleanup, so the backlog
drains one digest per opportunity while Emacs is idle. Remote workspaces rely
on turn, lifecycle, and session-opening opportunities. The timer stops when no
root session remains and at exit. Scheduling waits until the caller returns
and the target transport is idle. Already-due journal and memory opportunities
leave an event-loop interval between jobs rather than executing back-to-back.
On local Linux workspaces, scheduled abandoned-capture recovery, digest discovery
and claim admission, and journal retention run in short-lived batch Emacs children using the same pinned storage
operations and fenced claims. Children load package dependencies without user
init, receive frozen storage inputs and retention settings, and return bounded
completion records. Digest preparation returns a capture identity and fenced
claims; the editor freshly reads the selected capture and rechecks both claims
before starting inference. Provider requests keep the editor's configured gptel backend;
no backend or authentication objects are serialized to maintenance children. Recovery completes before its
processing opportunity resumes. Remote workspaces retain transport-aware editor
scheduling. Live-buffer artifact retention remains in the editor, where active
buffers and gptel context can protect references. Interrupted workers leave durable
claims and evidence for later recovery; exit stops this client's children.

Capture discovery observes marker directories in batches and omits fully retired
records whose evidence descriptor has already been removed. Retired records with
remaining descriptors stay visible to recovery. Claim ownership verifies the
latest generation on the target without transferring the full claim listing;
pruning stops when its deletion budget is exhausted.

Duplicate events coalesce, and completion
does not recursively drain the backlog. Only sealed captures run. A dedicated
workspace admission claim permits one digest request at a time; the capture's
own claim accepts its result. Both share a 120-second target-clock deadline.
Failed jobs get at most three automatic attempts on separate opportunities.
When a capture exhausts them, the next opportunity shows one warning per capture
and Emacs session naming it and pointing to `M-x mevedel-journal-jobs`, where it
can be retried or discarded.

Generation records dated evidence supporting qualifying user, feedback, project,
or reference knowledge under the [save policy](memory-curated.md#save-policy), rather than conversation
progress. A correction or discovery may qualify; completing a task, passing
tests, encountering a blocker, or making a decision does not qualify by itself.
The digest's Unfinished section preserves uncertainty about qualifying knowledge,
not pending work. A validated result with `- none` in all four sections succeeds
without publishing a journal entry or scheduling consolidation. Its accepted
outcome records turn coverage before releasing source pins, so recovery needs no
replacement inference and later boundaries do not recapture those turns.
Consolidation independently judges whether journal evidence supports a useful
memory, including when older notes contain project progress. It treats No action
as success, avoids duplicates, and proposes corrections or removal of outdated
memories when supported. Existing notes and memories are not rewritten merely
because the selection policy changed.

Processing re-resolves the exact frozen provider/model and uses the saved
streaming, effort, and output settings. Missing policy or client-owned evidence
stays unavailable instead of selecting a fallback. Frozen evidence is bounded
again to the generator's actual input budget, with explicit omissions. No live
conversation buffer or tools are needed. Cancellation and timeout preserve
pins; late callbacks cannot replace an accepted outcome.
If one accepted result cannot finish recovery, a background opportunity retains
it and can process another sealed capture. Explicit retry reports the recovery
error. The job browser identifies a capture that requires its original client.

New captures use the configured `journal` workload (default tier `balanced`)
and its effort selection. Unspecified effort retains the provider default;
journaling does not force reasoning off or translate a universal effort level
across models. Existing frozen policies keep their saved effort and limit.
Digest prompts request a few decisive facts and skip routine successful tool
chatter; continuation and handoff policy is unchanged.

Journaling adds no server token limit. Explicit output limits from the resolved
policy or originating buffer's `gptel-max-tokens` are frozen and honored where
the provider supports them, without an automatic 4,000-token ceiling. Omitting
a limit uses provider defaults, not unlimited generation. Input admission uses
the ordinary model context reserve (`mevedel-model-reserve-tokens`) and any
configured output allowance. Accepted digest text remains limited to 16 KiB,
with a 120-second client deadline. Streaming overflow cancels the request;
oversized final output is rejected. These client guards do not bound internal
reasoning tokens or guarantee a server billing ceiling. Codex OAuth has no
server output-token control. Changing journal policy affects new captures;
queued captures keep their frozen choice and published digests remain readable.

Successful output is accepted durably before publication. If publication
fails, the next opportunity recovers that exact output without another model
request. After successful publication, the processor releases the source pin,
retires the job, and removes its frozen transcript/notes bundle. Exit cancels
queued and active processing before sealing remaining checkpoints, starts no
inference, and does not wait for a model.

### Abandoned checkpoint recovery

Completed, durably saved root turns, session opening, and idle maintenance
schedule recovery before processing. Recovery inspects inactive captures as well as ready jobs,
repairs interrupted pin/ready publication, and seals abandoned completed work.
It verifies the original source session and frozen evidence under temporary
source authority. PID locks use the existing dead-holder check and a checked
replacement serialized with normal resume and release by Emacs native file
locking. A resume replacing the observed stale record prevents recovery from
acquiring it. Live, foreign,
or unreadable locks remain untouched. Portable recovery fences an expired
ordinary lease through the existing generation election. Live leases, publishing
leases, unresolved mutation, and reserved control transfers remain held for
normal session recovery. No conversation is resumed and no model request runs
while source authority is held. One unavailable capture does not prevent the
recovery scan from examining unrelated captures.

Recovery also finishes superseded pin release after checking the successor's
retained completed-turn coverage. It then removes the old raw bundle. Retirement
markers prevent an old completed-turn set from recreating its retired capture.

## Journal jobs and inspection

`M-x mevedel-journal-jobs` lists pending checkpoints and jobs, including failed,
exhausted, unavailable, and unreadable captures. `RET` inspects the frozen
evidence, `r` retries the selected sealed job once, `d` discards it, and `g`
refreshes the list. The corresponding commands are `mevedel-journal-inspect`,
`mevedel-journal-retry`, and `mevedel-journal-discard`. They work from a session
or workspace buffer as well as the job list. Pending jobs and overdue retained
evidence have separate headings. Capture inspection uses a section navigator for
diagnostics and exact frozen evidence. Inspection starts no inference.
Manual retry retains the frozen model policy and attempt history, and respects
active ownership and already accepted results.
Unready checkpoints also appear as recovery work, including damaged descriptors;
retired captures remain outside the pending list. Inspection does not acquire
source authority or repair storage.

Explicit discard remains available when journaling is disabled. It cancels a
matching request owned by this client, records an immutable omission before
releasing the source pin, and removes the raw evidence bundle. It cannot take
over another client's live request. An interrupted discard resumes from its
accepted outcome. An already accepted digest is recovered instead of discarded.
When a capture descriptor is unreadable, the interactive command asks for its
original source directory and verifies the matching pin before proceeding.
Discarding a checkpoint prevents that exact capture from being repinned; new
completed work can still produce a new capture.

The journal jobs inspector also shows overdue public entries that remain in
storage: total count, age in days, review mode, and observed retention reasons.
Unreviewed evidence, unfinished capture publication, evidence pins and retained
review/decision history appear separately from pending digest jobs. Entry buttons
read current validated storage for human inspection. Review and proposal/recovery
buttons use the existing `/remember` and proposal cockpit commands; capture retry
and discard keep their existing authority checks. No inspection grants ordinary
model recall or unconditional deletion.

## Journal recall and retention

`mevedel-journal-max-age-days` defaults to 14; nil disables ordinary recall age
filtering. It does not disable storage cleanup.
The clock starts at the immutable public entry `created` timestamp in UTC.
For a digest, this is the execution target time when its completed-turn capture
was first frozen, carried unchanged into later digest publication. Reviews and
decisions use their native record-creation time. Delayed digest generation can
therefore publish evidence whose ordinary recall period has already ended;
unprocessed evidence remains available for consolidation and human inspection.
Reading, retrying, reviewing and changing the address namespace do not reset it.
At the limit, ordinary Read/Glob/Grep, exact addresses, discovery and completion
exclude an entry even if physical storage must retain it. Recall checks use the
client's current UTC clock; native cleanup and automatic review use the execution
target's clock. Machines should have synchronized clocks for matching boundaries.
Cached discovery checks age on every use, and prepared reads check at execution.

Physical deletion follows review completion, independently of recall age. Unreviewed digests remain available
to consolidation and human inspection after their ordinary recall expires. A
successful general review with no proposed changes counts as processing; failed,
cancelled and focused reviews do not consume general coverage. Pending proposals
and interrupted operations can retain evidence indefinitely until resolved. A
14-day recall limit is therefore not a physical-erasure guarantee. Once a
published general review has terminal decisions for every candidate, its public
source notes retire at the next idle cleanup opportunity after other references
end, including when they are younger than 14 days. One decision does not discard
notes still needed by another candidate or review. Exact evidence remains in the
private review history for inspection and undo.

`mevedel-memory-history-max-age-days` is an independent nonnegative setting,
defaulting to 14 days after the latest terminal decision, or completion of a
review with no proposals. Reversal restarts that window. Expired history is
removed as a complete public/private dependency group. Curated memories remain.
Expiry runs independently of session expiry and new capture, for local and TRAMP
workspaces, after completed, saved root turns and existing cleanup opportunities.
On local Linux workspaces with an open root session, the idle maintenance timer
also offers cleanup. These opportunities are throttled to once an hour. Review/decision publication
also schedules idle cleanup. Each batch selects at most 50 content groups and
prunes at most 200 obsolete claim pairs. Progress queues another idle batch only
when a content-group or claim-pair limit was reached. A shorter batch stops the
drain, so collecting claims from earlier cleanup does not keep creating work.
Reviews still within their history window skip private dependency scans; expiry
candidates retain the complete acceptance and dependency checks.
Scheduled cleanup returns to Emacs between ownership
phases and coordination records. It rechecks acquired claims before resuming;
expired or fenced ownership stops the batch and settles its remaining claims.
Claim directories containing only the newest or referenced generations need only
a fresh listing; they do not trigger record reads, clock probes, or resumptions.
A possible deletion still requires fresh claim and target-clock observations and
locked byte verification.
Live-buffer artifact retention checks and their deletions remain one phase.
Explicit synchronous cleanup drains the same steps. Redraw does not run cleanup.
Selection currently scans validated
public entries. Pending captures and pinned review evidence remain retained.

Expiry accepts an immutable manifest through journal mutation ownership before
removing anything. Later mutations recover accepted expiry before selecting new
evidence. An expiry marker hides the public entry immediately, even if physical
deletion is interrupted; recovery finishes deletion without inference. Changed
public or private bytes are retained for inspection. Retired capture payloads can be removed
with their expired digest, while identity-only capture coverage survives.
Consolidation uses the same mutation boundary when pinning proposal evidence.
Cleanup also acquires consolidation admission. Its scheduler additionally defers
new cleanup while this client has a registered memory pass, explicit drain or
owned asynchronous decision/recovery operation, including the gaps between
sequential application claims. Forced requests remain coalesced and run after
that work; force bypasses the hourly throttle, not this scheduling gate. Actual
cleanup admission is distinguished from queued timers and transport callbacks.
An already-active local cleanup postpones new automatic consolidation and drain
admission without waiting in the editor; explicit review reports the busy state
before acquiring a pass claim. Queued cleanup does not block admission. These
observations grant no storage authority; native claims still fence every owner.
When an interactive memory decision encounters this client's active cleanup,
it waits for that batch to release ownership, with quitting available and a
120-second bound. Queued idle cleanup does not delay decisions. Bulk decisions
defer newly scheduled cleanup until the command finishes. Other owners still
prevent conflicting decisions.
Completed general reviews become eligible only after all their examined digests are gone,
their own evidence pins are released, and their accepted private state matches
the public record. A digest and its covering review expire on separate
opportunities; selecting a digest for deletion does not prematurely remove its
coverage. A completed focused review can expire while its unprocessed public
source notes remain. Retirement is recorded before public or private deletion. Pass recovery
and the proposal table skip retired passes, including interrupted deletions.
Retirement retains the completion date and general/focused scope, without the
focus text, so expiry does not erase the scheduling history of general reviews.
Every proposal must have a terminal applied, rejected, or reversed decision, and
the group must satisfy the resolution-based history deadline. Unpublished decisions, unresolved
write intents, outstanding target markers, and unavailable original targets
retain the group. The manifest includes the complete related public decision
history and exact private pass, decision, and write filenames with accepted
hashes; it cannot name arbitrary workspace files. Its encoded size is capped at
4 MiB before acceptance or deletion. A group may contain more than fifty public
records; splitting it would break retained reversal dependencies. Current curated
memory files are never deleted by journal expiry. Readers skip retired history
even when an interrupted deletion leaves only a private write intent behind.
Prompt and cockpit observations are invalidated after cleanup.

Private `.mevedel/state/journal/` directories serve separate purposes:

| Directory | Purpose and retention |
| --- | --- |
| `captures` | Frozen inputs and per-capture attempts; payload retires with its processed digest. |
| `coverage` | Minimal turn identities preventing duplicate capture; retained. |
| `mutation`, `digest-run`, `consolidation` | Numbered ownership claims and outcomes; obsolete settled pairs are pruned after their deadlines. |
| `passes`, `decisions`, `writes` | Exact review evidence, decisions and transaction snapshots; removed with expired history after recovery dependencies end. Failed/abandoned preparations retain a diagnostic window of the configured history duration after their deadline. |
| `evidence-pins` | References preventing early evidence deletion; released when resolved, empty directories removed. |
| `expiry` | Accepted cleanup manifests and completion receipts; collected after completion and claim expiry. Unaccepted abandoned manifests are collected after fencing and expiry. |
| `expired`, `retired-passes` | Minimal retirement identities preventing late recovery from republishing deleted records; retained. |

Claim pruning always preserves the newest generation, unexpired or unsettled
attempts, and any claim still referenced by recovery/history. It covers capture
attempts and configured shared memory-root claims too; outstanding target write
markers block pruning there. Acquisition, settlement, and pruning use the same
target lock and check the observed generation inside it. Delayed callers cannot
recreate a removed generation. Thus bulky coordination history is bounded by
active dependencies and batch progress, while minimal identity records remain.
Unreadable proof records stop cleanup rather than being treated as garbage.

## Working-note snapshot limits

The internal note snapshotter caps the entire working-note input at 32 KiB,
including provenance and omission labels. It visits the session's own
`work://` files in filename order without a preferred filename; shared files
are excluded. It labels notes as untrusted prior context, lists binary or non-UTF-8 files by
name, and includes only names and first lines for `local/plans/`. Reads are
bounded on the execution target before transfer. Snapshot strings remain
unchanged when the source files later change.
