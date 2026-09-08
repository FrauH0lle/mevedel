# Publish journal results from fenced outcomes

Status: accepted

## Context

Journal inference can outlive its source buffer or the client that started
it. Two clients may share a workspace, and a model callback may arrive after
its attempt expires. Checking a lock or deadline and then writing leaves a
race in which an old callback publishes after another client takes over.
Session publication has target-native filesystem primitives and exclusive
generation election, but its renewable lease also owns session mutation and
control transfer. A journal request should not create a synthetic session or
hold a session lease through inference.

## Decision

Use the existing pinned control filesystem for all journal storage. The
journal belongs to the workspace's `.mevedel/journal/`, independently of
where sessions are stored. Public Markdown entries are immutable; private
work state lives under `state/`. Neither storage nor publication edits
`.gitignore`.

Each bounded work scope elects a unique owner by exclusively creating a
numbered claim. Its fixed deadline uses the target clock. Completion,
failure, cancellation, and expired-claim takeover compete to create one
immutable outcome for that generation. A successor can be admitted only
after that outcome exists. Exclusive creation is the fencing operation;
clock observations and process identities alone are not authority.

A completed outcome retains the exact accepted payload. Callers may publish
only that payload, and must recover an accepted unpublished result before
starting replacement work for it. This separates acceptance from public
publication without losing a result if the client dies between them.
Failures and cancellations cannot replace a successor's outcome. The claim
layer neither advances review coverage nor chooses scheduling policy.

Digest IDs derive from durable capture IDs. The creation time is frozen in
the capture descriptor, so retries name the same public file. Exclusive
publication preserves the first result. A later result for the same metadata
returns the existing record; conflicting metadata or a corrupt existing
record fails closed. Discovery validates metadata, filename identity, and
the digest grammar, rather than treating every Markdown file as a journal
entry. Symlinks and private paths are rejected.

Completed-review records use the same immutable target-native publication seam.
Their identity derives from the pass ID; metadata includes the exact examined
digest IDs, focus, reference-check evidence and dates, and proposal IDs. The
public body is derived from those fields and contains no private replacement
body or before-state. The pass owner must durably store its accepted proposal
batch before publishing this record. General-review coverage is the union of
IDs in published records whose focus is empty. Focused reviews, private prepared
results, and scheduling timestamps cannot consume general coverage. External
review additions and removals participate in the same throttled observation as
digests, even when the newest digest filename is unchanged. Cleanup retains
review and decision records while evidence dependencies or unresolved work
need them, then expires each complete dependency group together.

Later decisions use their own immutable `kind: decision` records. Public fields
are identities, date, status, optional reason, and the hash of required private
state; they never copy original or proposed topic bodies into the journal. The
codec selects a closed schema by record kind, replacing the earlier boolean
review selector as the third kind makes that interface ambiguous.

Rejection accepts an immutable private record hash under current workspace
ownership before publishing its decision. Recovery replays only that accepted
record. It verifies the original proposal bundle and workspace identity; an
external public record without matching acceptance evidence cannot decide a
proposal or release its pins. Tests publish such an unaccepted record and
confirm that status/rejection fail closed and later model evidence omits it.
Rejection checks original root authority without requiring unchanged topic
bytes, because it performs no target edit. Repeated rejection keeps its original
decision and reason. All proposals must be terminal before pass evidence pins
are released, and later publication recovery still works after digest expiry.

The next review receives up to twenty whole rejection records within 8 KiB,
including original proposed content and the user's reason. Original root
authority gates private content disclosure; unavailable or oversized records
are counted as omissions. These are untrusted evidence for model judgment,
not semantic deduplication or authority to repeat a rejected change.

Review scope capture binds each root ID to its original physical directory,
execution target, and local host/user origin. Shared proposals must not resolve
their IDs through the approving client's current memory configuration. Bounded
whole-file snapshots retain literal bytes and hashes, including indexes, while
complete directory observations can establish expected absence for creation.
A truncated observation cannot establish absence. A boundary test showed that
absence of a filename alone was insufficient: a parent could be a symlink to
another root. The inventory therefore retains ordinary directory names and
rejects creation beneath any captured non-directory parent. Target reads also
use the pinned control-filesystem operation. These checks provide before-state
and freshness evidence; write coordination and recoverable application remain
the proposal coordinator's responsibility.

Workspace-source investigation excludes every configured memory root, including
ones whose unavailable index prevented capturing their contents. The captured
exclusion list is separate from the admitted before-state: otherwise a failed
memory read would enlarge source authority. The bounded reference pre-check
uses this same source boundary and records dated path-existence observations.
It leaves unsupported reference kinds unknown and does not certify symbols,
commands, lines, or the correctness of the associated memory.

Consolidation investigation uses request-local gptel tools with an explicit
captured-root argument. It does not create a session or register tools globally.
Memory and journal searches operate on admitted copies; bounded source searches
use the same copy boundary after source authorization. Ordinary Glob/Grep supply
the search behavior, while the investigation owns budgets, copy cleanup, and
late-result suppression. The external-helper boundary now returns cancellation
functions because sessionless owners could not previously stop an individual
search. Cancellation follows the existing process owner's terminal cleanup for
its single child. A failed confined launch settles without an unrestricted
retry, as required by [ADR 0116](0116-return-failed-confined-launches-without-retry.md).

The consolidation request measures gptel's prepared provider payload before
dispatch and again before every tool follow-up. Raw evidence length alone would
miss tool schemas and provider formatting. Admission keeps complete digests and
reports omissions; only a terminal validated response can reach the publication
coordinator. A local HTTP test exercises actual provider parsing, Read execution,
follow-up preparation, final validation, and cumulative token accounting.
gptel's HTTP-completion callback is not overall tool-loop completion, so the
request settles through its FSM terminal states. It disables interactive tool
confirmation for its already scoped read tools: an inherited global confirmation
setting otherwise stalled a sessionless request. Unknown tools fail before
dispatch. Deadline, tool exhaustion, output limits, and buffer closure all
retire the same request generation without publishing coverage.

Pass selection uses a fresh journal observation and existing session discovery,
including sessions whose publications cannot be read. Lease status is read from
the execution target; both this client's owned lease and another client's held
lease exclude evidence even when no local root buffer exists. File-session
checks retain the existing same-host PID/start-time proof and conservatively
treat foreign-host holders as live. An unreadable lock is unavailable, not an
absent owner. The PID record is read once for the decision: rereading between
validation and liveness could turn a changed or damaged record into apparent
absence. Selection reports excluded evidence without consuming its coverage.

Prepared consolidation state retains complete selected digest bodies and source
fingerprints alongside the original scope. Preparation holds journal mutation
ownership, recovers accepted expiry first, verifies the source entries, then
creates pass-specific evidence pins. A crash test leaves an accepted expiry
manifest unapplied: preparation finishes that expiry and refuses to pin the old
selection. This prevents late evidence retention from undoing an earlier deletion
decision. Pins identify the prepared record by hash and release independently.

Accepted proposal bundles retain exact before-state, including indexes and merge
inputs, and bind proposal identities to the pass and original targets. The bundle
is immutable before its hash enters the pass claim's outcome election. Publication
verifies both that accepted hash and its prepared-state hash, then writes the
public review under fresh journal mutation ownership. A restart can replay that
same publication without repeating inference. Public records contain identities
and reference-check metadata; global/client-local memory bodies remain private.
Only the published review consumes general coverage. Publication releases omitted
evidence and no-action pins afterward; unresolved proposals retain admitted
evidence. A later pin-release failure does not undo a completed review.

Private pass records use the package's established durable Lisp representation
with escaped literal bytes and a 4 MiB bound, avoiding a second conversion layer
for captured alists and byte strings. Readers disable evaluation and circular
syntax; accepted content must match its fenced hash before decoding. Original
root authority remains mandatory when displaying captured bodies or applying
proposals, even though storage can recover public review metadata on another
client.

The coordinator acquires one 180-second workspace claim, recovers prior accepted
publications before selection, and fences every inference callback against the
current target generation. Its timer includes preparation time. Selection and
pin preparation both recover accepted expiry under journal mutation ownership;
preparation rejects any selected source changed between those operations. A
real-expiry test lets an owner expire, admits a successor with its own pins,
then delivers the old owner's success: it cannot publish or disturb the
successor. Closing the request buffer follows the same cancellation settlement.
Both the explicit command and completed-root-turn opportunities use this
coordinator. Automatic opportunities check mode and cached timing without target
I/O, coalesce while queued, and defer through the transport idle boundary.
Under current workspace ownership the coordinator recovers earlier accepted
results, checks elapsed target time since the last successful general review,
then counts eligible unreviewed digests. Default thresholds are 24 hours and
five digests; a failed count check defers another scan for ten minutes. This
second check matters when another client finishes between the initial
observation and admission. Focused reviews never change the general clock, and
retired completion metadata retains it after expiry. Explicit commands bypass
the time/count gate. No success callback recursively drains the backlog.

Propose is the default mode. Auto runs the same read-only review and then uses
the existing checked application operation sequentially; instruction proposals
remain pending. The admission mode is frozen through asynchronous settlement.
The old application return value was only the immutable public decision, so
counting its changed files would also count an idempotent return of another
call's work. An optional confirmation callback now reports changed paths only
for a new application by that call. Auto counts distinct paths across the pass,
including a shared index once. Cancellation during application finishes the
current checked operation and holds subsequent proposals. Failed applications
retain the same stale/unavailable/recovery evidence used by explicit approval.

The first configured-model consolidation evaluation exposed a Unicode boundary
error before inference: the JSON encoder returns UTF-8 bytes, and embedding
those bytes as prompt text made gptel's next JSON serialization reject the
request. Review input now decodes that JSON to text before request preparation.
The same boundary applies to rejection evidence, public journal headers and
derived bodies, and JSON-quoted topic titles/descriptions. Unicode regression
cases verify request admission, inspection text, immutable metadata round trips,
and equality between patch text and decoded file bytes. This uses the existing
JSON encoders and UTF-8 conversion; no alternate persisted format is introduced.

Memory transaction preparation now derives complete topic and index changes
from accepted before-state. It generates canonical name/description/type
frontmatter, escapes index labels and destinations, validates destinations
including encoded aliases, and replaces/removes affected links while retaining
unrelated index lines. Instruction changes append only to the captured file.
The preparer writes nothing; the decision owner persists complete before/after
intent before applying through the shared patch engine.

The shared ApplyPatch transaction accepts expected-before snapshots and an
optional current-owner predicate. A before-state mismatch rejects the whole
batch before effects. It rechecks each path before writing and derives exact
expected after-state from the intended bytes, not from a later observation that
might belong to another writer. Rollback restores only matching owned results;
intervening disk or unsaved buffer edits are preserved as incomplete recovery.
An ownership-transfer test settles the old claim and admits a successor during
buffer synchronization: the old transaction cannot undo the successor's files.
This replaces the previous unconditional snapshot restoration, which reproduced
data loss when a synchronization hook wrote an intervening edit before failing.

Application now holds both workspace ownership and a target-root claim. A
workspace-only claim cannot serialize two workspaces using one global memory
root. Exact intent stays private in the originating journal; a hash-only pending
marker at the target blocks successors until a resolved decision is durable.
Claim expiry alone cannot remove this fence.
Final review identified an additional race: a client could pause after its
ownership check, then resume its filesystem write after recovery retired that
marker and a successor applied a change. Client-side predicates cannot fence an
already-admitted write. Curated mutations, rollback and marker retirement now
use the same target-side directory `flock` as claim settlement. Guard checks and
the mutation run in one pinned program while that lock remains held. Expiry uses
the same filesystem clock as admission, and requested permissions are applied to
the temporary before rename, keeping crash states recognizable. Independent
Emacs tests pause a writer inside that program and prove takeover waits; later
old writes/deletes are refused on both local and TRAMP targets.

Review also found abandoned PID-lock recovery could delete a normal resume's
replacement after observing stale state. Creation, replacement, release and
stale sweeping now share Emacs native file locking around the expected-holder
comparison and mutation. This preserves file-session platform support and keeps
background recovery from prompting to break a live mutation lock.

Source investigation formerly passed range requests straight to the ordinary
reader, which could accumulate a whole large single-line file before result
truncation. It now uses the existing pinned 512-KiB snapshot boundary before
ordinary line selection. Oversized or unavailable source stays unknown.
 The reserved coordination directory
is excluded from investigation and recursive memory inventory, avoiding both
control-state disclosure and exhaustion of the bounded inventory by claim history.

Crash reconciliation compares every captured dependency, including an unchanged
index, with exact before/after snapshots. Complete writes are recorded as applied
without repeating them. Partial or foreign state retains the fence. Explicit
rollback uses the patch engine and refuses intervening edits; before-state stays
retained for inspection and later checked reversal. Public decisions carry only
identities, outcome, reason, and the private-state hash. Their accepted claim
generation orders status, avoiding ambiguous retries with identical timestamps.

A sequential-approval test exposed an overly strict index expectation: the first
accepted proposal made the second stale despite both belonging to one pass.
Application now folds only confirmed, hash-verified same-pass index transitions
into the next expected snapshot. It never adopts current target bytes, so an
external index edit still blocks application. Original proposals remain immutable.

Checked reversal uses the same durable-intent and decision protocol. The reverse
intent binds the original write identity/hash and swaps its exact before/after
snapshots; it cannot choose a new target or baseline. Reversal checks every
dependency, including the index, and adds a separate immutable `reversed`
decision. The proposal stays terminal. This preserves the original application
as evidence and makes a crash during reversal distinguishable from an incomplete
initial application. An untouched reversal leaves the original application in
place; a partial reversal remains fenced and can be explicitly rolled back.
Only completed transitions advance shared-index expectations, so an aborted
reversal cannot manufacture a new baseline for remaining proposals.

Recovery discovery distinguishes a retained intent from an admitted write. A
private intent saved before its root marker cannot claim later external bytes
as its own completed application. Discovery therefore reconciles only verified
marked attempts after recovering accepted publications under successor workspace
ownership. Unreadable and unavailable records remain visible, and one unavailable
root does not rebind or hide another root's pending work.

The proposal cockpit follows the shared tabulated surface contract. It owns only
navigation and presentation; accept/reject/reverse/recover delegate to persisted
decision owners. Opening or refreshing the table cannot disturb the originating
composer or take over a live pass. Body/diff inspection checks original root
authority, and the table can still name unavailable records without disclosing
their private contents. Background request inspection/cancellation uses the pass
runner's existing buffer/cancel handles rather than creating a new session.

`/remember [focus]` and `M-x mevedel-remember` now start that same sessionless
runner and open the proposal table. Manual review can inspect current memory
without an eligible digest batch. Completion refreshes only an existing matching
table and preserves the conversation composer. The bundled report-only remember
skill is removed, leaving one command route. Automatic completion refreshes the
same table; auto reports confirmed updated-file counts and held proposals.

A long-session publication test exposed two boundary errors: the original
24 KiB entry limit could not fit complete turn IDs, and the decoder counted
publication separators against an otherwise valid 16 KiB body. Public entries
now have an 8 MiB total bound, matching the scale of private capture storage.
Metadata admission reserves the full digest body allowance; descriptor size is
checked before publication or pinning. The decoder excludes separator whitespace
from the body budget. These checks reject oversized input without dropping
coverage IDs or creating an unreadable pinned capture.

Evidence retention uses immutable, capture-keyed pins under the source
session's private `.journal-pins/` directory. Keeping these references beside
the source lets existing cleanup entry points honor them without locating or
opening a workspace journal. A pin retains the whole session, its referenced
publication heads, and every generation those heads resolve through. A
malformed pin blocks collection rather than silently releasing evidence.
Pin creation requires the source session's mutation authority; publication
or explicit discard releases only that capture's pin. An unsealed checkpoint
may also be superseded by an already pinned, ready checkpoint containing all
of its completed-turn identities. Capture descriptors
and model/control state still belong to the workspace journal's `state/`.

Completed-work checkpoints are created after successful root-turn save and
before generation collection, under the source session's existing authority.
Portable capture holds a bounded lease reservation during the synchronous
snapshot. A single exclusively created JSON record contains its descriptor,
projected evidence, notes, hashes, and serializable model selection. Separate
snapshot files would allow a crash to leave metadata detached from the notes
it described; the atomic bundle removes that interval. The source pin precedes
the ready marker. No model inference happens during a checkpoint.

Coverage uses hashes of persisted fork-point identities, alongside human-readable
turn numbers. Numbers alone repeat after Rewind and cannot identify completed
work. Capture IDs derive from the session and ordered stable turn identities;
published and sealed identities are excluded from later checkpoints. An
immutable seal records the first compaction/session-end trigger without
rewriting the capture. Root compaction retains its selected checkpoint before
changing the transcript and seals it after success. Buffer close and Emacs exit
seal existing completed evidence, never a partially streamed response. Exit
starts no inference. Turning journaling off leaves records and pins intact.

Deriving capture coverage solely from surviving public digests allowed deleted
digests to reappear as old work inside a new checkpoint. Publication now records
capture and turn identities separately in immutable `state/coverage/` records.
It writes the public digest first and coverage second; accepted-result recovery
repairs an interrupted coverage write before releasing the source pin. Sealed
pending captures reserve this interval. These identity-only records survive
public expiry and differ from the general-review coverage consolidation owns.

Digest processing uses a dedicated workspace admission scope, separate from
consolidation ownership, plus an acceptance claim under the capture's own
attempt directory. Both claims share one exact target-clock deadline. Giving
them separate relative timeouts could let the capture remain eligible after
workspace admission expired; the exact deadline prevents that extension.
One lifecycle opportunity starts at most one sealed job. The capture claim's
generation bounds automatic attempts to three; successful completion never
recursively starts another request.

Generation re-resolves the original provider selection and consumes frozen
text within the shared generator's actual input budget. It uses no live session
or model tools. The accepted capture outcome contains the exact validated digest
body. Publication failure leaves that outcome recoverable, and later processing
publishes it before admitting replacement inference. Publication and pin release
precede retirement and removal of the raw evidence bundle. Accepted digest
output remains available for inspection until control-state cleanup.

Lifecycle scheduling coalesces zero-delay opportunities and then uses existing
transport-idle deferral. Exit suppresses scheduling and cancels queued/active
requests before its save-and-seal work; it starts no inference and does not wait
for model output. Deadlines and cancellation preserve evidence pins for retry.

Workspace activation schedules recovery before a processing opportunity. A
temporary source owner repairs unready captures and seals abandoned checkpoints,
using the existing PID liveness checks or portable generation election. The
portable path can fence an expired ordinary lease because it consumes frozen
completed evidence and changes no session head. It refuses live or publishing
leases, unsettled mutation, and release fences for control transfer. It uses the
existing bounded reservation and releases it before any inference; it creates
no conversation or request. Frozen source identity and evidence hashes are
checked before pinning and sealing. Per-capture failures are returned as
diagnostics and do not abort recovery of unrelated captures.

An interrupted supersession can leave a retired capture's pin behind. Recovery
checks the successor's retained turn coverage before releasing that pin and
deleting the old raw bundle. The retirement marker remains: replaying an older
completed-turn set must not recreate a capture whose coverage already moved
to a successor.

Manual retry admits one selected sealed capture under the same ownership and
acceptance rules, even after three automatic attempts. It never resets attempt
history or changes the frozen model policy. The read-only job browser exposes
unreadable and unavailable work so failures remain actionable.

Explicit discard elects a cancelled outcome containing a closed omission record
with the original source identity. It persists that record before releasing the
pin, retiring the capture, and deleting its raw bundle. Persisting only a marker
would leave recovery unable to locate the pin after descriptor corruption; the
accepted omission retains enough origin information to finish without inference.
If the descriptor was already unreadable, the user must supply the original
source directory and its matching valid pin. Foreign client sources are not
rebound to the current client. A completed digest outcome takes precedence over
a later discard request. The omission marker prevents a later save of the same
completed-turn set from recreating its discarded checkpoint.

Journal expiry shares mutation ownership and also holds digest and consolidation admission
while selecting up to 50 digest/review groups. Pending captures and evidence pins exclude
entries from selection. A private immutable expiry manifest records exact public
filenames and content hashes; its mutation claim accepts the manifest's hash as
the durable outcome. Expired unaccepted owners cannot delete anything. Accepted
manifests remain replayable after their original deadlines, and new mutations
must finish accepted expiry before selecting further evidence. Consolidation
must create its evidence pins within this same boundary.

Completed reviews use this expiry transaction after their last surviving digest
and own evidence pin disappear. Counting digests selected in the current batch
as already gone would retire coverage prematurely, so review eligibility uses
the complete pre-deletion observation. The closed manifest records each public
record's kind/identity and exact private file hashes. Pass retirement precedes
deletion, preventing recovery or the proposal table from resurrecting an expired
review when only part of its private state has been removed. Private deletion
checks the originally accepted hashes and retains intervening edits. The manifest
and retirement marker preserve completion date and general/focused scope, so
short retention settings cannot erase the general-review scheduling clock.
Focus text and private topic bodies are not retained in those markers. The manifest
schema changes directly; there is no reader for superseded manifest formats.

Proposal history adds a dependency the original per-file limit did not model:
a reversal needs its original write intent, and later decisions need the same
accepted pass. Expiry therefore selects complete groups rather than splitting
them across a fifty-file boundary. Each proposal must be terminal, every related
decision old and published, and every write resolved with no target marker.
Unavailable original-client targets retain their evidence. The manifest records
all public decisions and the accepted hashes of private pass, decision, and
write records; its closed path grammar cannot name curated memory files. The
4-MiB manifest bound is checked before acceptance or deletion. A test expires
53 public records as one group; another interrupts private deletion after the
pass and decision bodies are gone. Retired decision/write readers skip those
remaining files while the accepted manifest completes deletion.

The large-history test also exposed repeated publication work in decision
recovery. Recovery now compares accepted records with one public observation,
replaying only missing or mismatched publications or unfinished evidence-pin
release. It still authenticates accepted private dependencies; it does not
infer acceptance from public records. Tests remove a review publication and
restore an unreleased pin to verify both recovery paths remain active. Missing
review publication still requires its admitted evidence pins and digest bytes;
recovery refuses to recreate coverage when that evidence is unavailable.

Application first creates a private expiry marker, making the entry unavailable
to public readers and republication, then removes the matching physical bytes.
Unexpected bytes remain for inspection. Completion and deletion are idempotent;
retired capture payloads may be collected, but capture coverage remains. This
avoids leaving expired content readable when a client dies during deletion.
The hourly cleanup opportunity runs independently of session expiry or capture
settings, on local and TRAMP targets, without inference or recursive draining.

Consolidation reports lifecycle events through the existing workspace telemetry
writer. The coordinator emits one terminal event under its settle-once guard,
freezing mode and scope at admission. Cancellation previously settled the
coordinator before the runner could return its received usage; storage rejection
also replaced that result. A read-only usage snapshot and preservation before
publication now retain those costs without weakening cancellation fencing.
Oversized non-streaming replies account for reported usage before validation.
The log contains counts and categorical failure classes, never evidence,
proposal bodies, focus text, backend objects, or arbitrary errors. It remains
diagnostic, independent of accepted outcomes and coverage authority.

Opening the proposals table was initially the only automatic discovery point
for interrupted memory writes. Activation now schedules the same checked
reconciliation independently of the journal capture setting, so a restart can
resolve an admitted write without first opening that table. It coalesces repeated
opportunities, respects transport deferral and live ownership, creates nothing
in a workspace without consolidation state, and cancels queued work on exit.
The main cockpit reads disposable ten-second count observations on entry and
renders them without target I/O. The observation never grants write authority.

## Consequences

Recovery can inspect accepted outcomes without the original request buffer,
provider, or process. Claim records do not contain credentials. The payload
can contain sensitive evidence and therefore remains private control state.
Readers of the public journal cannot reach it through entry discovery.

Claims do not renew: inference must honor its bounded runtime and settle
within the deadline. Expiration fences acceptance; it cannot physically
stop a provider request owned by an unreachable client. Callers must abort
their local requests on timeout and ignore rejected late callbacks.

Claim generations remain on disk until their owning work can be retired.
Cleanup must not remove a live claim, its accepted unpublished payload, or
source pins needed for recovery. Session and generation collectors now honor
source pins, and completed-turn autosave creates those pins. Scheduling and
accepted-result recovery, abandoned checkpoint recovery, and explicit job
controls are connected. Digest expiry and consolidation evidence pinning/release
are connected. Completed consolidation and decision history expires only after
its evidence and recovery dependencies end.

Working-note capture follows the `work://` ownership model: shared files are
captured first in deterministic filename order, then session-owned files, within
one 32-KiB bound. There is no privileged notes filename. Shared provenance says
that content may come from other sessions and does not establish what the
capturing session performed. This replaces the fixed notes.md preference because
the freeform shared-file evaluation recovered corrections without a prescribed
file or folder structure. Captured strings stay immutable after later shared edits.
