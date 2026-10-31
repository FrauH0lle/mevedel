# Memory decisions, application and recovery

## Proposal decisions

The decision module owns each proposal decision as one transaction. Callers use
`mevedel-memory-decision-apply`, `-reject`, `-reverse`, or `-recover-write`;
they do not acquire claims, prepare write intents, or interpret child replies.
The write module is private implementation, apart from its control-directory
and intent-filename helpers used by retention.

Each verb returns a plist with `:status`, optional `:reason`, detached
`:decision` metadata when a decision exists, and an opaque `:continuation`.
Statuses are `applied`, `rejected`, `reversed`, `stale`, `unavailable`,
`recovery-required`, `waiting`, `held`, `skipped`, and `idle-wait`. A hold or
unmet extraction dependency returns `held` or `waiting`, not an accepted
decision. The cockpit presents those refusals to the user. Automatic application
returns `skipped` for any already-decided proposal.

One explicit serial request may pass the previous result's `:continuation` to
the next verb. The decision owner freshly proves consecutive claim generations
for the same actor and origin before omitting repeated workspace recovery.
Omitting the continuation starts with full recovery. Holds (early or late),
waiting, idle deferral, failure, interruption, changed origin, and claim-chain
gaps discard continuity. Clean terminal outcomes and skipped work can retain it.
The continuation is ephemeral, never an authority cache, and must not be reused
across separate commands. Each continuously owned phase authenticates its
accepted bundle once; acceptance settlement, a new claim, or a child handoff
ends that phase. Forward-write checks remain fresh.

For automatic application, construct an opaque origin binding with
`mevedel-memory-decision-automatic-context`. It can be reused within the request;
each job has separate bookkeeping, while `mevedel-memory-decision-cancel`
cancels the shared context and its active work. `automatic-current-p` checks the
original actor's live policy and workspace identity rather than the timer's
current buffer. The context is not frozen permission.

Without a callback, application is synchronous and signals errors and quit.
`mevedel-memory-decision-interrupted` carries any already-accounted decision.
With `:callback`, application delivers one result, including admission failure,
as `:error` or `:cancelled` when appropriate; confirmed effects and updated-file
accounting survive cancellation and publication failure. Exceptions from the
caller callback propagate rather than causing a second delivery. A callback
alone grants no automatic authority. Automatic context plus callback selects
child-assisted placement on supported local Linux workspaces; unsupported and
remote targets retain checked synchronous execution. `pending` selects undecided
automatic identities through the same child boundary when given both arguments;
otherwise it is a read-only selection without acquiring a claim.

`mevedel-memory-decision-inspect` supplies detached provisional proposal/write
snapshots for the cockpit, copying mutable strings and lists. Inspection grants
no authority: actions freshly authenticate state. `retention` reports whether
decision/write evidence still prevents history expiry.

`mevedel-memory-decision-reject` records a rejection without editing memory or
changing the accepted review. Publication can repair its derived public text.
Rejection acquires workspace ownership and checks the
proposal's original target authority. A repeated rejection returns the same
terminal decision, including its original reason. The reason is optional and
limited to 4 KiB. Private `.mevedel/state/journal/decisions/<decision-id>.el` records retain the
claim, exact public metadata, and optional write-intent identity. Their hash is accepted before publication,
so a later client can recover a rejection after the owner stops.

Public `kind: decision` records contain decision, pass, proposal, and workspace
identities, date, status, reason, and a private-state hash. Their body is derived
from metadata and includes no private topic or proposed replacement. Decision
status is verified against immutable acceptance evidence; public text alone
cannot decide a pending proposal or release its evidence. When every proposal
in a pass has an authenticated terminal decision, publication releases that
pass's digest pins. Under one journal-mutation claim, publication reauthenticates
the decision and bundle, checks the exact pass-review bytes, and repairs an
absent or changed projection. Repair of existing bytes checks the observed hash,
unsettled claim and deadline under the mutation-claim lock; a changed observation
or fenced owner refuses repair. Review
and decision records remain retained while evidence or unresolved work needs
them, then expire together under the resolution-based history window.

Decision recovery groups small metadata records by pass, then freshly
authenticates each decision and its retained write evidence. It shares at most
one authenticated proposal bundle with the write reader within that scan and
discards the proof after publication. Pending automatic selection shares one
public journal observation, refreshing it and discarding pass proofs after
each application. Every application still acquires fresh workspace and target
ownership and performs freshness checks; only a freshly proved serial
continuation can omit the workspace recovery prelude.

For local automatic application, the child selects both the proposal's previous
decision and its required extraction decisions under the operation's workspace
claim. The editor checks the exact dependency roster against the accepted
proposal and freshly authenticates each selected decision before deleting a
source note. Dependency checks do not rescan public history in the editor;
manual and unsupported-target application retain native discovery.

Before a later request, the coordinator recovers accepted decisions and includes
bounded recent rejection evidence: the original suggestion and reason, under
the original memory-root authority. It admits at most twenty whole records
within 8 KiB and reports omitted or unavailable records. This can help the model
recognize reworded suggestions; it is untrusted evidence and does not guarantee
semantic suppression. Rejection, application, and reversal APIs are implemented
through the proposal cockpit as well as their internal APIs.

### Checked application

`mevedel-memory-apply-changes` prepares complete topic/index or instruction
changes from accepted before-state without writing them. Topic frontmatter is
generated from the validated name, description, and type. Index updates escape
labels and filenames, retain unrelated lines, and reject duplicate or invalid
destinations, including two encoded names for the same file. Instructions
replace their exact captured applicable file with the complete decoded body,
including zero bytes for an empty body. Existing and captured known-absent
targets use the same checked write path; this action never deletes a path.
Every emitted change retains expected original
bytes or absence and exact proposed bytes for the shared patch transaction.
Generated files must fit the memory reader's 32-KiB per-file bound, including
index growth, so a change cannot exclude its root from the next review.

The patch transaction checks supplied before-state before any write and
again before writing each path. It restores only its own expected after-state;
intervening disk and unsaved buffer edits survive a failure. A caller can fence
all writes and rollback through a target mutation callback in addition to
client-side ownership checks. Interrupted buffer
synchronization rolls back before propagating a quit.

`mevedel-memory-decision-apply` acquires the workspace claim and an independent
claim at the original target root. Topic and instruction files must still match
their captured state. For `MEMORY.md`, only entries for the proposal's topic and
merge sources must match, including entry absence. Unrelated entries, prose, and
entry ordering may change. The shared index parser rejects duplicate destinations,
invalid paths, and unsupported link syntax. A missing index has no entries.
After these checks, application uses the complete current index snapshot and
preserves its unrelated content and permissions. Exact file checks still protect
the interval between preparation and writing, including unchanged dependencies.
Full before/after states live in private
`.mevedel/state/journal/writes/<intent-id>.el` records. Standard `.mevedel/memory/`
roots keep a hash-only marker in the sibling `state/memory-write/pending/` before
curated writes. Other memory and instruction roots use
`ROOT/.mevedel/state/memory-write/root/pending/`. Coordination belongs to the
original target, independently of the calling workspace. An unresolved
marker blocks other workspaces at that root even after the original claim
expires. Coordination files are excluded from consolidation memory and source
scopes; inventory does not descend into their directory.

For local Linux automatic application, a sequential job runs publication and
store recovery in a maintenance child under one parent-owned, fixed-deadline
180-second workspace claim per operation. Pending automatic proposal discovery
and discovery of a selected proposal's previous public decision also run in
that child. It freshly authenticates decision status to omit every
already-decided proposal, rather than starting another recovery child just to
skip it; undecided extractions and their waiting dependent disposals remain
eligible. Selection returns identities only, never reusable acceptance proofs.
The editor checks the worker reply's operation identity, current job,
originating session/workspace binding, unchanged root and freshly read workspace
identity, cancellation, mode and fresh ownership on return, then freshly reads
the proposal and authenticates the selected decision against private accepted
evidence. A missing decision observation is an error, not an empty roster.
Reentrant and late duplicate replies cannot reenter
an executing or finished operation. Automatic admission reads the live origin
buffer's mode, not whichever buffer happens to run a timer or continuation.
Only the editor acquires original-target ownership, prepares the intent, checks
unsaved visiting buffers, writes files and synchronizes buffers. No target claim
crosses the child boundary. The editor durably accepts each decision and retires
a resolved write's marker while it still owns the target. A targeted child then
reauthenticates that exact decision identity and hash, publishes it and checks
pass settlement/pin release, without repeating full recovery or target mutation.
The operation remains registered until this reply; neither acceptance alone nor
an interrupted publication produces a clean recovery predecessor. Failure or
cancellation preserves the accepted decision for recovery and any confirmed
newly written-file accounting. Supported local automatic application waits for
one second of editor idleness and checks pending input before each proposal and
again after child recovery. Activity defers the proposal identity without
counting it as held or unapplied: the completed recovery operation is fenced and
released before the idle wait, and a fresh operation reacquires ownership later.
No claim deadline is extended or authority retained while waiting. Undecided
proposals remain durable for normal pending selection after cancellation or exit.
The editor's write transaction remains synchronous and can still block input;
the yielding proposal batch does not make that transaction nonblocking. Citation body
rereads use fresh per-root batches of at most eight, with unchanged byte bounds,
capture/reread/inventory ordering and final live-editor checks. The pass stays
registered until application finishes and its final callback runs once.
Explicit user application remains immediate; manual and unsupported remote application retain
the synchronous API. Deferred automatic retries retain their original buffer,
session, workspace and identity context, rather than adopting the timer's current
buffer. They freshly check that context and its live auto policy before admission
and at forward mutation boundaries. Invalid context or explicit cancellation
leaves decisions pending without rearming a retry from another actor. Explicit
cancel also removes deferred work when no pass is running. Unsupported workers
use the same guarded executor with native editor recovery, without a child.
Before target-sensitive recovery, the worker authenticates
retained scope metadata: historical remote roots remain authoritative even if
current configuration is local. Such history, or uncertain metadata, selects
checked native editor recovery under the same continuously held claim instead.
This preflight passes no accepted proofs back to the editor and does not bypass
accepted-expiry or marked-write recovery. It adds a history scan per operation;
offloading editor work does not imply lower total CPU or IO cost.
This split removes eligible recovery and selected-status/publication history
scans from the editor, not the synchronous intent/check/write/acceptance sequence:
an editor continuation can still block.

Scheduled publication recovery yields to this client's registered pass or
explicit drain, including waiting gaps, and to already-admitted local cleanup.
It retains the original callback and origin/workspace/root/canonical-key context
across a bounded, nonzero deferral instead of capturing a new caller. Conversely,
a drain waits for already-admitted decision work, including its claim-acquisition
handoff. Merely queued recovery and pre-admission freshness checks do not count as
admitted work, so the two sides cannot indefinitely defer to each other.
Admission rechecks exact registrations after reentrant IO; cancellation or a
changed root/key cannot launch work for a replacement owner. These local
observations neither grant native ownership nor turn foreign contention or IO
failures into a local retry. A competing native claim declines that unadmitted
recovery opportunity without a completion callback; later maintenance may offer
fresh work. It is not an accepted recovery job awaiting completion.
Publication recovery alone does not require auto
mode or a live application session and does not authorize proposal application.

Automatic shared-note mutations recheck whether an open session uses the note
at the actual write boundary, after preparation, as well as holding notes
already in use; this applies to the synchronous fallback for worker-ineligible
targets as well as the yielding path. All affected visiting buffers, including
unchanged before-state dependencies, must still be unmodified. Cancellation or
a newly started use prevents new forward writes without disabling safely
owned checked rollback. Note deletion checks local use again after the final
ownership/root IO and buffer checks: those checks can yield and admit a new
reader after the earlier admission check. This last check does no IO and a hold
still follows checked abandonment. A refusal after durable intent preparation follows the
checked abandonment path only for a known admission hold or cancellation whose
complete before-state is freshly proved after any checked rollback. It
authenticates the exact intent and original tokens, checks visiting buffers
before and after fencing, and authenticates that coordinator's cancelled,
empty-payload outcome on its original host. Under the still-owned original
target claim and lock, it verifies full disk before-state and the exact marker
before removing that marker alone. The immutable unmarked intent remains;
no public decision is accepted, so the proposal stays pending. Cross-host checks
stay on their original authorities. Uncertain, mixed or after states retain
ordinary application/recovery accounting. Genuine IO, ownership, fencing or
retirement failures remain actionable and retain unresolved evidence; quitting
during incomplete retirement is not quiet success. Child-phase cancellation fences
ownership before stopping this client's child. Once editor mutation starts,
cancellation stops forward admission but retains the unrenewed workspace claim
through transaction unwind and checked rollback; completion settles it before
the final callback. Expired or replaced ownership still refuses rollback and
retains recovery evidence. Late replies cannot start another write.
An allowed or delivered user quit remains a visible cancellation during setup
or cleanup:
it stops subsequent proposals, and an interrupted write completes its checked
rollback before delivering cancellation. Request-file IO remains interruptible;
only local child launch, process registration and the pure parent handle handoff
defer pending quit briefly, so cleanup can fence the exact claim before deletion.
Inherited outer quit inhibition remains authoritative; startup neither consumes
its pending flag nor overrides the timer/filter/sentinel caller's policy.
Settlement IO failure remains an error even if one idempotent retry succeeds.
Registry cleanup and the final callback still happen once. If neither attempt
establishes a fence, the original unrenewed deadline timer performs one terminal
fencing/retirement attempt; these lifecycle paths never delete the child on an
unproved fence.
Storage failure still preventing that proof is explicitly reported and leaves
the child tracked, not falsely described as a bounded process lifetime.
A nil native settlement does not prove release: retirement requires its accepted
outcome or a freshly authenticated outcome for that exact claim. If neither
exists after a fresh target-clock expiry check, retirement elects and authenticates
expiry for the original token, without acquiring or renewing ownership. Scheduled
publication recovery uses the same proof before release or child deletion; an
unproved release cannot schedule subsequent live-write inspection.
Generic maintenance-process teardown can still force-stop children without
waiting for storage; it does not establish settlement or erase retained evidence.

Callbacks distinguish held admission and ordinary user cancellation from errors;
neither fabricates an accepted decision. Timeout, malformed replies,
authentication, IO and cleanup failures remain errors. Cancellation cannot erase
an already authenticated result or its confirmed updated-file count, and stops
subsequent proposals. A cancelled coordinator fence alone does not identify why
an operation stopped: successful and failed operations also use fencing.

Curated mutations and target-claim settlement take the same target-side `flock`
on the pinned claims directory. Each mutation checks the unsettled claim,
filesystem-clock deadline, expected bytes and mode under that lock; replacement
mode is set on the temporary before rename. A delayed old writer either finishes
before takeover or fails its guard afterward. The same mechanism protects
rollback and marker retirement. This requires Linux `flock` on the storage
host; acquisition waits at most 20 seconds and process death releases the lock.

Application accepts an immutable decision, retires the resolved marker under
target ownership, then publishes under fresh journal-mutation ownership. This
order is shared by synchronous and child-assisted execution. Failed publication
leaves the accepted decision recoverable even after the marker is retired. Status
ordering uses accepted claim generations, so retries in the same second still
produce an unambiguous latest result. Entry-level index checks work independently
of review identity and retained write history. The original pass stays immutable;
only the transaction's working copy receives the checked current index snapshot.
Conflicting entries or topic files produce a stale decision naming the conflict.

### Recovery and reversal

`mevedel-memory-decision-recover-write` reconciles exact retained states without
repeating application: complete writes become applied, untouched attempts become
unavailable, and mixed or intervening changes remain recovery-required with their
marker intact. Its explicit rollback option restores a pending attempt through
the patch engine only when every dependency matches recorded before or after
state. Foreign or unsaved edits prevent rollback.

`mevedel-memory-decision-reverse` reverses a resolved application only while all
affected files match its exact after-state. It persists a separate reverse intent
linked to the original intent identity and hash, then uses the same patch and
decision path. Successful reversal adds an immutable `reversed` decision; the
original application remains inspectable. Reversed proposals remain terminal,
so repeating accept, reject, or reverse cannot silently reapply them. A new
review is needed for a new proposal.

Reversal can also be interrupted: completed reverse writes become `reversed`
when reconciled, untouched attempts leave the proposal `applied`, and partial
attempts remain recovery-required. Explicitly rolling back a partial reversal
restores the previously applied state. Reversal retains exact whole-file checks:
an earlier proposal cannot be reversed through a later proposal's index edits;
reverse those later changes first, or leave the current files intact.

`mevedel-memory-decision-recover-pending` discovers retained write records and
reconciles their independently marked attempts without inference or file
reapplication. It first recovers settled publications under successor workspace
ownership. Unmarked intents are not classified as applications, even if an
external edit happens to match their proposed bytes. Unreadable or unavailable
records stay inspectable. Completed, durably saved root turns schedule this
recovery independently of journaling being enabled. Repeated opportunities
coalesce and wait for idle transport. On local Linux workspaces, accepted
publication recovery and read-only write-candidate discovery run in a batch child
under the workspace claim. Discovery authenticates writes grouped by pass,
discarding bodies between passes, and returns only marked or unavailable
identities. Its directory listing is the cutoff: later arrivals wait for another
recovery. The reply is bounded to 8 MiB and 65,536 identities; overflow or child
failure is an error, never a truncated or successful empty selection.
The editor validates the reply's operation identity and inspects one selected
write per event-loop callback. A currently marked candidate enters a new owned
operation that freshly rereads evidence, current target authority and live
buffers before reconciliation; child observations never authorize mutation.
Changed markers, retired records and unsaved edits are checked after yielding.
Unreadable records report their diagnostic while independent inspections continue.
Native fallback remembers immutable marker paths for successfully read records,
but observes their existence afresh in batches of at most eight, never one
unbounded warm-cache marker program. Cancellation stops the
remaining checks, and the original deadline also bounds this inspection phase.
Root-turn automatic consolidation is offered after recovery finishes, so it does
not race that recovery claim. A queued offer refused by a busy claim does not
consume the scheduling cooldown; a fresh admission observation still does.
Consolidation still recovers under its own claim
before selecting evidence; the intervening live-write phase releases ownership.
A live workspace owner leaves recovery for a later opportunity. The session
chooser and conversation setup do not start this recovery or cleanup; opening a
session only queues journal processing and a consolidation offer. Exit cancels queued
recovery, fences this client's active claims and stops its children. A 180-second
deadline bounds scheduled recovery, and late replies cannot continue it. Remote
workspaces and explicit synchronous recovery retain the target-native path.
Recovery neither requests a model nor repeats or rolls back memory writes.
Because root turns retry recovery, a failure warns once per distinct reason and
later repeats go to `*Messages*`; it states that review waits for recovery, and
an invalid accepted bundle names its pass. Successful recovery forgets reported
failures. Each unavailable write record likewise warns once per session.
