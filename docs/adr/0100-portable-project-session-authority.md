# Use one portable authority for project and file sessions

Status: accepted

## Decision

Project and file-workspace sessions use one persisted `:authority-mode portable`
profile.  The profile is valid for both local and TRAMP access
to the same execution target.  Ownership is a renewable `.lease/`; committed
state is addressed by an immutable publication head.  Sessions do not create
`.lock`; a `.lock` or plain sidecar without a lease is an unmigrated file
session that only the migration script reads.  Sessions persist and validate
a non-empty target incarnation.

Every lease record names its holder by host identity (boot ID, machine ID,
PID namespace), user ID, PID and process start ticks.  A client of the same
user on the same host reclaims an `active` or `claiming` lease at once when
that process is provably gone or reused; any other or uncertain evidence
waits for expiry under ordinary acquisition, as does a `publishing` lease.
An expired `claiming` record that never activated is replaced on ordinary
acquisition without a prompt; the successor inherits the head and unsettled
mutation latch. This does not authorize a heartbeat to reclaim a foreign lease.
An explicit forced takeover can replace an unexpired live or foreign holder
after a full yes/no confirmation naming the host, buffer, last renewal and
status. Publishing additionally requires confirmation that the prior client is
stopped because a critical write may still be in flight. Ordinary acquisition
never forces. Exclusive generation creation elects the winner, and the exact
publishing-generation/head compare-and-set fences the old holder's commits,
renewals and releases. Already committed state remains authoritative.

On observing a newer activated foreign generation, the old holder stops its
request, retained agent turns and live executions, becomes read-only and follows
committed publications. Unpublished work here is discarded. A superseded async
publication whose head has advanced under that foreign generation is retired
without rollback, acknowledgement or retry; its pending local batches are
dropped. A local supersession callback releases any deferred turn-settlement
hold without running the old terminal hooks. Queued preparations that have not
published fail their acknowledgements
with a takeover error, and new submissions fail immediately after confirmed
takeover. A renewal failure or this client's expired lease alone is not evidence
of takeover and does not discard its queued preparations. Fenced retained-agent
settlement retires the local runtime without publishing or changing its retained
transcript buffer. Each admitted agent invocation records the session's local
ownership epoch; a mismatched epoch or confirmed takeover keeps later settlement
and saves fenced even after the parent takes control back. The epoch advances
only on gaining ownership after another holder or confirmed demotion, not on
same-owner lease rotations or reclaiming this client's own expired record.
Ordinary lease loss retains normal settlement and recovery. Taking control back
must finish issuing the old epoch's pending stop before acquiring or adopting;
a blocked stop refuses without changing authority. Deferred provider cancellation
acknowledgements do not block take-back, because late settlement remains epoch-fenced.
An unexplained head change still retains unresolved recovery. Project
file effects cannot be undone by the session lease: old-holder producers may
continue until ownership loss is observed and the publication gate permits stopping.

All authority operations receive the session's explicit profile.  Path
remoteness is only a transport/path concern and cannot choose lock acquisition,
release, renewal, sweeping, cleanup, cold discovery, or mutation admission.
Cold discovery verifies the committed profile before reading state; a mixed
`.lock` and `.lease` directory fails closed.

Archived execution completion uses that same profile: portable sessions read
the authoritative artifact and publish transcript plus sidecar, independent of
path remoteness.

Portable sessions persist a non-empty target-incarnation fingerprint.  Local
and TRAMP probes construct the same canonical boot-id, machine-id, PID-1 start,
and hostname payload, so a replacement target invalidates old authority in
either access mode.  Restore, takeover, unsettled-mutation recovery, and
publication all use that same profile.

Incarnation fencing checks target identity and reachability independently of
child confinement. A missing sandbox must not prevent permission-mode changes;
request and execution admission enforce the chosen confinement requirement.

Retention applies to both workspace categories with the same age cap and
newest-session floor. A portable session is deleted only through its own lease protocol: an
abandoned head (absent in an existing `.lease`, released without a transfer
reservation, or an expired
active or claiming record without unsettled mutation) must also have its last
renewal older than the cap. A missing `.lease` directory and unreadable or
unsupported generation records are unknown authority and keep the session.
An unmigrated session with no PID lock is not evidence of a modern, abandoned
session. Known carrier scratch payloads are not
generation records; observation and election preserve them, and deletion still
proves the unfiltered name set. Their names do not prove producer termination.
Cleanup claims the next generation and
uses the same final deletion boundary as explicitly confirmed discard: exact
owned bytes, newest generation, target-clock deadline, and no retained recovery
or journal pins. Election and deletion programs share the stable sessions-parent
directory's target-side lock. Delayed activation verifies its exact candidate
before writing, rather than recreating a removed candidate. No lock spans user
confirmation or journal processing.

The age sweep and remote chooser batch their read-only discovery across
sessions. The cost in target programs depends on the read stages, not the
number of healthy sessions. An expired candidate that reaches deletion still
gets the same live authority and retention checks as an explicit action.

In a local-shell TRAMP replay of opening the picker with 102 published
sessions and the age sweep due, medians over three samples per build were:

- TRAMP shell commands: 3,019 before, 528 after.
- Target control programs: 932 before, 13 after, including 106 versus 2
  that read publication payloads.
- Wall time to present the picker: 19.52 seconds before, 5.86 seconds after.

This fixture exercised the scan without deleting a session. It measures
TRAMP command traffic and local processing, not network latency.
Doubling the fixture to 204 sessions kept the first opening at 560 TRAMP
commands and 13 control programs in one sample, the same as the first
102-session opening. The target still does more work as the list grows.

The chooser exposes recovery-blocked, unowned sessions with Recover, Discard,
and Cancel. Discard revalidates session/publication, workspace, and target
identity without restoring the conversation. It abandons recovery and settles
captures through their existing owners before deletion; unsupported evidence
remains blocked. Confirmation acknowledges permanent data loss and that prior
writers have stopped, not proof of process termination. A later failure may keep
the session after approved capture omissions or recovery abandonment completed.

A missing or contradictory authority profile is an error. The session codec
accepts one current format without automatic migration or a dual reader. The
explicit [migration script](https://github.com/FrauH0lle/mevedel/blob/master/scripts/migrate-sessions.el)
upgrades supported closed sessions through ordered version steps and validates
their Goals. It validates a separate staging copy, then installs it in place
while retaining the original as a backup outside session discovery. This
standalone migration does not change the runtime authority or codec contract. See
[Sessions](../sessions.md) for the current storage contract.

## Decision history

The same 2026-11-14 change removed the runtime PID-lock implementation after
moving file sessions to the lease. Keeping unused branches made tests and
callers select obsolete fixed-path storage and bypass the publication contract.
Durable fixtures now exercise real lease ownership and committed artifacts;
workspace-free fixtures carry no durable authority. Legacy PID sidecars remain
input data only for the standalone migration script. That step did not remove
the lease takeover restriction: forced takeover of a live or foreign holder
remained unavailable until the confirmed-takeover amendment below.

Removing the PID-specific paths intentionally changes these interactions:

- A conflicting live lease opens a read-only inspector immediately instead
  of offering break/read-only/abort. Closing that buffer is the abort action;
  it does not acquire ownership or change the holder's state.
- A missing published live transcript fails closed with an explicit error,
  rather than offering to delete the directory during resume. Confirmed
  Discard is the separate destructive action and retains its authority checks.
- Cleanup no longer exempts a pre-lease `.lock` because it names this Emacs
  or an apparently dead PID. Such evidence refuses cleanup unchanged; close
  the session and run the standalone migration script first.
- Resume no longer salvages orphan fixed transcripts by advancing the segment
  counter. The lease publishes transcript and counter in one generation, so
  the old transcript-before-sidecar crash ordering cannot occur. Arbitrarily
  inconsistent manually edited trees have no automatic salvage path.

### 2026-11-14: Confirmed takeover replaces the PID break

Roland required parity with the PID lock's live-holder break before removing
that implementation. The portable lease now offers the read-only follower a
separate confirmed takeover, shared by project and file-workspace sessions.
Unlike a PID break, the successor generation fences later storage commits by
the old holder. No new authority capability or on-disk format is needed: whoever
can write the session directory could already create a successor generation.

Implementation review exposed an additional async reconciliation case: if the
successor had already published, the old holder retained an unresolved job and
its active-publication gate blocked following forever. A newer activated
foreign generation proves supersession, so that case drops unpublished batches
without rollback or acknowledgement that might duplicate inherited execution
mail. Preparations still queued before publication instead receive exactly one
failure acknowledgement; mail batching also releases its pending slot. Same-client
and otherwise unexplained head changes keep the existing unresolved behavior.

Agent interruption itself waits behind publication, so enqueueing it during the
old job could let that job's cleanup discard the interrupt. Demotion is immediate
but stopping waits for the publication gate and is re-derived on the existing
view poll or retained session heartbeat, with no extra timer kind. The heartbeat
only retries stopping, never renews lost authority, and ends after producers stop;
closing all views cannot strand a running agent. If reconciliation errors after
a validated newer foreign generation was bound, the next tick can retire the job
from that irreversible fence without a fresh target read. Dropped checkpoint callbacks
also relinquish their buffer-local pending identity before later control reuse.

Review reproduced two additional failures: taking control back could adopt over
an unstopped child process, and a retained agent's normal terminal publication
could fail against the foreign lease and prevent demotion from completing. All
acquisition now first completes the pending stop, or asks the user to try again
if the publication gate still blocks it. Fenced settlement cancels and retires
only local runtime state, keeping the transcript buffer unchanged. New publication
admission fails immediately after confirmed takeover, so a producer cannot keep
the stop behind an endlessly replenished queue. Holder labels shown in
confirmation and takeover messages lose text properties, control characters and
Unicode format characters (including bidi controls) and are capped at 80 characters.

A deferred ACP cancellation exposed a second epoch boundary: after local demotion
and take-back, the old terminal callback could use the parent's new lease. Each
provider invocation now captures the local ownership epoch, including retained
followups. A mismatched epoch retires locally even if the parent currently
owns authority. Late cleanup also checks buffer invocation identity before
ending its request, so it cannot interrupt a newer turn reusing that transcript.
Authority assertions can themselves yield to takeover and take-back. Settlement
rechecks the epoch after those assertions and before building terminal publication;
if the epoch changed after staging, it rolls back the local transaction and retires.
Review reproduced a false fence during ordinary remote mutation: arming and
settling the unsettled-mutation latch rotate the lease generation under the same
owner. The local ownership epoch therefore replaces the originally chosen lease
generation; same-owner rotations and own-expired reclaim preserve that epoch.

On 2026-11-14 Roland decided to keep one session-ownership implementation:
file-workspace sessions moved from the PID-lock profile to the portable
lease, replacing the earlier decision that kept both profiles. A matched
measurement (compiled, CPU-pinned, six processes per tree, local disk, median)
timed the editor command loop, not just wall time.  The auto-save checkpoint
runs asynchronously on the lease: its longest single block fell from 11.9 ms
(small transcript) and 20.0 ms (200 turns) under the PID lock to 7.1 ms and
7.2 ms, although total Lisp time per checkpoint rose to 22.1 and 28.4 ms.  The
synchronous writer used at turn settlement and explicit saves grew from 11.9
and 20.0 ms to 42.7 and 49.4 ms in one block, resume from 36.2 and 45.7 ms to
159.0 and 163.9 ms, and an idle lease renewal costs 4.1 ms every 30 s.  These
are the costs project sessions already paid. Immediate reclaim of a proven
same-host dead holder moved into the lease as described above, so those
`active` and `claiming` leases no longer wait for the 90 s expiry and a
confirmation. That first step did not reproduce the PID lock's forced break;
the confirmed-takeover amendment above adds it. Without that explicit command,
uncertain holders and `publishing` leases still wait for expiry and the
applicable confirmation. Existing PID-lock file sessions convert through the
migration script.

The 2026-11-18 change moved warm local root settlement to the publication
queue. It favors shorter editor stalls over faster completion. In a matched
source-loaded, unpinned local replay (five saves per transcript size), the
median longest interval of a 1 ms heartbeat fell from 58.867 to 9.182 ms with
2 turns and from 82.973 to 25.237 ms with 200 turns. Median save wall time rose
from 60.599 to 77.437 ms and from 84.749 to 96.459 ms respectively. These are
not directly comparable to the earlier compiled measurements. A separate
matching replay of closed-segment reconciliation (three runs, 31 segments,
6,000 closed turns and no execution rows) fell from 259.915 to 18.007 ms median:
it still reads the published text, but avoids property restoration on segments
without execution markers. This measures reconciliation, not all of resume.

The original migration commands required separate session/artifact invocations,
new destination directories, and a manual swap. The user requested a single
automation-friendly command taking a session or project directory, together
with version steps whose oldest prefix can be retired. The unified script now
owns those mechanics and backup handling; runtime compatibility remains
explicitly out of scope.

ADR 0100 introduced the portable profile in session format v0.5.2, replacing
transport-selected ownership for project sessions. It initially described file
sessions as lacking an incarnation; the current shared codec requires one for
both profiles. The original record did not document that later extension's
reason. Keeping the incarnation requirement explicit avoids implying that a
PID lock makes target identity irrelevant.

A later audit found archived execution settlement still selected publication
by path remoteness. Running its poisoned-cache regression through local project
access reproduced a missing-record failure: settlement read the fixed cache
instead of the immutable artifact. The handler now selects storage by the
session's authority profile, completing the transport-independent decision.

The same audit reproduced a strict metadata call reporting failure after its
bytes had entered the publication queue. Strict metadata and archived-transcript
updates now request the publisher's existing required-commit mode, which rejects
reentrant calls before staging and treats committed-head cleanup failures as
diagnostic. Callers no longer reconstruct commit semantics from a queued result.

Mode-derived required confinement in Edits exposed another coupling: a remote
target without Bubblewrap could not switch to Ask or Full Access because mode
changes ran the current mode's sandbox readiness guard. The incarnation fence
now probes identity without requiring child confinement, preserving ownership
and replacement checks while allowing recovery through the mode controls.
An initial correction forced an `off` probe at this fence. Review showed that
request admission then alternated the mode-sensitive readiness cache between
the session requirement and `off`, repeating the full remote probe suite.
The fence now retains the cached confinement mode and ignores only its
`sandbox-unavailable` block for identity observation. It still observes every
mutation, preserves blocked child-execution readiness, and rejects other target
failures.

Discovery profiling on 2026-09-21 put manifest path qualification at about
470 ms while listing 109 saved sessions. Each artifact repeated the same five
canonical-spelling calls, although target-side reads already prove physical
containment. Capture now canonicalizes the session root once and accepts only
normalized artifact names directly inside immutable generations. The three-run
discovery median fell from 1.16 seconds to 743 ms without caching ownership
observations or weakening artifact verification.

Resume profiling on 2026-09-21 found 64 lease-clock checks costing 447 ms in
a complete archive with 28 retained agents. Artifact reads now look for a staged
candidate before proving lease ownership; without a candidate they read verified
bytes from the captured immutable publication. Ownership is still required before
reading staged bytes. Cold restore fell from 5.13 to 4.70 seconds and control
programs from 112 to 51. This avoids proving authority for a read that does not
depend on it, without caching or weakening the staged-read proof.

Large tool-result profiling on 2026-09-21 isolated 648 ms of a roughly 794 ms
PID/file-session completion inside the target control program. Passing a large
base64 field through shell function arguments repeatedly copied it. Keeping
operation variables in the decoder reduced the completion to 443 ms; streaming
oversized required temporary-file writes reduced it to 157 ms in the same
actual-input fixture. Stdin fields now carry their encoded length and a trailing
NUL; a truncated frame fails before destination replacement. Small argv requests,
optional operations, parent descriptor proofs, and result classification retain
the same semantics. The portable project fixture, which performs additional
publications, fell from 2.80 seconds to 907 ms before pipeline yielding. These
measurements changed data transport within the existing authority boundary.

A follow-up 2026-09-21 collection replay found the opposite-direction copy in
control reads: command substitution retained an entire encoded manifest before
emitting it. Ordinary operation responses now stream their payload first and
append the operation identity and status afterward. The receiver consumes bytes
only after a matching successful completion record; partial failed reads are
not values, and required failures still skip subsequent operations. The private
archive cold scan fell from 31.1 to 20.7 seconds without changing
stored manifests, retention, descriptor proofs, or the batched archive reader.

A second follow-up on 2026-09-21 found that stdin framing concatenated each
large encoded payload into an operation string and then copied it again into
the complete request. Framing now joins the fields once. Immutable publication
also hashes the literal staged bytes already loaded for its write, avoiding a
second file read. Together with the ASCII normalization fast path, three paired
9 MiB completion replays reduced median completion from 775 to 726 ms and
cumulative string allocation from 131 million to 96 million characters. Worst
input delay remained about 240 ms: synchronous lease checks and publication
writes still occupy the final pipeline step. The measured benefit justified
removing copies, but not weakening those proofs or changing the persisted format.

Portable stores were originally exempt from auto-cleanup because the lease
protocol had no deletion claim, so cleanup could have raced another client
resuming the same session. On 2026-10-03 this repository's own project store
held 128 sessions (4.6 GB), 63 of them past the 30-day cap (about 1.1 GB, back
to 2026-07-19). None had pins, recovery markers, or a live lease. The
exemption never reclaimed anything, and the generation election that already
fences abandoned-storage recovery (plus the control program's `verify-latest`
proof) supplies the missing deletion claim. Cleanup now treats both profiles
alike and selects their ownership checks by profile.

The first full test run with portable cleanup deleted 42 of those expired
sessions from the real store. A cold-load test subprocess ran in the checkout,
registered it as a workspace, and swept it from `kill-emacs-hook`. The
project-store exemption had hidden this; file workspaces had the same hazard.
Exit cleanup now runs only in an interactive Emacs, because a batch Emacs
registers workspaces incidentally rather than through a user's session.

Cleanup was throttled to once per workspace per Emacs invocation and triggered
only by the chooser and exit, so an Emacs left running for weeks never expired
anything despite the documented auto-cleanup. A daily per-workspace throttle
replaced it, and opening a session or ending a root turn now offers an idle,
transport-deferred sweep. Batch Emacs is excluded from these sweeps for the
same reason as from exit cleanup.

That run also kept 24 expired sessions whose August lease records predate
`:transfer-generation`. Current code cannot interpret or resume them, so
treating every invalid head as owned would have kept them forever. Liveness in
every lease format was treated as an expiry renewed within minutes, so a numeric
expiry a whole cap behind was accepted as abandoned; a head without one was kept.
Crash-session discard replaces that permissive fallback with strict generation
validation: the existing head reader could skip unreadable records and thereby
mistake unknown authority for absence. Recovery blockers are now visible rather
than silently bypassed. Review also established that a single unlocked
verify/newest/delete program still allowed a competing claim between proof and
deletion; the shared election/deletion lock closes that cooperating-client race.
