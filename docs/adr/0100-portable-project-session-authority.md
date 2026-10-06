# Use one portable authority for project sessions

Status: accepted

## Decision

Project sessions use one persisted `:authority-mode portable` profile.  The profile is valid for both local and TRAMP access
to the same execution target.  Ownership is a renewable `.lease/`; committed
state is addressed by an immutable publication head.  Project sessions do not
create or interpret `.lock`.

File-workspace sessions retain the separate `:authority-mode pid-lock`
profile and `.lock` authority.  They do not create a portable lease or
publication tree. Both profiles persist and validate a non-empty target
incarnation; the profile chooses ownership mechanics, not whether target
identity is checked.

All authority operations receive the session's explicit profile.  Path
remoteness is only a transport/path concern and cannot choose lock acquisition,
release, renewal, sweeping, cleanup, cold discovery, or mutation admission.
Cold discovery verifies the committed profile before reading state; a mixed
`.lock` and `.lease` directory fails closed.

Archived execution completion uses that same profile: portable sessions read
the authoritative artifact and publish transcript plus sidecar, independent of
path remoteness. PID-lock sessions replace the transcript atomically.

Portable sessions persist a non-empty target-incarnation fingerprint.  Local
and TRAMP probes construct the same canonical boot-id, machine-id, PID-1 start,
and hostname payload, so a replacement target invalidates old authority in
either access mode.  Restore, takeover, unsettled-mutation recovery, and
publication all use that same profile.

Incarnation fencing checks target identity and reachability independently of
child confinement. A missing sandbox must not prevent permission-mode changes;
request and execution admission enforce the chosen confinement requirement.

Retention applies to both profiles with the same age cap and newest-session
floor. A portable session is deleted only through its own lease protocol: an
abandoned head (absent, released without a transfer reservation, or an expired
active or claiming record without unsettled mutation) must also have its last
renewal older than the cap. A head in another lease format is accepted only
when its numeric expiry is itself older than the cap. Cleanup claims the next generation, rechecks
recovery markers and journal pins, and deletes the directory in one program
after proving the claim's bytes are still the newest generation. A competing
claim from another client therefore always wins.

A missing or contradictory authority profile is an error. The session codec
accepts one current format without automatic migration or a dual reader. The
explicit `v0.5.6` conversion tool operates on a separate closed-session copy;
it does not change the runtime authority or codec contract. See
[Sessions](../sessions.md) for the current storage contract.

## Decision history

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
every lease format is an expiry renewed within minutes, so a numeric expiry a
whole cap behind is accepted as abandoned; a head without one is still kept.
