# Artifact store branch review

Review branch: `review/artifact-store` in `.scratch/worktrees/artifact-store-audit`.
Source: `artifact-store` at `ffea346`; full review base `e65137c` (eight commits,
108 changed files). Source worktree had only untracked working notes, no code diff.
Original requirements: source worktree `.mevedel/shared/artifact-store/handoff.md`;
current decisions: ADR 0124 and linked contracts.

## Scope and method

Reviewed all branch changes, including store persistence, leases, shared editing,
model tools/resources, migration/session lifecycle, browser projections, cockpit,
sandbox permissions, cold loading and documentation. Parallel Standards and Spec
reviews were followed by targeted filesystem/concurrency probes, integration tests,
performance measurements, and independent review of the fixes.

No features were intentionally removed. The user explicitly approved separating
host bookkeeping while preserving authored file workflows. Authored paths remain
`.mevedel/artifacts/ID/...`; metadata, versions, comments and editor state now live
under `.mevedel/artifacts/.state/ID/`. There is no reader for the branch's superseded
per-item bookkeeping layout. The explicitly requested per-session migration script
writes the new layout. No real artifact store was migrated or modified.

## Standards

- Concurrent version writers could publish the same next version and lose a
  snapshot. Version/index publication and metadata updates now compare prior bytes
  under the existing target mutation lock. ApplyPatch records its committed bytes.
- Malformed metadata/index filenames and symbolic links could escape the store
  during restore/pruning. Inputs and physical paths are validated at their owners;
  target programs validate their pinned filesystem parents.
  Repeated concurrent-writer tests also exposed a false rejection in Emacs 31.1's
  directory comparison: two root attribute reads differed when a sibling file
  changed its timestamps. The resource owner now compares canonical root equality
  or a trailing-slash prefix after the existing lexical and symlink checks.
  The timestamp regression failed before the fix; 30 real concurrent child-Emacs
  repetitions passed afterward, alongside 92 resource/store tests.
- Late authored-path swaps could make restore overwrite an external file or make
  duplicate read/write outside the store. Source reads now use no-follow target
  operations, and destination writes use pinned parents. Restore records the same
  captured bytes that it writes. Duplicate retains executable modes, nested
  directory modes and empty directories.
- The inline target writer reopened a mutable temporary pathname after creation.
  A real helper-side probe replaced it with a symlink and overwrote an external
  victim. Writes and chmod now use a proved open descriptor; a later authored
  pathname change cannot redirect that output to an external file.
- Lease writes checked only remembered generation bytes. Expiry and a newer
  generation could be missed, and takeover could interleave with the write.
  Acquisition, renewal, release and state/metadata commits now use the same stable
  target lock, proving latest generation and expiry before committing.
- A stopped editor runtime could accept late replies or settle callbacks twice.
  Stop clears ownership and queues before callbacks; late replies are discarded.
- Browser and cockpit restore reported completion before asynchronous state and
  version saves finished. They now refresh/report from the completed callback.
- Shared item title/revision projections were stale after edits. Metadata joins
  the fenced state commit and supplies the lightweight item catalog.
- Store mutations repeatedly scanned all artifacts per room. Nested notifications
  now coalesce and fan out one workspace snapshot, with counts from session
  summaries. Metadata/index reads are batched; local path proofs avoid redundant
  truename walks while still rejecting symlinks.
- Wildcard sandbox mounts protected only existing bookkeeping. Missing files and
  future ids were writable. The protected subtree covers both. Its directories
  persist: removing an empty mount source after one job could let another active
  job recreate that directory writable. Real Bubblewrap tests cover both attacks.
- Git omits empty authored directories. Discovery and collision checks include
  bookkeeping ids so shared items survive clone and cannot be overwritten by import.
- Concurrent dedicated-conversation creation could leave inconsistent ownership.
  Candidates save before claiming the metadata slot; the loser discards its unused
  session and opens the winner. Ordinary chat selection excludes dedicated chats.
- Interrupted migration could be marked complete or remove existing state.
  Completion markers publish last and rollback only owns new directories. Fork
  origins, portable comment attribution, corrupt-session preservation and collision
  handling now have regression coverage. See [migration evidence](migration-review.md).
- Remote editor continuations could disappear inside TRAMP's temporary timer list.
  Shared editing uses the existing transport timer API. Archived history reads
  defer through that API too, with guest/room authority checked before disclosure.
  A regression also reproduced this loss in the idle scheduler's initial timer;
  it now uses the same retained-timer API. Store notifications invalidate stats
  immediately and coalesce one deferred workspace snapshot after acknowledgement.
  SSH acceptance then exposed incoming websocket callbacks reentering those
  reads. A captured stack and a deterministic authenticated-frame regression
  confirmed the overlap. The browser transport now delivers frames and peer
  controls through one ordered idle queue, rechecks availability before each
  delivery, and discards pending input on disconnect/stop. Its 96 focused
  transport/observer checks passed, including nested and asynchronous callbacks.
- Cold abort skipped pending Plan approval handling when its module was unloaded.
  The caller now autoloads it only when approval state requires it.

## Spec

- View-only lobby guests could not open boards/documents. They can now open the
  dedicated editor room with their view authority preserved.
- Lobby whiteboard/document creation and read-only historical previews were missing.
  Creation rechecks authority when queued work executes. Historical previews export
  saved states without restoring or changing live content.
- Attached-session counts were absent; lobby rows now include them.
- Deleted store-only artifacts retained conversation tabs, and answered-in labels
  did not refresh after session renaming. Both projections update while preserving
  active composer drafts, including multiline drafts beginning with `>`.
- A store refresh closed an action menu between opening and clicking Conversation.
  Refresh preserves the selected menu and rebuilds its current permission controls.
- Cockpit deletion derived identity from the physical editor-state path after the
  layout split. It now sends the artifact id and primary filename to the queue.

All 15 grouped Standards findings and six Spec findings above are fixed. The
most consequential Standards finding was writable host bookkeeping in the Bash
sandbox; the most consequential Spec gap was inaccessible shared editors for
view-only lobby guests.

## Verification

The full ERT run, package compilation, local/SSH editor acceptance and separate
provisioned transport acceptance passed.

- Full ERT: 10,058 cases, 10,035 passed, zero unexpected, 23 conditional skips;
  209.11 seconds across six isolated workers, with no unexpected diagnostics.
  Report: `.scratch/test-suite-performance/20261010-104937/summary.json`.
  Log: `/tmp/mevedel-artifact-audit-full-ert-dispatch.log`.
- Package compilation: all 246 files, no warnings or errors; bytecode cleaned
  afterward. Log: `/tmp/mevedel-artifact-audit-compile-handoff.log`.
  The final mechanical `file-name-concat` standards cleanup also passed all
  46 store tests: `/tmp/mevedel-artifact-audit-store-handoff.log`.
- Migration 23/23, including four items from two copied real portable sessions;
  all 223 original files and all 223 input-copy files unchanged by SHA-256.
- Cold/loading/chat/workspace 104/104, including source and compiled artifact
  cockpit first use; fixture diagnostics 77/77; cockpit 14/14.
- Shared editor Node tests 53/53; complete browser suite 67/67, including the
  local real-room scenario. Final local real-room acceptance after all changes
  passed in 43.3 seconds: image moves median 606 ms/max 664 ms, drag preview
  72 ms and image-board typing 167 ms, with unchanged thresholds.
  Log: `/tmp/mevedel-artifact-audit-room-final-dispatch.log`.
- Final provisioned SSH real-room acceptance passed in 135.1 seconds, including
  versions, deletion, recovery and lease fencing. Image moves: median 1,027 ms,
  max 1,328 ms; drag preview 72 ms; image-board typing 976 ms. The existing
  1,200 ms median image gate and 180-second journey timeout were unchanged.
  Log: `/tmp/mevedel-artifact-audit-remote-final-dispatch.log`.
- Separate provisioned transport ERT: 29 passed, four skipped, zero unexpected
  in 340.0 seconds. SSH and Podman execution, Bubblewrap, history search,
  permissions, connection loss, independent-client recovery and transfer passed.
  Three skips are unconfigured Docker cases; one is the opt-in alias-only case.
  Fixtures and containers were cleaned up.
  Log: `/tmp/mevedel-artifact-audit-remote-ert-final.log`.
- Chromium request/menu tests 2/2; protocol/store/artifact/comments JS scripts pass.
- Relay Go tests, vet and build pass.
- Focused store/control-filesystem suite 104/104; combined integration 82/82.
- Real Bubblewrap regressions cover missing/existing/future state and overlapping
  jobs; authored creation, asset writes and atomic replacement remain permitted.

## Validation environment

The original broad run exposed a synthetic `/tmp/.git` created by the agent tool
sandbox. Emacs consequently detected `/tmp` as a project. Final root tests use
isolated temporary storage under `/var/tmp`, removed afterward.

A separate full-suite hang reproduced after 85 immediate HTTP cancellation pairs.
A debugger found Emacs/glibc spinning in async DNS teardown (`gai_suspend`). The
HTTP fixture now waits for the local request to arrive before cancelling it, still
exercising cancellation of a real unanswered request. The corrected pair passed
100 repetitions (200 test executions), and the provider suite passed 32/32.

The broad run also found a SharedCreate fixture that deleted its temporary store
without cancelling its renewing lease. The owning fixture now isolates and
releases its helper and leases before deleting that store; all 54 PTC tests pass.
Two memory fixtures now schedule before simulating a busy transport, preserving
their queued-cancellation assertions with the corrected retained-timer scheduler.
The affected memory/transport/observer checks passed 111 tests.

## Performance evidence

The same provisioned SSH probe measured shared-item read time **3.770 -> 0.155 s**
and one-item store listing **1.165 -> 0.281 s**. The former spent 3.625 s on eight
redundant generic path proofs; both operations now let the existing target-side
program prove paths during I/O. No CRDT cache or weakened checks were added.
The shared catalog likewise batches small metadata records without reading full
editor states. Final remote image-latency acceptance passed at 1,027 ms median
against its unchanged 1,200 ms limit. This remains a relatively narrow margin;
the result establishes this provisioned route, not arbitrary remote latency.

Local before/after benchmarking compiles modules into private temporary trees,
uses 50 artifacts with 20 versions each and three rooms, and JSON-encodes actual
store frames. Transcript publication is stubbed, so this measures store
bookkeeping/fanout rather than a complete interactive turn. The final measurement
waits for deferred fanout before recording completion, and reports acknowledgement
separately. Values below are per operation, averaged over ten iterations (twenty
for version writes); timing log: `/tmp/mevedel-artifact-audit-store-perf-complete.log`.

| Operation | Original branch | Reviewed branch |
| --- | ---: | ---: |
| First patch acknowledgement | 121.2 ms | 20.2 ms |
| First patch including fanout | 121.3 ms | 68.2 ms |
| Repeat patch acknowledgement | 40.7 ms | 14.4 ms |
| Repeat patch including fanout | 40.7 ms | 62.6 ms |
| List 50 artifacts | 7.2 ms | 45.4 ms |
| Record 4 KiB version | 0.5 ms | 19.4 ms |
| Record 1 MiB version | 0.8 ms | 27.0 ms |

First-patch broadcasts fell from nine to three per operation. The additional
version/listing cost pays for pinned reads and serialized publication; these
operations are deliberately slower than the original unsafe implementation.
Coalescing and deferral reduce acknowledgement latency without claiming that
background work disappeared. The benchmark's before/after redefinitions emit
signature-change warnings; final package compilation is checked independently.

## Preserved limits

The documented HTML-comment last-writer-wins policy across Emacs instances remains.
Only primary authored files are versioned; assets retain the branch's existing
behavior. Bash writes create no automatic snapshot until a supported save records
one. Fenced state and metadata writes serialize writers but are not a crash-atomic
transaction: a partial write is reported as a save failure, and may leave an
incomplete new item or stale catalog metadata until retried. Real migration
evidence covers portable sessions; synthetic tests cover PID
sessions. Provisioned remote acceptance shares one host/container route; it does
not establish physical second-host or genuine Docker Engine behavior. Ordinary
entry and Plan Worktree selection exercise callable command seams rather than
rendered UI keypress automation.
