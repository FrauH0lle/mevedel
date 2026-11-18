# Memory decisions, application and recovery

## Proposal decisions

The decision module owns each proposal decision as one transaction. Callers use
`mevedel-memory-decision-apply`, `-reject`, or `-reverse`;
they do not acquire locks or interpret child replies.
The write module is private implementation, apart from its control-directory
used for target locking.

Each verb returns a plist with `:status`, optional `:reason`, detached
`:decision` metadata when a decision exists, and an opaque `:continuation`.
Statuses are `applied`, `rejected`, `reversed`, `stale`, `unavailable`,
`waiting`, `held`, `skipped`, and `idle-wait`. A hold or
unmet extraction dependency returns `held` or `waiting`, not an accepted
decision. The cockpit presents those refusals to the user. Automatic application
returns `skipped` for any already-decided proposal.

Foreground decisions take no workspace claim. A serial automatic request may
carry its opaque `:continuation` to the next operation; it carries no reusable
proposal, target or file authority. Do not reuse it across commands.

For automatic application, construct an opaque origin binding with
`mevedel-memory-decision-automatic-context`. It can be reused within the request;
each job has separate bookkeeping, while `mevedel-memory-decision-cancel`
cancels the shared context and its active work. `automatic-current-p` checks the
original actor's live policy and workspace identity rather than the timer's
current buffer. The context is not frozen permission.

Without a callback, application is synchronous and signals errors and quit.
A foreground decision can reuse its local control process until it returns.
Calling the synchronous verb in a quit-inhibited timer or process callback
refuses before admission; it must not block the callback waiting for a helper.
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

### What decides a proposal

The authoritative result is an immutable private slot in
`.mevedel/state/journal/decision-slots/`. An application or rejection elects
`<proposal>.el`; a reversal elects `<proposal>.reverse.el`. Numbered attempt
slots record stale or unavailable attempts without blocking an explicit retry.
Reading a slot verifies its metadata, its source pass and the derived before
and after states against that pass. A public journal entry alone cannot settle
a proposal or release evidence. The public projection contains identities,
status, reason, date and a state hash, not private replacement bytes.

Publication retains the journal-mutation claim. It reauthenticates the slot,
repairs the projection under that claim and releases a pass's digest pins only
when all its proposals have authenticated terminal outcomes. Publication may
lag a committed decision; recovery publishes it without writing target files.
Pass acceptance and inference still use their own claims. They do not hold a
file lock while waiting for a model.

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

### Ownership across files and hosts

```text
lock target root → lock workspace journal mutation directory → reread evidence
  → verify all rows → write changed rows and sync visiting buffers
  → exact workspace owner creates decision slot → unlock → publish
```

Each lock is a Linux `flock` on the directory pinned by a dedicated process on
that directory's host. Both acquisitions share a 20-second budget. On timeout,
inspect the named host, directory and observed holder. If a remote launcher
outlives its editor, verify that the process **still holds this lock** and that
no active editor is using it before manually stopping it. The system neither
kills a suspected orphan nor resumes a transaction on a replacement process.
A lost target acknowledgement or observed dead owner prevents election; two
hosts cannot prove simultaneous liveness. Cleanup uses the same workspace lock
from eligibility selection through acceptance of its expiry manifest.

For `MEMORY.md`, only the proposal's destination and merge-source entries must
match capture. Unrelated prose and entries survive. A rebased index's exact
preimage is saved under a proposal/output-hash key before writing; after a
crash this preserves original bytes, including trailing blank lines. Existing
file modes are preserved. Creating an absent file may not exceed the current
editor's permitted create bits; a refusal names the captured and allowed modes.

| Observed rows, no terminal slot | Accept | Reject |
| --- | --- | --- |
| Every row exactly before or exactly after | Complete missing changes; record applied | Restore changed rows to before; record rejected |
| Any row in a third state | Report conflict; change no file | Report conflict; change no file |
| Shared note already deleted | Record observed deletion | Refuse: deletion is permanent |

This is completion by **choice**, not automatic adoption of matching bytes.
An unsaved visiting buffer also prevents a write. Note use and live automatic
policy are checked at the actual forward boundary; checked rollback does not
require continued automatic permission. Only automatic supersession can reject
a still-present changed note without editing it: the newer accepted review
must identify the same original file and authenticate its current version.
Ordinary Reject gets no such exception.

Automatic application waits for a command-loop opportunity outside timers and
process callbacks. An active minibuffer or intervening input defers admission
without retaining target locks. A continuation rechecks the original buffer,
session, workspace identity, live mode and current note use. The worker may
select proposal/dependency identities or publish journal evidence; only the
editor commits the file transaction and synchronizes buffers. Once a deletion
is acknowledged its accounting survives a quit. A cancellation during a
reversible batch stops forward writes and attempts checked rollback before
releasing its owners.

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

The shared maintenance launcher also owns cockpit display children. Its closed
operation table selects public owner entries and bounded replies. Launch failure
signals to the owner without a callback. After a successful launch, completion
arrives once through idle transport, never in the process sentinel or before
launch returns; disabled or cancelled transport uses a deferred timer fallback.
Explicit launcher cancellation drops live or queued replies; unsolicited signals
and nonzero exits deliver an error once. The operation owner still checks identity
and fences mutation claims before cancellation; display cancellation has no
mutation claim to settle. Native editor recovery remains an owner policy, not
another launcher adapter.

Callbacks distinguish held admission and ordinary user cancellation from errors;
neither fabricates an accepted decision. Timeout, malformed replies,
authentication, IO and cleanup failures remain errors. Cancellation cannot erase
an already authenticated result or its confirmed updated-file count, and stops
subsequent proposals. A cancelled coordinator fence alone does not identify why
an operation stopped: successful and failed operations also use fencing.

### Recovery, reversal and retention

Scheduled recovery publishes missing results and finishes already-authorized
expiry. It never infers a user decision from files, reruns a model, or mutates
memory to finish an interrupted operation. Completed root turns offer recovery;
repeated offers coalesce. A failed recovery names the problem and does not
advance automatic review. Local maintenance can run in a child under its own
bounded journal claim. A remote retained scope selects native execution;
no child reply grants fresh authority over a target.

Reverse restores an applied proposal to its exact retained before-state, only
when each row is exactly before or after. An unrelated subsequent index edit
also refuses reversal. Successful reversal has a separate immutable slot;
repeating any decision cannot apply it again. A shared-note deletion is not
reversible. If some changed row is already before and all changed rows are
before/after, the cockpit says **A reversal may be incomplete. Choose Reverse
to finish it.** An identical manual undo is indistinguishable. Retain the
applied evidence in that state (or when the target cannot be checked); a third
state or recorded reversal permits ordinary history expiry. Accept on an
already applied proposal returns the decision without redoing the write.

Old passes without the slot format are not automatically selected or migrated.
Explicit decisions explain that a later review can propose the change again.
Retained historical files and existing memory are untouched. An already
authorized old expiry manifest can finish only its named deletion set; a
malformed manifest refuses with its filename.
