# Memory review contract

## On-demand review

`/remember [focus]` and `M-x mevedel-remember` run bounded consolidation over
eligible digests, captured memory, and applicable instructions. In `manual` and `propose` modes, resulting proposals wait in the memory cockpit
for accept/reject decisions; inspection alone edits no curated files. `propose`
is the default. The bundled `$learn` skill provides a separate, explicitly
requested workflow for writing durable session findings to instructions or memory.

## Model selection

Journal generation and memory consolidation have independent `journal` and
`memory` workloads, both defaulting to the `balanced` tier. They use the ordinary
`mevedel-model-workloads` and preset `:model-workloads` configuration:

```elisp
(mevedel-define-preset my-preset
  :parents (mevedel-implement)
  :model-workloads ((journal :tier balanced)
                    (memory :tier balanced)))
```

These workloads are independent of `summarization` (compaction and handoff)
and `buddy` (edit reviews and guidance). Configure either workload with the
ordinary model policy to select a provider, model, and reasoning effort.

Journal capture freezes the originating root buffer's policy. Changing that
entry affects new captures; already captured jobs retain their frozen policy.
Consolidation resolves `memory` in the caller's current buffer. Automatic review
runs from an idle callback without restoring an originating session preset;
configure its workload globally when a consistent background policy is needed.

## Review evidence and proposal format

The consolidation output contract is implemented in
`mevedel-memory-proposal-parse`. It validates the complete reply against captured
root IDs, admitted files, complete filename observations for creation, and
admitted digest and note IDs before returning any proposals. The sections are
Promote, Update, Merge, Remove, Instructions, and No action. There is no Notes
section or per-note acknowledgement. The request's notes-root filename roster
lists only admitted notes; references and investigation reads do not admit more
notes. Proposal fences
contain JSON-valued header lines, a `---` separator, and Markdown replacement
content. The header is a sequence of `key: value` lines, not a JSON object;
only the values use JSON syntax. Merge sources must be admitted files; overlapping file operations
invalidate the entire reply. Instruction targets must be exact captured
applicable files. Direct MEMORY.md proposals are rejected because application
owns index consistency. This parser performs no reads or writes and does not
establish current authority, freshness, or factual correctness. The request uses
this parser at terminal settlement before accepting proposals into the durable
review and decision lifecycle exposed by the memory cockpit.
The model extracts qualifying knowledge with ordinary proposals citing the
admitted note ID in evidence; nothing else is needed for notes. The accepting
store generates an internal deletion proposal for every admitted note only after
a successful review. Instructions proposals supply the complete
desired content of an exact captured applicable file, preserving unrelated
guidance, rather than appended text. Captured known-absent instruction targets
remain creatable. Empty Instructions bodies mean zero bytes, not path deletion;
other decoded body bytes, including a final newline, are preserved exactly.
One newline immediately before the closing fence is structural, so retaining
a final newline in the desired body requires an extra newline before the fence.
A block with an empty body may omit the `---` separator. Repeated header fields
are invalid. Sections may appear in any order
(each once; every action section required, so a truncated reply still fails),
and a reply may end without `## No action`; text between proposal blocks still
invalidates the reply. These accept shapes real replies
produced, which previously discarded whole reviews. A rejected reply is retained,
newest eight, under `.mevedel/state/diagnostics/rejected-replies/` for
diagnosis; it never becomes proposals. Failed or rejected reviews create no
disposals (see [Shared working-note lifecycle](memory-notes.md#shared-working-note-lifecycle)).

`mevedel-memory-scope-capture` supplies the parser's root and file allowlist.
It reads complete topic, index, and root instruction files without changing
them: at most 32 KiB per file and 256 KiB total, with a 256-entry inventory per
memory root. Topics are taken in name order starting at an offset that changes
each UTC day, so when memory exceeds the total, a different subset is omitted
each day and curation still reaches every topic over time. A review admits at
most sixteen configured memory roots; exceeding
that count refuses capture. Omitted files remain known existing names; an incomplete inventory
cannot authorize creation. Symlinked files and symlinked parents cannot supply
before-state or new proposal targets. The captured index is reserved for the
application transaction, rather than offered as a direct model-editable topic.

Each root retains its original physical path, target identity,
and local host/user identity where applicable. Later configuration changes do
not rebind these IDs. Inspection checks original authority; a different local
client or a retargeted root makes it unavailable. Freshness checks compare
literal captured bytes and expected absence with current target reads, including
indexes and merge inputs selected by the caller. These read-only checks do not
acquire write ownership or commit files; the application coordinator must hold
the required ownership across checking and mutation.

The captured source boundary accepts relative paths only within the original
workspace target. It excludes VCS metadata, `.mevedel` state, symlinks, and all
configured memory roots, including roots omitted because their index was
unavailable. Omitting a memory root does not make its contents available through
a source-file fallback.

`mevedel-memory-reference-check` inspects backticked references in the admitted
topic snapshots, returning at most 200 distinct topic/token observations with
dates. Its current check is workspace-relative **path existence**, including
references with line-number suffixes. A found path does not verify a lesson or
the referenced line. Commands, flags, external references, ambiguous identifiers,
and paths outside the source boundary remain `unknown`. Oversized references,
unavailable topics, and the count limit produce explicit omissions. Topics
without candidates receive no certification. These rows can be stored in the
immutable review record and are supplied to the consolidation request. The
request groups them by identical topic, scope and check date, each group listing
its token/result pairs; every observation is retained, and the stored rows keep
their own schema. On a real 200-row scope this shrank that part of the request
from 56,258 to 19,567 bytes, so more notes fit under the same input bound.

### Investigation tools

`mevedel-memory-investigation` supplies request-local gptel Read, Glob, and Grep
tools. Each call names `workspace`, `journal`, or an admitted root ID, plus a
relative path. This is a scoped tool argument, not another resource-address
family. Memory and journal reads use captured evidence; source Read uses the
ordinary text reader over a pinned UTF-8 snapshot capped at 512 KiB, including
range support within that snapshot. Oversized files are refused before the
reader can scan a long line or a large offset. No tool definitions are added to gptel's global tool registry.
The investigation delivers source text to the reviewer without accumulating
another full-text evidence map in its state or callback. A truncated Read
names the line offset to continue from, because a
whole large document otherwise shows only its first 8 KiB.
A fourth tool, Git, reads the workspace's history without a shell, pager,
hook or optional lock: `log` searches commit messages (at most 30 commits,
optionally limited to a path; a commit hash is shown instead), and `show`
prints one commit's message and changed files plus whether the current HEAD
contains it, naming the commit. Its output is bounded like the other tools,
counts against the same budgets and is unavailable on remote workspaces.
Git remains useful for a material claim about source history; note deletion
does not require Git completion searches or registered resolution quotes.
Commit containment establishes inclusion only, not activation or deployment.

Searches reuse ordinary Glob/Grep over private admitted copies. Source search
examines at most 256 entries, copies at most 2 MiB, and omits files larger than
512 KiB. Directory batches rotate one entry at a time, so pending root siblings
do not consume all descendant visits. Each directory listing is bounded by the
remaining visit allowance; this is a partial snapshot, not an exhaustive search.
Traversal and copy writes yield between short timer slices (at most eight steps
or 20 ms before the next step). A single pinned read or filesystem operation can
still take longer. Each source admission rechecks authority; yielding does not
cache a path's permission or make the whole search an atomic snapshot.
Omitted, unavailable, and excluded source paths produce a partial-search
notice. Narrowing the subtree or reading a named source file supports further
investigation. Results use relative filenames and omit private copy paths.
Each result is capped at 8 KiB of UTF-8; a request may return at most 64 KiB
across 64 calls. The module constants `mevedel-memory-investigation--max-calls`
and `mevedel-memory-investigation--max-bytes` supply these aggregate limits to
both enforcement and the review prompt. Exceeding either limit retires the investigation and
reports failure to its request owner. Stopping cancels active search helpers,
removes their copies, and suppresses late delivery. The owner still supplies
the overall deadline and generation check.
Search completion restores the caller's working directory before delivering
the result, including synchronous empty results. A provider follow-up never
inherits a deleted investigation snapshot as its working directory.

Review results and live usage snapshots also carry a metadata-only investigation
summary. The terminal pass telemetry event retains it under the pass identity,
not in the accepted bundle, public review or model prompt. Each admitted call records its tool,
root, normalized relative path, pattern or Git command and Read range when
present, delivery status, delivered UTF-8 byte count and SHA-256, and partial
snapshot and truncation flags. No result text or error message is retained in
this summary. Target/query fields are capped at 256 UTF-8 bytes each and marked
when shortened. At most 32 call rows fit within 32 KiB of serialized UTF-8;
an explicit omitted-row count distinguishes summary omissions from unused
tools. A bounded roster of admitted note addresses supports joining the pass to
its proposals, without attributing any call to a particular note. This follows
the telemetry enable switch and rotation lifetime, not immutable acceptance.
Cancelled calls and results refused by the aggregate output budget have
zero delivered bytes. These observations establish delivery, not whether the
model understood evidence or used it for any particular note.

### Request limits and settlement

`mevedel-memory-review-request` combines that scope, reference pre-check, and
investigation in a sessionless request using the `memory` workload (default
tier `balanced`). It admits a prefix of at most 20 complete candidate digests and
reports omissions. The
initial prepared provider payload, including tools and roles, must fit both
32,000 estimated tokens and the model's usable context with output reserve.
Indexes, instructions, and topics are supplied as complete documents when they
fit; other captured documents remain available through Read. An empty digest
set requires an explicit memory-only request. Focus and recent rejection text
are each bounded to 8 KiB.

The request has a 180-second deadline. Its cumulative output budgets default to
64,000 tokens (`mevedel-memory-review-max-tokens`) and 256 KiB
(`mevedel-memory-review-max-bytes`). Both settings must be positive integers and
are captured at request start. Reasoning, intermediate replies, and tool-call
arguments share these budgets with the final reply. Both the client token
estimate and available provider-reported output usage must fit the token budget;
the estimate is not the provider's tokenizer and charges non-ASCII text
conservatively. Tool results have the separate investigation limits above.
The client charges tool calls at TOOL using a normalized JSON encoding of
their executable names and parsed arguments, not their raw wire spelling.
Claude Code charges each MCP call before invoking the same investigation tools.
Whitespace and escape spelling in argument JSON are not preserved in that
charge, and in-flight argument fragments have not yet been charged. The byte
counter is therefore an accounted-output size, not a bound on raw transport
traffic. Provider-reported usage, when available, supplies the separate token
guard; the deadline still bounds an interrupted argument stream.

The final proposal reply alone must fit the parser's independent **32 KiB**
limit, checked at terminal completion; reasoning and earlier rounds do not consume
that allowance. Increasing the cumulative budgets does not increase the amount
of proposal text that may be accepted.

Supported provider output limits are clamped to the smaller of the workload's
per-response limit and the remaining cumulative token budget, subtracting the
larger of estimated and reported output usage. Either depleted cumulative budget
refuses another round. Providers without that control retain client-side limits, not a server
billing ceiling. Every follow-up rechecks the prepared payload against usable
context. The initial input admission limit remains 32,000 estimated tokens.

The initial consolidation prompt renders the frozen output, investigation,
proposal, and deadline limits. It asks the model to reserve room for its final
proposals or No action response, and to use focused evidence checks rather than
exhaustive validation. After completed tool results, before a follow-up's context
check and dispatch, the request may inject a budget reminder through native
gptel prompt APIs or Claude's post-tool-batch hook. Reminders fire at 75% and 90% of any cumulative output,
investigation-call, tool-result-byte, or elapsed-time allowance, at most twice;
a jump across both thresholds emits only the highest stage. They report the
remaining allowances. The last stage asks the model to stop tools and return
the final review, with No action valid for unresolved claims. They cannot
interrupt a response already in progress and do not replace the hard guards.
This sessionless request creates no session-backed reminder queue or transcript.

Review callback results and live usage snapshots include accumulated
`:output-bytes`, `:output-estimated-tokens`, and the current round's reply size
(`:result-bytes`), separately from provider-reported usage. The breakdown
`:reasoning-bytes`, `:reply-bytes`, and `:tool-call-bytes` measures cumulative
reasoning, replies across all response rounds, and the normalized tool-call
charge. Thus `:reply-bytes` can exceed `:result-bytes`; only the latter is the
final proposal size at DONE. `:tool-call-count` records admitted investigation
calls, including calls that return errors, but not calls refused after exhaustion.
`:rounds` counts completed HTTP rounds for gptel. For Claude Code it counts
completed native tool batches plus final prompt completion; hidden provider
retries are not counted. Neither count depends on provider token metadata.
Output guard failures
include `:budget-kind` (`output-bytes`, `output-estimated-tokens`, `output-tokens`,
or `proposal-bytes`) and the numeric `:output-limit`; the error message also names
the guard and its measured usage. The pass result and terminal workspace
telemetry retain these counters, including on cancellation and publication
failure, without retaining generated text. A streaming abort can occur before
the provider reports the interrupted round's usage.
Read tools execute within their captured authority without interactive gptel
confirmation; unavailable tool names fail the review. Only terminal model-turn
completion validates the final proposal response (gptel DONE or successful ACP
prompt completion). An intermediate response or tool batch does not finish the
tool loop or consume coverage.

When `memory` resolves to Claude Code, an isolated ACP conversation receives
only the request's Read/Glob/Grep tools through private MCP. Captured roots,
search cancellation, call and result limits stay in the existing investigation
owner; it acquires no root-session permission authority. Initial admission
estimates the explicit system, input and tool definitions. At native batch
boundaries, the client checks the initial estimate plus accumulated visible
output, tool results and budget reminders against usable context. This is a
conservative estimate, not inspection of Claude's exact payload or compaction.
Client output checks run during streaming and before tools; batch boundaries
refuse further model work after exhaustion. Available final-turn usage is
charged once. This path currently has no server per-response token ceiling and
cannot enforce exact provider token spending between batches.

While running, its read-only buffer displays admitted evidence, model output,
and tool results. Closing it or invoking the returned cancel function retires
the request, stops active searches, and suppresses late results. The validated
callback includes the exact validated reply, admitted digests, and cumulative
provider usage. It does not publish proposals or coverage. Connecting the
request to pass selection, storage, and cancellation is handled by
`mevedel-memory-pass-start`.

## Selecting and accepting a review

`mevedel-memory-pass-select` supplies candidate selection from a fresh journal
observation. General passes select uncovered digests; focused passes can reuse
covered ones. Selection returns at most twenty, oldest first with digest ID as
the tie breaker, and reports the eligible backlog beyond that batch. Later
arrivals do not enter an already captured observation.

Published digests describe completed, immutable work and are eligible even while
their source session remains open or another client holds that session. Selection
needs no source-session authority: it changes neither session state nor review
coverage. The coordinator still acquires journal/consolidation ownership and pins
freshly validated evidence before a review starts.

`mevedel-memory-store` supplies private prepared and accepted pass storage. The
caller holds the 180-second consolidation claim. Preparation acquires journal
mutation ownership, recovers accepted expiry, checks that selected entries still
match their public source bytes, and then pins them. This order prevents a pass
from reviving evidence already committed for expiry. The prepared record retains
the original scope, complete digest bodies, source fingerprints, and focus.
Focus must fit the public record's 4 KiB bound before inference begins.

Private records live in `.mevedel/state/journal/passes/<pass-id>/prepared.el` and `accepted.el`.
They use bounded durable Lisp data, preserving literal byte strings in captured
before-state; each record is limited to 5 MiB. Reading disables evaluation and
circular-reader syntax. Evidence pins live under
`.mevedel/state/journal/evidence-pins/<digest-id>/<pass-id>.pin` and bind to the prepared record's
hash. These records are internal storage, outside `memory://journal/`; displaying their
memory bodies still requires the original scope's authority checks.

Acceptance validates the reply again against the prepared scope and exact
admitted digest subset. Each proposal retains its affected topics, merge inputs,
index, or instruction before-state. Its identity includes the pass, original
target and client, proposed change, and before-state. The accepted bundle is
written before its hash competes in the pass claim's immutable outcome election.
Changed private bundles cannot be replayed as accepted results.

Recovery authenticates completed bundles on every operation. A matching public
review with no spare pins needs no republication transaction; accepted expiry
is still recovered under mutation ownership before this observation.

Publication reads that accepted bundle under fresh journal mutation ownership
and publishes the immutable review record. Only this public record advances
general coverage. Replaying publication is idempotent, including after the
original request is gone. Pending proposals retain admitted digest pins;
omitted digests and a no-action pass release their pins after publication.
Failed, cancelled, or expired passes can release only their own pins, after
their unsuccessful outcome has been recorded. None of these storage operations
applies proposals to curated files. Decisions, application, and expiry use the
separate [checked operations](memory-recovery.md).

### Pass ownership

`mevedel-memory-pass-start` coordinates one 180-second workspace claim. Before
selecting evidence, it recovers accepted older pass publications and releases
pins belonging to fenced unsuccessful attempts. Selection runs under journal
mutation ownership after accepted expiry recovery. Preparation rechecks the
selected entries when pinning them; a change between those operations fails
without consuming coverage. Each request checks current target ownership, and
the coordinator's timer includes selection and preparation in its deadline.

For local Linux workspaces whose configured memory roots are also local,
publication recovery, evidence selection, automatic admission, scope capture,
immutable preparation and rejection-history reading run in a short-lived batch
Emacs child. The editor supplies the resolved memory roots, instruction paths,
automatic thresholds and original client identity; the child does not rediscover
user configuration. The claim deadline covers admission as well as preparation.
An automatic opportunity that is not due settles its state as `:skipped`, updates
the next-opportunity observation, and starts neither inference nor a completion
callback. Scope reads transfer in bounded batches of eight while
each admitted file still obeys the remaining total byte budget. The editor
rechecks the claim before starting the configured review from the originating
buffer. Cancellation stops preparation, fences the pass, and ignores late
replies. Remote roots retain target-native preparation in the editor.

Local accepted-result storage and review publication also run in the child.
Only immutable review inputs cross that boundary; automatic proposal application
and its original-target checks stay in the editor. The pass remains running
until publication returns. Cancellation fences an unaccepted reply; an already
accepted outcome retains its bundle and pins for publication recovery. Provider
usage and errors remain visible through the ordinary pass callback.

The returned state is available through `mevedel-memory-pass-running`. Its
`:request` is populated after preparation and contains the read-only review
buffer; the pass remains registered while its result is being published.
`mevedel-memory-pass-cancel`, or closing
that buffer, retires only this client's attempt. Failure and expired callbacks
cannot publish fresh results or change a successor's claim or pins. Success
returns the immutable public review, usage, and remaining frozen backlog count;
arrivals after selection do not widen an admitted daily note cohort. A single
pass never recursively drains the backlog; the cycle coordinator and explicit
drain coordinate successive separately owned passes.
Explicit callers may allow an empty batch for current-memory review. The proposal
cockpit and `/remember [focus]` command are available. Workspace telemetry records
pass starts and terminal outcomes, usage received so far, published proposal and
coverage counts, and remaining backlog without private content.

### Automatic review and application

`mevedel-memory-consolidation-mode` supports `manual`, `propose` (the default),
and `auto`. Manual runs only on request. Propose schedules a read-only review at
eligible completed, durably saved root turns, digest publication, session
opening and idle maintenance (see [Digest generation](memory-journal.md#digest-generation)), and
leaves its proposals for approval. Auto uses the same review and applies fresh
memory and instruction proposals, including changes to `AGENTS.md`, through the
ordinary checked decision path without prior review, followed by shared-note
decisions. The mode is frozen at pass admission.

Automatic admission has two independent clocks: a daily shared-note cycle and
the ordinary journal-only review gate. Ready shared notes do not need journal
digests or journaling to be enabled, and note-only reviews do not postpone a
later journal review.

The **journal-only** gate checks mode, then elapsed time since the last successful
journal-bearing general review, then the eligible unreviewed digest count. The settings
`mevedel-memory-consolidation-min-hours` and
`mevedel-memory-consolidation-min-digests` default to 24 hours and five digests.
Automatic and general on-demand reviews that admit journal digests move this
clock; focused and note-only reviews do not. Retirement records preserve the
journal-bearing distinction and clock after ordinary review-history expiry.
A smaller backlog becomes eligible one day before its oldest digest reaches the
configured ordinary-recall age limit (13 days under the default). Disabled expiry
keeps the count threshold; the elapsed-time gate still applies in either case.
This creates review opportunities for sparse workspaces and completed work from
long-running sessions. A note-cycle batch can also carry digests when their
independent journal gate is due, under the ordinary admission bounds, unless an
oversized note owns that batch alone. Ready notes do not bypass the journal
time or count gate.
General `/remember` bypasses timing and digest-count thresholds, not capture or
mutation safety. It participates in or resumes the note cycle for one bounded
pass; its existing allow-empty/current-memory-review option is not an exclusion
of notes. A nonempty focus excludes notes and does not join the note cycle.

The **shared-note cycle** uses the target clock and becomes due inclusively
24 hours after the preceding cycle's admission, independently of the journal
settings above. This automatic limit is fixed: one cycle services every eligible
note in its frozen cohort through successive bounded batches, rather than one
batch per day. Use an explicit general `/remember` or `/remember drain` to review
sooner; explicit review bypasses timing, not safety checks.
Under the existing workspace claim, it freezes a bounded finite
cohort of eligible path/version metadata before starting bounded batches. The
private `.mevedel/state/shared-notes/cycle.el` record binds the workspace identity,
physical shared root and execution target. It contains cycle identity, admission
time, frozen cohort and hash, attempted members with accepted pass identities,
deferred members and reasons, an in-flight original pass/claim reservation, and
active, complete or paused status with a bounded error class, not note bodies.
It is scheduling state, never authority to read, review or delete a file.
Accepted passes and decisions remain the evidence for review and application.

There is one cycle record per actual workspace journal, not one per
directory/target tuple or a registry mapping aliases to shared physical state.
Changing the shared directory or execution-target identity while a cycle is
active or unreconciled fails visibly closed. Finishing or resolving that cycle
requires its original namespace and authority; operators must restore them
rather than treating a new path or target as a continuation. A completed,
reconciled cycle can freeze the current namespace at the next due admission.
Local-to-TRAMP takeover and alias equivalence have not been established for this
cycle protocol; physical identity fields are checks, not a cross-access mapping
or proof of remote portability.

Admission atomically persists that reservation before yielding note capture,
not only before immutable preparation. A failed preparation that returns no
bundle can be adopted for metadata recovery only when its exact original claim
still owns the slot; fresh ownership then reconciles and pauses it. Cleanup pins
the reserved predecessor even when no prepared record exists. The strict
`mevedel-memory-cycle-inflight-claim` observation supplies that retention pin,
never admission or mutation authority.

Each frozen version is attempted at most once in that cycle. Attempts are
authenticated against the accepted pass and its exact admitted note IDs, not
the number of files deleted. Failed reviews pause the cycle without disposal;
successful reviews generate disposals even when no memory qualifies. Held
extractions or use can still prevent actual deletion. A batch with no deletions does not
stop an otherwise successful cycle. Capture-history ranking, applied-progress
catch-up and the rolling successful-batch ceiling are removed.

Later arrivals and versions changed after the cohort was frozen wait for the
next cycle rather than extending the current run. A captured member that can no
longer be safely admitted is deferred, not silently rebound to its new content;
deferral is not an attempt. An empty cohort is recorded as complete and starts
no note-review inference. An independently due journal review or an explicitly
requested allow-empty current-memory review can still run. Ordinary note,
digest, request, chunk-summary and output bounds
still apply to each batch; the coordinator does not create one oversized
request. Both propose and auto can service the finite cohort; propose leaves
decisions for approval, while auto uses checked application. Manual mode needs
an explicit request.

Turn completion checks cached timing and settings without target I/O. Queued
turns coalesce, and filesystem work waits for idle transport. A failed count
check is cached for ten minutes; timing observations respect the separate
journal and note-cycle clocks. The coordinator acquires target
ownership and recovers accepted publications before rechecking the gate, so
another client's completion cannot be missed merely because the initial
observation was stale. Journaling can be disabled while existing digests remain
eligible for consolidation. Exit cancels queued and active review work.

Cycle continuation preserves fresh ownership, pending/recovery holds, age and
current-use checks, and unsaved-buffer protection. Cleanup or claim contention
before admission preserves the offered opportunity; busy work waits with a
nonzero retry rather than spinning. Failure, cancellation or an application
recovery requirement pauses successor batches without rewriting accepted
outcomes or confirmed-write counts. Automatic opportunities do not resume the
same paused cycle before its daily boundary; an explicit review or drain can
resume it immediately. At or after admission plus 24 hours, an automatic offer
can freeze a new cohort, including still-eligible versions left by the paused
cycle; pausing is not permanent suppression. An active cycle instead retains
its original cohort across later opportunities. Active-cycle recovery uses the
private record and accepted pass evidence without duplicating completed attempts
or reapplying the journal-only gates.
There is no completion-time scan that expands the cohort to newly ready files.
An attempted version is not necessarily reviewed, extracted or deleted. Finite
batching establishes no provider-cost, throughput or capacity guarantee.

Auto applies proposals sequentially with fresh workspace and original-root
ownership for each application. Unrelated index edits are preserved across
reviews and manual changes. Changed topic files or affected index entries remain
stale; cancelled application holds the remaining proposals. Completion reports
the number of distinct files confirmed written by that run, counting a shared index once and
excluding idempotently returned decisions from another call. A successful
review can still leave held, stale, unavailable, or recovery-required proposals.
Neither syntactic validation nor the updated-file count establishes that the
new memory is correct.

## Memory cockpit

`M-x mevedel-memory-list-open`, or **Memory** (`l`) in the session cockpit, opens
the memory table with **Candidates**, **Memories**, and **History** views.
Switch with the clickable header or `1`, `2`, and `3`. Opening uses the
workspace's last proposal counts (the observation the main cockpit's Memory
row shows) at once: Candidates when they report proposals needing attention,
otherwise Memories. Passes, decisions, cleanup and recovery that changed
committed state discard those counts; without them the table opens on
Memories and scans history in the background only to learn them. The table
never switches views by itself: whenever known counts are nonzero, the
Candidates tab label shows them, for example `Candidates (2)`. Proposal history
is otherwise collected only for Candidates and History: with known counts,
opening on, refreshing, or acting in Memories starts no history scan, and
switching to a proposal view loads it on demand. The header labels counts not
from the current table's scan `(cached)`, and says `proposals not checked`
before any observation. Refresh retains
the selected view and each displayed window's row where possible, including
across loading gaps and tab switches. Background completion restores window
positions without selecting a window. Resolved candidates move to History.
Rows show action, type, title, status, and exact target/origin. A candidate's
status reflects the same read-only freshness check acceptance runs: a recorded
`stale` decision that would now apply reads `ready`, and a pending or stale
candidate whose targets changed since capture reads `superseded` (only `r`
resolves it). The history collector batches decision and write evidence and
shares one authenticated bundle at a time across each pass's rows. Locality
classification happens in that same scan; checked recovery still reads separately.
The header shows workspace, pending/recovery counts,
history retention, and this client's running pass. Opening requests checked workspace
recovery without blocking the table. Initial history collection independently
reads committed data even while recovery is pending, dispatching or active; the
header labels the committed snapshot and pending recovery rather than implying
reconciliation has completed. While its bounded observer remains active, recovery
completion requests a fresh history scan only if that recovery changed committed
state (published or released evidence, finished expiry, or reconciled a marked
write); a no-op recovery keeps the scan in flight and the workspace's counts.
The recovery owner exposes this as a per-workspace generation, an observation
for presentation only. After 180 seconds, display observation
ends without cancelling workspace recovery or discarding rows or an active scan.
A visible incomplete-recovery warning remains across `g`, which refreshes data
without claiming recovery completed. Reopening renews recovery observation.
Local history collection runs in a cancellable read-only batch child, returning
detached display metadata, never accepted bundles or write proofs. The header
shows loading or a retryable failure; complete data can arrive after command
return. An empty table also shows a static loading notice while its observation
is pending, rather than looking like a completed empty result. Completed empty
views and failed scans show distinct notices; these are not selectable rows or
decision targets. Switching views reuses snapshots rather than rescanning history.
Reopening Candidates or History can also reuse unchanged terminal rows (applied,
rejected, reversed and completed) from this Emacs process. The display child
compares pinned recursive metadata for workspace state, public journal and
original write-marker parents: membership, type, device, inode, size, mtime,
ctime, mode and owner. Symlinks, failed or unrunnable checks, changed metadata,
or a different workspace root, workspace identity, client or configured memory
roots select a full scan. Non-terminal passes are always authenticated again,
including their terminal siblings. Newly discovered historical marker locations
need a covered scan before they can seed reuse; changes during collection also
withhold reuse. No reuse state is persisted or shared between clients. Metadata
equality is only a display hint, not content authentication or an atomic snapshot.
Memories inventories configured topics independently. `g`, completed decisions
and settled review updates request fresh observations. Current job, context,
session, root, fresh workspace identity, client and configured roots fence replies.
Closing the table retires its display work, not workspace-owned checked recovery.
Authenticated remote retained scopes keep native inspection and can still
block. Invalid local bundles instead produce unavailable rows in the child,
without accessing their untrusted targets or selecting editor-thread collection.
A scan-local retirement listing avoids repeated checks; a final pinned check
rejects a pass whose retirement or bundle removal raced its held proofs. This
is a display observation, not an atomic snapshot or recovery authorization.
Display snapshots never authorize inspection or mutation: those paths
freshly authenticate accepted evidence, original authority and current status.
Explicit accept, reject and reversal commands retain synchronous authentication
and decision processing, which can still block; asynchronous display collection
does not make these manual actions asynchronous.
Refresh preserves the active
session composer draft. Another client's unavailable memory root remains visible,
but inspecting private body/diff evidence or applying there requires its original
authority.

Memories inventories actual topic files in configured roots, including manual
and unindexed topics, with frontmatter title/type and indexed/unindexed status.
Indexes and internal coordination files are excluded. The listing is display
metadata, not a transaction snapshot: each root directory is listed once with
file attributes, and only the index and topics whose attributes changed are
read, together by bounded pinned programs that run asynchronously. Unchanged
files reuse their cached title, type and error within the Emacs session. The
table shows the last observed rows at once, marked `refreshing memories` in the
header (or a loading notice before any observation), and replaces them after
display; `g` keeps rows visible while they refresh. A retired table stops its
remaining reads; rows observed under a changed workspace root or memory
configuration are discarded and observed again. Frontmatter in the
form mevedel writes (JSON-string name, plain type word) is read without a YAML
parse; any other frontmatter is parsed as YAML. `RET` inspects the full
stored body, `o` opens the actual file, and `d` asks for confirmation naming the
exact topic. Inspection reads a fresh bounded snapshot. Deletion freshly
captures the topic and index, then uses the same
checked transaction as proposal application. Its producer is `user`; it runs no
model and fabricates no model response. A concurrent edit prevents application.
The deletion appears in History, where `u` can restore its exact before-state
while that history remains and the current files still match. Unrelated topics
are untouched. History also includes completed reviews with no candidates;
details show the retention deadline or unresolved dependency.

The main cockpit's Memory row shows pending/stale proposals, interrupted-write
recovery, and unavailable-record counts separately. Opening and rendering the
main cockpit use cached observations without reading journal or memory files.
The row says `not checked` before an observation exists and marks observations
older than ten seconds as `cached`. Show Candidates or History in the Memory
table, or refresh there, to collect current counts. Counts are disposable UI
state and never authorize
an application or override its freshness checks.

`RET`/`i` shows the complete proposed body, retained before/after diff, decision
reason, retained digest bodies with their original addresses, and dated reference
checks. A section index opens the proposed body initially; decision status stays
visible while navigating. Each changed file and retained evidence entry has its
own destination. `n`/`p` switch sections; `g` refreshes the same proposal; `q`
closes both panes and returns to the table. Narrow windows stack the index above
the reader. Evidence inspection still works if a public digest is unavailable.
Pending diffs include unrelated current index edits when freshness checks pass.
If they fail, inspection retains the captured diff and shows the current conflict
or unavailable-state reason. Applied/reversed diffs come from the exact retained
write intent. `a` accepts, `r` rejects, `A` accepts pending and stale proposals sequentially,
and `R` rejects pending proposals. Rejection is one key; use a prefix argument to
enter an optional reason. `u` performs checked reversal. `c` reconciles an
interrupted write; `U` explicitly rolls back a known interrupted attempt.
`v` opens a separate inspector for this client's running request. Admitted
evidence and model response occupy separate sections; incoming source changes
update the reader without moving focus. Closing the inspector leaves the request
running; when the source closes, the inspector reports that state. `k` cancels
the request through the pass owner,
and `j` opens pending journal jobs with their inspection/retry/discard actions.
`s` shows the latest shared-folder observation with blocked items and reasons.
Shared-note rows show their disposition as the action and `note`, `attachment`
or `directory` as the type; a disposal whose extraction is undecided shows
`awaits extraction`. Attachment diffs show size and hash.
`D` starts a **backlog drain**; `k` stops it, including between requests or while
waiting for running memory work. `/remember drain` and `M-x mevedel-remember-drain`
start the same operation. The header shows batch number, status and the last
observed notes-remaining count (frozen members past the batch; one changed since
freezing counts until its own batch defers it); completion messages report each batch's notes
and digest backlog. Drain runs successive general reviews, with ordinary-note
batches bounded to 16 notes / 96 KiB and oversized notes reviewed alone in
bounded chunks; digest/request limits remain unchanged. Its first offer can explicitly
bypass the daily note-cycle time gate and journal timing/count thresholds, but
not claim ownership, cohort/version checks, pending recovery, age, current use,
unsaved buffers or mutation checks. It services a finite frozen note cohort;
new arrivals and changed versions do not extend that cohort. Completed attempts
are authenticated against accepted pass identities and exact admitted notes.
Attempted is not confirmed
deleted. An empty selection requests no empty current-memory review.
Pending/recovery-required and blocked notes remain held; propose/manual drain leaves decisions for approval rather
than deleting their files. Other sessions' turns do not stop the drain; notes
they use wait (see [Readiness](memory-notes.md#shared-working-note-lifecycle)). Running local consolidation, application or
cleanup and a busy transport wait with a 60-second retry, so drain neither
races that work nor spins on empty reviews. Closing its
originating buffer or exiting stops it; a failed review stops without retry.
An accepted review also stops the drain if application reports an error or a
recovery requirement. Acceptance, actual application counts and the diagnostic
remain unchanged; a published review is not proof that every proposal applied.
A changed chunked note reports `restart-required`, not a failed review; its
changed version waits for a later cohort rather than being retried in the
current one. The persistent cycle record allows a later resume without
repeating already accepted attempts. Successor batches retain the cycle identity
rather than refreezing another note cohort. Once the note cohort is complete,
drain can service remaining eligible ordinary digests without admitting new
notes. There is no rolling successful-batch cap.

`m` runs another consolidation; a prefix argument asks for focus. The first-class
`/remember [focus]` command and `M-x mevedel-remember` use the same bounded
sessionless pass, allow explicit current-memory review without eligible digests,
and open this table. Completion refreshes an existing matching table without
reopening a closed table or altering the composer. The command may run while a
conversation request is active; already published digests from it remain eligible.
`g`, `?`, and `q` follow the shared cockpit refresh/help/back contract.
