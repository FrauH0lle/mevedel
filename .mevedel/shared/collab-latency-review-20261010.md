# Collaboration latency review

The review covers all 18 commits of `collab-latency` work from `cc85951` through
`6c01fda`, including the editor, publication and turn/skill branches merged into
it. The original worktree was clean. Fixes live on `review/collab-latency` in
`.scratch/worktrees/collab-latency-review`; the source branch is unchanged.

The performance changes remain: immediate editor sends, batched item commits,
lease-scoped cached state, room broadcasts, retained transcript projection,
pre-encoded snapshot chunks and deferred store notices. Review found concrete
correctness defects around those optimizations, plus defects in the benchmark's
success criteria. No user feature was removed.

## Fixed findings

| Area | Failure and correction |
| --- | --- |
| Filesystem writes | The cheaper inode comparison could accept an outside file opened through a substituted symlink. A second pathname substitution made the new check pass, then truncated the outside file. Restore the descriptor-path proof; retain the independent safe process reductions. |
| Error diagnostics | Capturing raw stderr in a shell variable discarded NUL bytes. Restore binary-safe encoding and its existing contract. |
| Batched editing authority | Deferred edits lost their cancellation and authorization checks. Recheck all contributors before committing; reject the dependent uncommitted batch together when a contributor loses authority. |
| Batched editing leases | A batch could survive losing and reacquiring its lease and overwrite another holder's edits. Retain the original holding identity and refuse a changed holding. |
| Editing shutdown | Stopping during fallback batch commit left queued work running. Honor the pending stop after settlement. |
| Cached shared reads | A missed heartbeat left cached content trusted after another host could take over. Conservatively require recent renewal for cached reads; token inspection performs no target I/O. |
| Queue limits | Deferred jobs disappeared from request and byte accounting. Include them in both limits. |
| Empty room joins | Empty records compared equal to an absent snapshot cache, so no final snapshot frame arrived. Require an existing cache before reuse, including empty snapshots. |
| Tool identity | Fixed tool IDs could be reused by anonymous tool records. Reserve IDs fixed in the new projection while preserving identities adopted during pending-to-canonical settlement. |
| Pending tool publication | Published records shared mutable pending entries, so completion changed the comparison snapshot and suppressed a terminal update. Copy pending records at the projection boundary. |
| Record revisions | Unchanged publication reset retained revisions. Keep the previous revision and increment only on a change. |
| Projection complexity | Repeated list indexing made record assembly quadratic. Consume the canonical list once, preserving order. |
| Skill preparation ownership | Out-of-order nested completion could restore a finished request forever; stale completion could overwrite a newer agent invocation. Restore in stack order through the existing deferred settlement queue and preserve both ownership slots. |
| Settlement teardown | Buffer death could park cleanup in a dying buffer, leaking checkpoint/settlement state. Run cancellation cleanup directly during teardown. |
| Interrupted callbacks | A quit in one resumed callback discarded all remaining continuations. Drain the remaining callbacks before propagating the quit. |
| Reentrant turn completion | Saving artifact versions could admit a new request, then the old request marked it idle. Set the old activity before yielding to version publication. |
| Host busy detection | Native retained agents could run while the host reported idle. Include pending agent admission, running turns and terminal publication. |
| Skill notifications | Every event resolved filesystem paths, including remote paths; overlapping plugin/missing roots could suppress real changes. Cache resolved roots at installation and compare paths without I/O in the callback. |
| Editor save status | The save pump scheduled a timer with no pending edits; that timer erased a later validation error. Schedule the rate-limit timer only while edits remain. |
| Store notifications | An immediate metadata notice left an obsolete delayed notice queued. Cancel it when the immediate publication already includes the content change; clean pending timers between tests. |
| Benchmark joins | The timer stopped at welcome before the snapshot finished. Wait for the final chunk and fail bounded waits/disconnects. |
| Benchmark reliability | Missing observations and prompt timeouts could still produce successful output. Require all expected reliable observations; presence remains explicitly best effort. |
| Benchmark correlation | Reusing an item could count old rectangle geometry as a new update. Give measured elements unique identities per run. |
| Benchmark lifecycle | Cleanup could delete the stop file before Emacs read it, leaving the host alive. Track, terminate and wait for child processes before deleting their workspace. |
| Benchmark interpretation | Writer intervals follow acknowledgements, rather than specifying a fixed arrival rate. Correct the historical interpretation and identify the unsupported local prompt scenario explicitly. |

## Standards review

Independent standards review covered the full diff and the deep-module,
loading, testing, state-isolation and maintainability contracts. The retained
cache and batching have measured purposes; neither warrants blanket removal.
The fixes keep authority and lifecycle mechanics inside their existing owners.
A second review caught two defects in intermediate fixes: target I/O introduced
into a token accessor, and loss of adopted anonymous tool identities. Both were
corrected before final verification.

## Specification review

The branch's measurement report and current collaboration/shared-editing/skill
contracts supplied its recorded intent. Independent specification review found
empty snapshot completion and benchmark readiness/reliability mismatches.
Those are fixed, and the historical measurement report now explains its timing
limitations. A further lifecycle review found the buffer-death and quit cases
above. The original report's WAN numbers are historical observations, not new
measurements of this review branch.

## Validation

The isolated Eask full suite completed all 10,122 cases with zero unexpected
results and 26 environment-dependent skips (205.57 seconds). Cold loading and
compilation tests are included. Package compilation covered 246 files without
warnings. Bytecode was then removed with Eask; benchmark snapshots retain their
own compiled copies. The suite report is
`.scratch/test-suite-performance/20261010-191030/summary.json`.

Targeted regressions reproduced the material failures before their fixes. The
completed local Node gates comprise 55 shared-editing unit tests, 72 browser
tests, nine benchmark tests, viewer protocol checks, regenerated assets and a
rebuilt relay. Remote acceptance exposed two test timing assumptions: checking
browser attachment cleanup before its acknowledgement, and requiring one lossy
presence packet. A held-reply editor regression proves the first; a real-relay
packet-coalescing probe proves the second. The acceptance scenario now waits
for the actual UI completion and exercises sustained drag input within the same
deadline. The full browser suite then passed all 72 cases. The provisioned SSH/container
shared-editing acceptance scenario passed, including concurrent browsers,
deterministic native tools, recovery, transfer and stale-owner fencing.
Validation logs are retained under `.scratch/review-validation/`.


## Performance comparison

Both versions were byte-compiled with GNU Emacs 31.1 and the same isolated Eask
dependencies. The baseline is the original branch tip `6c01fda`; the reviewed
snapshot matches all 245 current mevedel source modules. Three alternating
before/after runs used the corrected runner against a local relay and temporary
host. Each writer waited for acknowledgement plus a 100 ms interval. Other test
processes had finished before measurements began.

Each table entry is the median of the three run summaries, not a pooled
percentile. Editing runs used 20 edits for one writer and 12 edits each for five
writers. Join runs used four guests and a seeded 200-turn transcript. Every
fixture began with a nonempty transcript so the original empty-snapshot defect
did not prevent baseline participation.

| Measurement | Original branch | Reviewed branch |
| --- | ---: | ---: |
| Two guests, peer observation p50 | 18.5 ms | 20.1 ms |
| Two guests, peer observation p95 | 20.5 ms | 21.2 ms |
| Twenty guests and five writers, peer observation p50 | 57.2 ms | 56.4 ms |
| Twenty guests and five writers, peer observation p95 | 98.9 ms | 99.3 ms |
| Four guests, complete snapshot join | 186.5 ms | 185.4 ms |
| Retained room projection, 2,000 records plus streamed tail | 35.05 ms | 28.35 ms |
| Retained room projection, 10,000 records plus streamed tail | 483.53 ms | 298.46 ms |

All 243 edits and 3,489 expected peer observations arrived in each version.
The safety fixes carry a small two-person latency cost in this local workload;
the multiwriter and join results are effectively unchanged. Removing quadratic
record assembly improves long-transcript projection. The room-projection test
uses the caller-facing `mevedel-collaboration--project-records`, appends streamed
text between calls, and checks equality with a full canonical projection. Each
run takes the median of five calls with garbage collection before timing.
This is projection cost, not total network publication time.

The existing canonical-only projection benchmark also preserved full/retained
equality across 10, 50, 100, 200 and 400 synthetic turns. At 400 turns retained
projection remained about 29 ms versus about 137 ms without retained state.
The original single-target-program steady-edit assertion continues to pass.

Raw outputs and dependency hashes are under `.scratch/perf-results/`.
The local experiment drivers are `.scratch/measure-latency.py`,
`.scratch/measure-projection.py` and `.scratch/measure-publication.py`.
The gptel request-source SHA-256 used by both snapshots is
`249971e56d0ba5fb757c8eba8dd598a91325ce9ffc7dbce9aa3ae04f7362b37f`.

## Limits

These measurements use a local relay and synthetic transcripts; they do not
re-establish the historical WAN deployment numbers or browser rendering costs.
The 26 ERT skips require additional remote, sandbox, graphical or interactive
facilities. The separate remote browser acceptance exercises provisioned SSH
and container targets on this host, not a physical second machine or Docker
Engine. Changes have not been deployed or merged into the source branch.
