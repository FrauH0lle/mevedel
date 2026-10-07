# Claude engine: whole-branch review and fixes (2026-10-07)

Review branch: `review/claude-code-engine`.
Worktree: `/home/roland/Projekte/mevedel/.scratch/worktrees/claude-code-engine-review`.
Reviewed the entire `5adfcfb28c4504968bc76b70a8bed18e8038f776...2b4e30f` range,
including later authentication, quota, migration, loading, browser and media work.
The source feature worktree was left unchanged. Eight independent reviewers
covered standards, specification, transport, lifecycle, loading, collaboration,
authentication and resources; two reviewers independently inspected fixes.

## Correctness findings fixed

| Area | Failure and correction | Regression evidence |
| --- | --- | --- |
| Isolated shared conversations | Fresh item turns reused root mention/path acknowledgements and consumed root mail. Their context now has request-local ownership and leaves root mail queued. | Real ACP/MCP shared workflow tests cover both directions and nested instructions. |
| ACP hook decisions | Native calls ignored hook replacement/stop/block decisions. Apply decisions before transcript/MCP delivery, validate rewritten arguments through the pipeline, withhold replaced media, and settle publication failures. | Actual subprocess tool calls check wire output, no second effect after stop, and asynchronous failure settlement. |
| Retained input | Emacs could not requeue restored failed follow-ups, while Resume falsely appeared to release them. Explicit requeue preserves provenance; Resume checks both queues. | New cases fail on original HEAD and pass after correction. |
| Model recovery | Local model changes left blocking errors recorded under request/preflight IDs. Clear only root model-category failures at provider selection. | Real readiness passes after selection; unrelated holds and failure pause remain. |
| Cold loading | Directive previews, answer lookup, direct session creation and Buddy command discovery depended on prior eager loads. Load their owning modules at actual feature boundaries. | Fresh source and compiled Emacs exercise public entry points. |
| Cold collaboration | First room publication lacked the task-projection owner. Load it during room setup. | A cold real room runs its publication timer. |
| Browser recovery | Newly failed inputs/history choices did not reach connected owners. Publish changed recovery state; retain login drafts and selected provider; scope auth events to matching credentials. | Owner-only frames, queued failures, histories, cross-provider state and draft tests. |
| Agent hydration | An ordinary timer could be lost during TRAMP suspension, leaving loading pending permanently. Use the existing transport timer owner. | Timer suspension regression. |
| Lobby startup | A failing or interactive remote root probe could stop subsequent lobby restoration. Suppress interaction and isolate each root failure. | Failed first root does not prevent later local restoration. |
| Required locks | Editor `create-lockfiles=nil` silently disabled credential/runtime serialization. Force and verify noninteractive locks. | Real lock files with the user preference disabled. |
| Login failure | Lock acquisition failure left an active operation with no work and no retry. Publish terminal safe failure. | Unavailable credential store permits retry. |
| HTTP lifetime | Cancelled late authentication responses leaked buffers; absent response buffers waited for timeout. Clean late buffers and fail missing requests immediately. | Cancellation, late callback and absent-buffer cases. |
| Runtime readiness | Successful installation falsely cleared authentication failures and caused quadratic session publication. Preserve login blockers, invalidate preflight, and publish once per session. | Three-session test counts saves and preserves authentication issues. |
| PDF input | Mention page images were prepended into gptel in reverse order. Reverse insertion to retain authored page order. | Actual staged bytes and gptel media collection, plus cleanup. |
| Resource clicks | Logical links skipped canonical parsing, breaking escaped names and accepting malformed suffixes. Canonicalize at click dispatch. | Button-level valid/invalid address cases. |
| Address families | History roots and skill aliases dispatched raw spellings despite accepting escapes elsewhere. Dispatch decoded family names consistently. | Canonical-equivalence cases. |
| Remote test runtime | Eask socket isolation changed XDG_RUNTIME_DIR for Podman too, making its provisioned containers inaccessible. Restore the client runtime only inside the Podman wrapper. | Native ACP/MCP target workflow and provisioned SSH/Podman acceptance. |
| Large remote publications | One oversized pipe write stalled image-bearing saves. Send the same bytes in bounded pieces through the common synchronous/asynchronous carrier. | Byte-exact binary publication and transport-fence tests; actual image-state SSH benchmark. This performance defect predates the branch. |
| UTF-8 validation | The touched parser already admitted malformed bytes and out-of-range Unicode. Reject invalid decoded names/pointers. | Invalid byte, surrogate and upper-bound cases. This defect predates the branch. |

No product feature was removed. Migration remains supported as explicitly
restored on the reviewed branch. ACP cannot implement gptel's separate hook
`:confirm` UI or rename an admitted native call: these unsupported hook requests
now fail before effects instead of silently ignoring their control intent. The
ordinary mevedel permission pipeline remains authoritative. No stored schema
change or destructive conversion is introduced by this review.

## Standards

The independent standards pass found two concrete performance/simplicity issues:
quadratic runtime-recovery publication and repeated full tool-registry allocation.
Both are fixed. Native roster construction now uses existing category/name lookup
and still checks exact tool object identity. No cache or new registry was added.
Central command autoloads, the two-engine seam, and isolated native histories were
retained because their current callers need them.

## Spec

The independent specification pass confirmed the isolated shared-context failure
above. It is fixed, including mailbox isolation discovered during the follow-up.
Cross-engine continuation, directive isolation and capability guards yielded no
additional verified defect in that pass. The newer intentional model-fallback and
explicit migration behavior were reviewed as later branch requirements, not
removed to match an older PRD snapshot.

Original review: Standards 2 findings, Spec 1 grouped finding; all fixed.
The broader area reviews produced the remaining concrete fixes in the table.
Second-pass review caught and corrected publication failure hanging an MCP call,
a later nil block verdict clearing denial, auth broadcasts overriding unrelated
owners' selected provider, and interrupted asynchronous pipe dispatch leaking
its child. A final independent carrier review found no unresolved issue; the
regression verifies single settlement, child/buffer cleanup and quit propagation.

## Performance evidence

- Runtime-update publication changes from N-squared to N durable save attempts;
  a three-session regression requires exactly three and preserves login blockers.
- The native roster benchmark compares compiled old/new roster functions over
  the actual 52-tool registry, 1,000 calls per sample, five samples. Visibility
  is held constant to isolate registry cost. Median old time is 665.57 ms versus
  14.68 ms, about 45x faster for this lookup work. Old samples trigger seven GCs;
  new samples trigger none. This is not an end-to-end request speedup claim.
- Browser recovery tests require 20 unchanged status publications to perform no
  model-catalog rebuilds or repeated runtime-file reads. Runtime state is read at
  room setup and refreshed by maintenance events.
- An optional provisioned shared-editing room run timed out with image moves
  around 12.5 seconds. A reduced real-publication probe traced the time to pipe
  writes, not the 0.2-second readback. The same 1.4 MB image state costs
  5.7–9.9 seconds on the original base and 9.4–9.8 seconds on this branch.
  Bounded 1024-character writes reduced it to 0.65–0.99 seconds while preserving
  byte framing and mutation fences. Both carrier paths share the small helper;
  ADR 0101 records the measured rationale. The full SSH browser workload with
  bounded script and payload writes has a 1,212 ms image-move median versus its
  unchanged 1,200 ms limit (maximum 1,554 ms). This is a substantial improvement,
  but that acceptance gate still fails; the reduced probe is not a substitute
  for the browser measurement. Remaining reads and post-helper authority
  checks protect canonical state and the asynchronous commit gap. Replacing
  them with a cache or changing the durable layout would require a separate
  design; neither was introduced to chase a 12 ms threshold miss.
- A final reduced real helper profile on provisioned SSH (interpreted source)
  renamed an item in the same 1.4 MB canonical image state: 1,264 ms cold,
  1,008/1,117 ms warm. Warm commits cost 699/791 ms; canonical reads cost
  131/186 ms. Admission and commit authority checks cost 145/117 ms, with
  nested timings overlapping. JSON encoding/parsing took about 4 ms and warm
  Node dispatch about 6 ms. Durable remote operations dominate. This excludes
  browser/relay work and does not override the failing browser gate. Log:
  `.scratch/remote-edit-profile/helper-final.log`.
- Existing source/compiled loading checks retain deferred modules while covering
  first use. No performance threshold or model-call latency gain is inferred.

Benchmark script/results: `.scratch/roster-benchmark.el`,
`.scratch/roster-benchmark-results.el`. Focused logs remain in `.scratch/`.

## Verification

- Full isolated Eask suite: **9,663 tests, zero unexpected, 23 conditional skips**,
  152.54 seconds. Every worker exited zero; inspected logs contain no unexpected
  messages/warnings. Report: `.scratch/test-suite-performance/20261007-131416/summary.json`.
- `npx @emacs-eask/cli compile`: **240 production modules plus the existing
  test-foo example**, no warnings. Compilation used a disposable copy; all
  production source hashes match the reviewed worktree. Log:
  `.scratch/review-compile-complete.log`. No generated bytecode remains beside sources.
- Shared editing: **51/51 Node tests**, **67/67 browser tests**, including the
  real local room, passed after rebuilding generated assets. The rebuild made
  no tracked generated-asset changes. Logs: `.scratch/shared-editing-tests.log`,
  `.scratch/shared-editing-browser-final.log`.
- Viewer protocol and artifact-comment protocol checks passed; the separate
  request-panel and artifact-comment browser cases passed **2/2**.
- Relay `go vet ./...`, `go test ./...`, and `go build .` passed.
- Provisioned SSH/Podman acceptance: **29 passed, zero unexpected, four skipped**
  in 308.32 seconds; the outer provisioning/cleanup script exited zero. Coverage
  includes native ACP/MCP, history search, execution, sandboxing, recovery and
  ownership transfer. Skips are the unprovisioned Docker and opt-in alias-only
  cases. Log: `.scratch/review-remote-complete.log`. This is container-backed
  transport evidence, not a physical second-host claim.
- Optional SSH shared-editing browser acceptance still fails its unchanged
  image-move latency gate: **1,212 ms median versus 1,200 ms**, as explained
  above. Log: `.scratch/remote-edit-profile/room-header-final.log`. It is not
  counted among the passing local browser cases.
- `git diff --check` passed. Dependency sources were copied from the feature
  worktree's isolated Eask environment; their hashes are in
  `.scratch/review-dependency-hashes.json`. Upstream gptel was refreshed and read
  before implementation.

Early runs are not represented as passes: the first full suite caught an
intermediate unbalanced test edit, subsequently fixed and covered by the green
integrity/full run. Initial browser runs exposed missing fixture owner imports;
these were corrected without weakening assertions. Compilation initially found
one overlong new docstring; it was wrapped before the warning-free final run.
The initial remote run exposed the Podman runtime fixture failure. A subsequent
run passed all ERT cases but its shell failed because this review edited the
running script; the final stable-script run above exited zero. Neither earlier
run is presented as successful end-to-end validation.

No live subscription model calls, account changes, merge, or deployment were
performed. Prior live Enterprise subscription evidence is historical and was not
re-run; it does not become separate Pro/Max account evidence through this review.
