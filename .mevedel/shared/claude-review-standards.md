# Claude engine Standards review

Reviewed `git diff 5adfcfb28c4504968bc76b70a8bed18e8038f776...514ae42`.
The current worktree was clean before writing this report. The initial
uncommitted follow-ups are included in the final commit. Product files were
not changed. The parent reviewer owns compilation and regression checks.

## Main Standards report

No hard repository-standard violation was established. The migration script
has a recorded explicit user exception. The engine seam has two real owners,
keeps native gptel state intact, and shares the existing tool and settlement
pipeline; its abstraction is justified.

1. **[P2, performance judgment] Avoid serial readiness waits on every send** —
   `mevedel-claude-code.el:535-545` runs four version subprocesses and a login
   subprocess before opening each root, directive, child or isolated workload.
   The shared helper waits synchronously in `accept-process-output`, preventing
   the command loop from returning. Three read-only measurements of these five
   commands took **0.302, 0.301 and 0.305 seconds**, before ACP startup or Emacs
   drain overhead. Background naming/guardian/summary requests repeat this
   cost. Prepare checks asynchronously; cache unchanged executable-version
   results while retaining the current-login check and its failures.

2. **[P2, performance judgment] Make native-call uniqueness validation linear** —
   `mevedel-session-codec.el:702-708` rejects duplicate IDs by searching the
   growing `ids` list for every call. Native histories retain every admitted
   call across turns (`mevedel-claude-code-history.el:56-61`), so reopening a
   long-running session acquires quadratic validation cost. The compiled
   complete validator took **2.35 ms at 1,000 IDs, 56.37 ms at 5,000 and
   234.43 ms at 10,000**, per validation, with no GC. Use one local `equal`
   hash table for duplicate detection; preserve durable admission and rejection.
   The same persistence path also builds and prints the complete sidecar twice
   on PID-lock sessions (`mevedel-session-artifacts.el:2023-2027`). Reuse the
   already-built artifact content rather than rebuilding it.

Two findings, both engineering judgments. No broad rewrite, speculative new
engine framework or reduction in the requested workflows is warranted.

## Evidence and limits

### Readiness latency

The measured commands were Node `--version`, Python `--version`, installed
Claude CLI `--version`, the tested ACP adapter `--version`, and Claude
`auth status`. Output was discarded, including all authentication output.
Inherited API-route variables were removed, matching launch isolation. Commands
ran sequentially from `/tmp`; no model request, login change or package
installation occurred.

| Run | Node | Python | Claude version | Adapter version | Auth status | Total |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 1 | 0.007 s | 0.001 s | 0.007 s | 0.179 s | 0.108 s | 0.302 s |
| 2 | 0.006 s | 0.001 s | 0.007 s | 0.184 s | 0.102 s | 0.301 s |
| 3 | 0.006 s | 0.001 s | 0.006 s | 0.185 s | 0.106 s | 0.305 s |

These timings isolate prerequisite subprocess cost. They are not full-send
latency or a provider-performance comparison. The product helper additionally
drains process output and allows timers/process callbacks while waiting; that
does not return control to the editor command loop. Four version probes plus
the authentication probe each have a separate ten-second timeout. Required
login checks are a trust boundary and must not simply be removed.

### Native-call validation scaling

The parent reviewer ran the complete, compiled current-sidecar validator through
the isolated Eask environment, five repetitions for each history size:

| Native call IDs | Five validations | Per validation |
| ---: | ---: | ---: |
| 100 | 0.000239407 s | 0.048 ms |
| 1,000 | 0.011729281 s | 2.346 ms |
| 5,000 | 0.281828277 s | 56.366 ms |
| 10,000 | 1.172166303 s | 234.433 ms |

No GC occurred. Disposable reproduction files are
`.scratch/claude-code-engine/review-codec-benchmark.el`, `.sexp`, and `.log`.
The quadratic cost is confirmed on validation/reopen. Serialization does not
directly invoke this validator, so the report does not claim quadratic
validation on every tool admission.

Call admission separately prepends the new ID, copies the entire native history
into the sidecar, prints it, and strictly publishes before tool effects.
Whole-history publication therefore grows linearly per admission; accumulated
bytes across a growing history are quadratic. That observation alone does not
justify replacing the publication architecture. The local hash-table fix is
small and preserves the safety contract.

The PID-lock duplication is direct call-graph evidence:
`--sidecar-publication-artifact` calls `--sidecar-artifact`, which builds and
prints the sidecar; the PID branch ignores that content and calls
`build-sidecar` plus `codec-write`, which prints the same state again. The
existing file-writing seam can publish the captured artifact bytes after the
same authority check. This has no separate end-to-end timing measurement.

### Scope and standards decisions

Read the repository entry rules, development guide, documentation map/module
map, Claude handoff and PRD, current session/engine contracts and ADR 0123.
Inspected the engine, Claude session/agent/context/history/usage implementations
and broad changes to request admission/settlement, pipeline, reminders, tools,
models, agent runtime, context summaries, memory review, session persistence,
transfer, composer and rendering. ACP/MCP transport internals are assigned to
the separate transport reviewer.

The explicit migration exception is recorded in the implementation progress
log at lines 1959-1979. It was not classified as an unauthorized compatibility
layer. Root/child distinctions and receipt routes implement real requested
ownership and restoration behavior; their conditional branches alone were
not classified as speculative complexity.

## Independent second pass

Rechecked chat dispatch, presets and skill model/permission propagation,
context-summary cancellation, Buddy and memory-review workload lifecycles,
the one-off migration, and package contents. No additional high-confidence
correctness finding was established in those changes. External cancellation
thunks discarded by context-summary/Buddy do not leak the native request:
their buffer teardown invokes the ACP text/workload kill hooks. Memory review
retains its explicit canceller. Native WAIT attaches its FSM before capturing
the effective request policy and draining pending skill context.

Also independently inspected the transport wrapper for the other reviewer's
lost-timer finding. `mevedel-transport--handler-advice` enters
`mevedel-transport-call-as-remote-operation`; its unwind cleanup rearms only
the transport pending-work hash and its held-timer list. It does not restore
arbitrary timers armed by ACP inside TRAMP's temporary timer-list binding.
The transport reviewer strengthened the reproduction by wrapping actual
`with-tramp-suspended-timers` in that production call-as-remote-operation
wrapper. The control still passed and the completion regression still failed
with no terminal outcome. Reproduction source is
`.scratch/claude-review/transport-repro.el` (its
`review-with-tramp-operation` macro).

The separate exclusive-connection wrapper does reactivate timers scheduled
inside its own suspended lists. It therefore must not be cited as an affected
path without further evidence. The proven problem is the ordinary TRAMP
handler path; its timer repair is deliberately limited to the harness's own
registered pending and held timers.

## Implemented readiness fix and validation

Readiness now prepares asynchronously before ACP starts its adapter. Each
status child has a ten-second timeout and a cancellation thunk; connection
shutdown owns cancellation and prevents a late readiness callback from
starting a prompt. The existing connection startup watchdog bounds preparation
and handshake together. Successful versions are cached by resolved executable
identity and nearest package-manifest metadata, including modification/change
times, size and inode. Authentication and the subscription billing route are
checked freshly on every dispatch. Explicit setup/install commands may await
their diagnostics; conversation and background request paths do not wait.

Focused isolated Eask checks passed 139/139 cases across readiness, policy,
ACP, isolated text, Buddy and memory review. A final added connection-owner
cancellation case passed with all 23 readiness-file cases. Tests use real slow
local subprocesses and prove the editor timer runs before readiness finishes,
timeout/cancellation releases process and output buffers, version caching
avoids repeated probes, executable/package/symlink changes invalidate cached
identity, and a fresh bad authentication route stops the connection before
`acp-send-request`. No provider model request is involved.

Independent review of the parent-owned Goal/codec/artifact fixes confirmed
the local hash-table uniqueness check, reuse of prebuilt PID sidecar content,
durable lower-bound marker, and explicitly requested migration update. Two
accounting lifecycle cases were corrected: a setup failure before prompt
submission remains known zero through the submission marker, and prior
complete usage cannot hide missing counters in a later context continuation.

### Resolved dispositions

Both Standards findings are implemented: asynchronous cached-version readiness
with fresh authentication, and linear native-call uniqueness validation plus
reuse of prebuilt PID sidecar content. The transport owner corrected the
ordinary TRAMP lost-timer path. No unresolved Standards finding remains.

The accounting follow-up now tracks complete fields for each native prompt
separately from aggregate lower bounds. Missing sample counters preserve
measured fields, including input measured without its cache-creation component.
A later prompt adds known counters without dropping absent base fields; it
cannot repair an earlier usage gap. A small pending-usage flag lets GetGoal
distinguish an incomplete current prompt from complete previous usage. A real
two-prompt subprocess regression verifies the durable Goal marker, retained
15-token lower bound, and finite-budget limit after unknown continuation usage.

The final focused readiness/context/Goal/ACP run passed 50/50 cases. After the
last partial-input lower-bound refinement, the complete focused usage file
passed 7/7, including the real continuation and sidecar-reopen assertion.
Authentication parsing no longer swallows READY callback failures; the ACP
owner catches asynchronous initialization exceptions and fails immediately.
The parent owns compilation and the final full-suite validation.
