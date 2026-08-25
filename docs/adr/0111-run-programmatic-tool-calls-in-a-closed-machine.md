# Run programmatic tool calls in a closed machine

Status: accepted

Mevedel exposes programmatic tool calling as one hybrid model tool named
`ToolScript`. The name is model-facing prompt surface, so it states what the
tool does rather than abbreviating it; the original `PTC` acronym survives only
as the internal `mevedel-ptc-*` namespace and the `ptc-primitives` skill key.
Its JSON `script` argument runs in a fresh, in-process explicit
continuation machine, never through host `eval` or `macroexpand-all`. Guest
text is read into a private obarray. Special forms, syntax transformers, and
pure primitives are closed hand-audited tables; operator resolution precedes
argument evaluation, and a static preflight walks the parsed script before
execution, reporting every unknown operator in one error with nearest-match
suggestions and validating literal regexps. Step, time, input, expansion,
tool-call, operand, result, numeric, regexp, and retained-value budgets keep
the guest bounded. Atomic operand accounting counts each use of shared
aggregates before host execution; final-value accounting does the same before
serialization. Fuel pauses return control to Emacs between slices.

Two boundaries were relaxed after a profiled test session (2026-08-25) showed
the model burning full turns rediscovering them: the guest regexp subset now
admits unnested quantifiers (`*`, `+`, `?`, `\{n,m\}` on one literal, escape,
dot, or bracket class, capped at eight quantified atoms) while still rejecting
quantified groups, stacked quantifiers, alternation, and backreferences.  The
work budget conservatively raises input size to the quantified-atom count,
backstopping the residual polynomial adjacency case; and the
pure-primitive table gained the syntactic `file-name-*` helpers (applied with
file-name handlers disabled), `take`, and a fixed-comparator `sort`, because
the model hand-rolled basename/dirname from `split-string` twice in one
session. The audit standard is unchanged; the default answer to "is this pure
string/list manipulation?" moved to yes.

The implementation keeps three ownership seams: `mevedel-ptc-interpreter.el`
owns the pipeline-independent guest machine, `mevedel-ptc-driver.el` owns the
nested pipeline lifecycle behind one execution entry, and
`mevedel-tool-ptc.el` owns request roster policy, the adapter, registration,
and aggregate rendering. This split keeps the security-sensitive evaluator
cohesive while isolating asynchronous orchestration state from model-facing
tool policy.

Nested calls use `mevedel-pipeline-run-tool-outcome`, the structured consumer
of the ordinary pipeline. Validation, hooks, permission, resource preparation,
snapshots, handler execution, render transforms, and post-use hooks remain one
common path. Provider-only reminders, nudges, persistence banners, Goal
warnings, and transcript side channels are applied only by the provider
consumer. Each child carries source `ptc` and identity `ENVELOPE/N`; the model
receives only the script's final value on ordinary success, while the settled
ToolScript row owns elapsed time, output disposition, and the ordered child
audit. Hooks, permission logs, cancellation, and telemetry retain the same
parent/child identity. Full child output is user-visible inside that audit but
does not enter provider history. That audit is presented as one collapsible row
per nested call, each rendered by its own tool's renderer: a single flat body
could carry only one fontification mode and duplicated every child result three
ways (preview, full output, and the returned value).

The per-request ToolScript description lists the effective active and deferred
callable roster. Direct calls and ToolScript remain available together.
`parallel` and `parallel-map` are the only concurrency forms: each entry is one
direct tool call, the host owns a small concurrency cap, and joined results
preserve source order. ToolScript is root-session-only and is removed from
inherited, role-declared, and deferred tools when a retained agent freezes its
request configuration. Ordinary child failures are guest `(:error MESSAGE)`
values. Permission denial aborts the script and reports bounded completed work;
user cancellation interrupts the envelope. A denial cancels sibling pipeline
continuations where possible, and synchronous completions are admitted in
bounded timer turns so the batch yields to Emacs. Permission waits and child
status are live trusted view facts. An in-flight machine is never serialized or
resumed: after restart it is an interrupted tool call. The sidecar checkpoints
only the envelope and bounded child audit; recovery materializes that record as
a tool row and atomically consumes it with the repaired segment.

Checkpoint durability is per script, not per nested call. The original design
rewrote the sidecar through a full publication after every child transition;
the profiled test session put 40 of its 58 publication generations (69%)
inside six script windows, stored 3.72 MiB of publications for a 224 KiB
transcript, and serialized nominally parallel children ~180-300 ms apart for
20-30 ms of actual work — and on TRAMP targets each write is a remote round
trip. Now the durable writes are the start checkpoint (before any child runs)
and the settled write; between them child audit progress is journaled in the
in-memory session checkpoint, which any unrelated autosave captures. The cost
is recovery fidelity after a crash mid-script: the child audit restores as of
the last autosave rather than the last child, and the row still settles as
interrupted either way.

Nested Reads bypass session-level duplicate suppression in both directions:
the model never sees nested output, so a "reuse the previous contents" stub is
unusable inside a script, and recording the access would poison the
conversation's later Reads with content that never entered provider history.

This rejects native Elisp evaluation, host macro expansion, property-scraped
primitives, a JavaScript runtime, virtual transcript rows, ToolScript-only
modes, futures, resumable cells, guest notifications, cross-call storage, and
guest media-emission helpers. Child media references remain in the user-visible
audit with payload bytes removed. Those broader mechanisms add lifecycle or
trust boundaries that the measured orchestration use case does not require.
