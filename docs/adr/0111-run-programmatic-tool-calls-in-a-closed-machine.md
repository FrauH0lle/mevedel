# Run programmatic tool calls in a closed machine

Status: accepted

## Current decision

Mevedel exposes programmatic tool calling as one hybrid model tool named
`ToolCall`. The name is model-facing prompt surface, so it states what the
tool does rather than abbreviating it; the original `PTC` acronym survives only
as the internal `mevedel-ptc-*` namespace and the `ptc-primitives` skill key.
Its JSON `expression` argument runs in a fresh, in-process explicit
continuation machine, never through host `eval` or `macroexpand-all`. Guest
text is read into a private obarray. Special forms, syntax transformers, and
pure primitives are closed hand-audited tables; operator resolution precedes
argument evaluation, and a static preflight walks the parsed script before
execution, reporting statically visible unknown operators in one error with nearest-match
suggestions and validating literal regexps. Step, time, input, expansion,
tool-call, operand, result, numeric, regexp, and retained-value budgets keep
the guest bounded. Atomic operand accounting counts each use of shared
aggregates before host execution; final-value accounting does the same before
serialization. Fuel pauses return control to Emacs between slices.

The implementation keeps three ownership seams: `mevedel-ptc-interpreter.el`
owns the pipeline-independent guest machine, `mevedel-ptc-driver.el` owns the
nested pipeline lifecycle behind one execution entry, and
`mevedel-tool-ptc.el` owns request roster policy, the adapter, registration,
and aggregate rendering. This split keeps the security-sensitive evaluator
cohesive while isolating asynchronous orchestration state from model-facing
tool policy.

ToolCall has a static description; ToolSearch returns callable contracts without
changing the role's native core. A provably direct call returns the underlying
tool result and supported media, rendered as that tool. A composed program returns
its final value with a separate child audit. A direct text result retains only
a bounded preview in the redundant child audit; the outer result carries the
returned value. Instruction/interaction tools may
require standalone calls. Required hook context and repair feedback survive even
when intermediate values are discarded.

Nested calls use the ordinary structured pipeline for validation, hooks,
permissions, resource preparation, snapshots, handlers, rendering, and post-use
hooks. Children carry `ptc` source and `ENVELOPE/N` identity. Provider-only delivery
runs at the outer consumer. The user can inspect full child output in per-tool
rows; composed programs do not put all intermediate output into provider history.
Nested Reads bypass conversation duplicate suppression so hidden intermediate
content cannot poison later reads or become an unusable reuse stub.

Parallel and parallel-map admit direct tool calls with bounded host concurrency
and source-ordered joined results. Ordinary child failures become guest error
values. Permission denial aborts composition and cancels siblings where possible;
user cancellation interrupts the envelope. Earlier effects are not rolled back.
Fuel/timer boundaries yield to Emacs even when children complete synchronously.

Root scripts persist an envelope before children start and on settlement;
intermediate child progress stays in memory for the next ordinary autosave.
The provider pipeline records settlement after output projection, committing the
bounded preview and any staged full-output artifact together before delivery.
An interruption before this boundary restores the last running checkpoint;
post-use hooks still receive the full result before projection.
Agent scripts use agent interrupted-turn recovery instead of the parent's root
checkpoint. The guest machine is never serialized or resumed after restart.
Runtime-generated prohibited calls fail before their dispatch, but may be
constructed after earlier authorized calls; static preflight is not a transaction.

The [dialect reference](../ptc-dialect.md) owns forms, primitives, regexps, limits,
and composition rules. The [tool manual](../tools.md) owns pipeline delivery and
the native/discoverable capability boundary.

## Rationale, alternatives, and consequences

A closed in-process evaluator provides a stable orchestration boundary without
letting model-authored expressions evaluate host Lisp. Operator resolution occurs
before arguments, and shared aggregates are charged per use before host work and
serialization. The boundary must hold even when operating-system confinement is
unavailable. Eval's separately permission-gated best-effort process tradeoff does
not supply that unconditional orchestration boundary.

This rejects native Elisp evaluation, host macro expansion, property-scraped
primitives, a JavaScript runtime, virtual transcript rows, ToolCall-only
modes, futures, resumable cells, guest notifications, cross-call storage, and
guest media-emission helpers.

It also rejects the alternative of running full Elisp in a child
`emacs -Q --batch` confined by an OS sandbox, which is what the Eval tool's
batch mode does. That boundary is conditional: `mevedel-sandbox-mode` defaults
to `best-effort`, so a failed Bubblewrap probe -- no `bwrap`, no user
namespaces, macOS or Windows, or a TRAMP target that lacks it -- runs the child
unconfined with a disclosure rather than refusing. A process boundary without a
working sandbox isolates crashes and host data structures, not authority: the
child still reads the user's credentials and reaches the network. Eval accepts
that trade because it is permission-gated per call and the user chooses the
mode. Orchestration cannot: scripts are authored from context that includes
tool output, so their boundary has to hold on every platform without a probe.
The interpreter's does. Child media references remain in the user-visible
audit with payload bytes removed. Those broader mechanisms add lifecycle or
trust boundaries that the measured orchestration use case does not require.

## Decision history

### Language and display

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

The original flat child display duplicated preview, full output, and return value,
and could use only one fontification mode. Separate collapsible rows delegate
each child to its existing renderer while retaining one ordered envelope audit.

### Discovery and agent ownership

On 2026-09-07 provider cache measurements moved ToolCall to a static description
and ToolSearch-retrieved contracts. Native schema loading was removed; direct
calls gained ordinary result/media semantics while composed calls retained the
closed guest and audit.

The same lifecycle measurements exposed an additional avoidable prefix change:
an ephemeral generic ToolSearch/ToolCall reminder preceded each worker task,
then disappeared when gptel reconstructed its history. This repeated guidance
already present in the native descriptions. The reminder and its durable
constructor recipes are removed; discovery remains in the native tool
contracts, with result-specific recovery and optional path-skill notices at
their existing seams. Persisted agent templates containing the removed recipes
are rejected under the project's no-compatibility policy; start fresh agents.

The original root-only restriction protected two host seams, not the evaluator.
Agents lacked effective-roster handling, and their session resolution could write
an envelope checkpoint into the root sidecar and later recover it into the root
transcript. On 2026-08-25 agent-local callable catalogs and omission of root
checkpoints for agent-owned scripts removed those restrictions. Built-in roles
now declare ToolCall; their interrupted-turn recovery owns abandoned scripts.

### Durability cost and preflight limits

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

The replacement review exposed a limit in the preflight claim: a runtime macro
can construct a prohibited standalone call after an earlier authorized tool.
We retain the language and its existing no-rollback semantics. Static and
literal-indirect violations fail before execution; computed calls fail before
the prohibited tool dispatches. Direct classification requires provably pure
arguments, so an indirect nested call cannot prematurely settle the envelope.

### September 2026: bound redundant direct-call text audits

A saved 9.6 MB tool span contained a direct Eval child audit with a 9.26 million
character result, although the outer, displayed result was only 2,366 characters.
The direct-call path had bypassed the existing preview bound. Direct text child
results now use that bound; the actual return value and outer result are not
truncated here. Non-text results and underlying renderer metadata stay intact.
An in-memory transformation retained identical full view text and expanded Eval
output. The initial serialized benchmark fixture did not retain that equivalence
after reload, so its latency figures are withdrawn. Original saved sessions are
not rewritten.

### Large-result checkpoint placement

A 2026-09-21 native 9 MiB tool replay found the driver serializing the full result
inside its recovery sidecar before the provider pipeline separately persisted
that output. Moving settlement into the pipeline after projection removes that
redundant large sidecar and commits the referenced output at the same marker.
The checkpoint retains the same schema. Recovery displays the saved preview and
resource reference; it does not reconstruct a multi-megabyte inline tool row.
The driver still commits the running envelope before any child effects.
