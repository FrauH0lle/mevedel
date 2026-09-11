# Retain delivered reminders and completed provider response fragments

Status: accepted

Delivered reminders remain at their original conversation boundary until
compaction retires the corresponding history. Eligibility remains sparse and
state-driven; later state changes supersede historical descriptions. Staging
without delivery creates no history. The existing trusted injection record now
stores complete bodies and reconstructs a separate user message after the
current prompt or complete tool results.

This reverses the ephemeral reminder policy formerly documented in
`docs/reminders.md`. Fully installed lifecycle measurements found six changed
history boundaries and five cached-token drops across eight continuations.
Permission/verifier reminders disappeared during reconstruction, changing an
early prefix. Independently, an offline reproduction showed that gptel rebuilt
grouped tool calls as interleaved call/result pairs. Correct IDs alone did not
preserve the delivered conversation shape.

Preserve each completed provider tool-response fragment beside its rendered
transcript. Reuse it only for the same provider, model and cache policy when
the entire covered source is unchanged. Paired boundaries and source/prepared
digests protect context selection and later filtering. A prepared digest
includes role properties: changing a span to ignored must not resurrect it.
Provider changes, edits, omitted results and incomplete spans use ordinary
backend serialization. This preserves multi-model operation without forwarding
opaque fields between providers. Transport credentials are not recorded.

A repaired tool result exposed a boundary mismatch on the next user request:
its hidden audit split the result into two `gptel` tool runs, and the backend
tried to read the trailing `Note:` prose as another call plist. Prompt-copy
preparation removes hidden audit/render metadata between runs of the same tool
ID before computing prepared digests. This preserves one complete result for
ordinary serialization and retained replay while leaving transcript audits
intact.

The alternative of reparsing every live continuation into a common text form
was rejected: it can discard provider-specific reasoning fields and signatures.
Keeping reminders ephemeral would retain the measured prefix disruption.
Keeping a new snapshot of the entire request on every dispatch would cause
unnecessary cumulative storage growth.

The selected design duplicates individual response content in local storage
and keeps more historical guidance in context. Those are explicit costs.
Compaction consumes decoded guidance and ordinary displayed evidence, excludes
encoded reconstruction metadata, and removes records with retired history.
Estimation counts guidance bodies rather than metadata size. Provider cache
retention and serving remain outside mevedel's control. No migration repairs
previously incomplete history.

## Dynamic-context extension

A subsequent source audit found environment dates, Goal counters, memory indexes,
skill catalogs and resource availability embedded in the system prefix. The
previous live fixture held those inputs stable, so its successful history replay
did not establish stability when they changed.

Deliver these named observations, and workspace guidance, through the existing
retained reminder transaction. Keep the behavioral contract stable. Deduplicate
against complete trusted reminder messages actually present in the realized
payload, in payload order, rather than assuming all source history was selected.
A derived incremental transcript index avoids repeated decoding; it is rebuilt
after history edits and cold restore. Agent component selection is frozen and
persisted; inline authored components remain system content.

This preserves freshness and context-loss recovery without an independent
persisted acknowledgement schema. Short catalogs and an on-demand memory manual
reduce unnecessary procedural content. See
[retained instruction context](../architecture.md#retained-instruction-context)
for the delivery contract.

## Complete current-state snapshots

Partial fact updates preserved the prefix but smaller models sometimes treated
omitted sections as unavailable. A 216-request comparison found correct facts in
36/36 complete-snapshot reports versus 30/36 partial-update reports. Adopt one
complete snapshot of selected environment, active Goal, skills, memory and
resource facts whenever any selected fact changes. Keep repository instructions
and Goal procedures independently retained. The snapshot remains valid until
superseded; unchanged turns add no snapshot. Recipient selection and actual
outgoing-history acknowledgement remain authoritative.

This adds 16.4% total input in the measured growing workload, with more uncached
input for three models. Cache discounts can buffer repeated content, but new
snapshots still require an initial uncached delivery. The user accepts this cost
for the measured factual reliability and accepts Flash's three remaining
repository reporting failures as a known model limitation. The detailed
216-request experiment is archived locally under
`.scratch/instruction-simplification/instruction-snapshot-measurements.md`;
the decision and its measured tradeoffs are recorded here for clean checkouts.

When the general supported model baseline reaches gpt-5.6-sol's capability level
or better, reconsider partial updates to reduce repeated context. Sol and Pro
passed all nine partial-update reports. Recheck current-state and lifecycle
behavior across that future baseline before changing the decision. Keep one
delivery design; do not infer a runtime capability gate from model names.
