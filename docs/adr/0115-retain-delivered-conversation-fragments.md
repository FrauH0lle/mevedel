# Retain delivered reminders and completed provider response fragments

Status: accepted

## Current decision

Delivered reminders remain at their original conversation boundary until
compaction retires that history. Eligibility is sparse and state-driven;
staging without delivery creates no history. Trusted injection records retain
complete bodies and reconstruct separate user messages after the current prompt
or complete tool results.

Named workspace guidance, environment, Goal facts and policy, skill catalogs,
memory indexes, journal discovery, and resource availability are independently
retained observations. When a selected section changes, deliver its complete
current contents and explicitly state that other sections remain applicable.
Unchanged sections are not repeated. Empty or inactive state is an explicit
observation, not silent omission.

For gptel-controlled history, a section is acknowledged only by a complete
trusted message actually present in the realized outgoing payload, in payload
order. Context loss, filtering, or restore therefore causes missing current
sections to be delivered again. A rebuildable transcript index avoids repeated
decoding. Agents receive only their frozen selected components; authored inline
components remain system content. There is no independent persisted
acknowledgment schema or runtime model-name gate. See [retained instruction context](../architecture.md#retained-instruction-context).

An external engine that owns model history acknowledges the same independent
observations at its own receipt boundary instead of a realized gptel payload.
The Claude engine's receipt decision is recorded in
[ADR 0123](0123-keep-turn-authority-in-mevedel.md); its behavior is described
under [native context delivery](../sessions.md#native-context-delivery).

Completed provider tool-response fragments are retained beside the rendered
transcript. Replay requires the same provider, model, and cache policy and an
unchanged complete covered source. Paired boundaries and source/prepared digests
protect context selection and filtering; prepared digests include role
properties so a newly ignored span cannot reappear. Provider changes, edits,
omitted results, and incomplete spans use ordinary backend serialization.
Transport credentials are not retained.

Prompt-copy preparation removes hidden audit/render metadata between runs of
the same tool ID before prepared-digest computation. This preserves a complete
tool result for both ordinary serialization and retained replay, while keeping
its transcript audit intact.

## Rationale and consequences

Removing delivered reminders or reshaping grouped tool exchanges changes an
early conversation prefix even when call IDs remain correct. Retaining their
actual delivered shape avoids that reconstruction loss without snapshotting the
entire request on every dispatch. Reparsing every continuation into a common
text representation was rejected because provider reasoning fields and
signatures can be lost.

Changing system-prefix facts also invalidates everything following them.
Independent retained updates preserve freshness and prefix continuity while
avoiding unrelated repeated facts. They cost additional history and duplicate
individual provider responses in local storage. Compaction consumes decoded
guidance and displayed evidence, excludes encoded reconstruction metadata, and
retires records with their history. Estimates count guidance bodies rather than
encoded metadata. Provider cache retention and serving remain external limits.

## Decision history

- **Ephemeral reminders to retained delivery:** installed lifecycle measurements
  found six changed history boundaries and five cached-token drops across eight
  continuations. Permission/verifier reminders disappeared during reconstruction.
  An offline reproduction also found grouped tool calls rebuilt as interleaved
  call/result pairs. Retained reminders and provider fragments addressed those
  separate losses. Previously incomplete history is not repaired by migration.
- **Dynamic observations:** a source audit found dates, Goal counters, memory
  indexes, catalogs, and resource availability in the system prefix. The earlier
  live fixture held them constant, so successful fragment replay did not prove
  stability when they changed. Named facts moved to retained delivery; workspace
  guidance and Goal policy used the same transaction. This replaced the roster
  placement and separate skill-snapshot acknowledgment described by ADR 0008.
- **Complete-snapshot experiment:** a 216-request comparison found correct facts
  in 36/36 complete-snapshot reports versus 30/36 partial-update reports. The
  complete variant added 16.4% total input in the measured growing workload and
  more uncached input for three models. It was initially selected for that
  factual reliability despite Flash's three remaining repository-reporting
  failures. Sol and Pro each passed all nine partial-update reports. The detailed
  experiment remains local evidence under
  `.scratch/instruction-simplification/instruction-snapshot-measurements.md`;
  these numbers preserve its consequential tradeoff in the maintained record.
- **Return to independent sections, 2026-09-08:** commit `e31fe423` deliberately
  replaced the whole snapshot with separately acknowledged sections and an
  explicit statement that omitted sections remain applicable. It updated source,
  delivery tests, architecture, and reminders documentation, but left this ADR's
  complete-snapshot text stale. The change reduces repeated unrelated context
  while retaining actual-payload acknowledgment and context-loss recovery.
  Its recorded tests cover delivery mechanics; the commit supplies no new model
  comparison establishing that the earlier smaller-model reliability gap closed.
  That measurement limitation remains rather than treating complete snapshots as
  current behavior or inventing a capability threshold.
- **Repaired tool results:** a hidden audit split one result into two gptel tool
  runs, so the next request tried to read trailing `Note:` prose as a call plist.
  Removing same-tool metadata in prompt copies before computing their prepared
  digest restored complete serialization without deleting the durable audit.
- **Repeated tool names lost call identity:** the installed gptel renderer
  selected the first tool-use record matching a name. A metadata repair after
  insertion could not recover plain result IDs. The adapter now gives the normal
  renderer each authoritative call record separately, preserving order and IDs
  without a second history store. Original IDs in already-corrupted plain
  transcripts cannot generally be reconstructed. This rationale was recorded in
  the tool manual without a separate date.
- **Duplicate tool-availability reminder:** an ephemeral reminder repeated
  guidance already in native ToolSearch/ToolCall descriptions and changed the
  beginning of worker task history when it disappeared on follow-up. Removing
  it leaves static descriptions and actual search results as the capability
  discovery interface. The reminder manual recorded this without a date.
