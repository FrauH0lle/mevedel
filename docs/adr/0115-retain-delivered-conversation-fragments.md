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

For gptel-controlled history, a section is acknowledged only by a complete trusted message actually present
in the realized outgoing payload, in payload order. Context loss, filtering, or
restore therefore causes missing current sections to be delivered again. A
rebuildable transcript index avoids repeated decoding. Agents receive only their
frozen selected components; authored inline components remain system content.
There is no independent persisted acknowledgment schema or runtime model-name
gate. See [retained instruction context](../architecture.md#retained-instruction-context).

External Claude mail uses two observable receipt boundaries. Mail submitted
with a prompt requires the exact complete mail block in the SDK's user-message
echo for the owning conversation. Mail delivered by a PostToolBatch hook requires
a successful SDK `hook_response` containing the exact complete output.
Until the corresponding receipt arrives, the mailbox remains unread. Each batch
has a unique correlation marker, is recorded once in the recipient's canonical
transcript, and consumes only the captured messages. Receipt establishes SDK
acceptance into conversation context, not model understanding or permanent
retention through compaction. Mailbox consumption keeps its existing deferred
persistence contract; process loss before saving can redeliver mail.

Submitted attachments use the same native user-message boundary. The echo must
contain the submitted text and every image's complete bytes, MIME type and order
before mention deduplication commits. Text receipts alone do not establish image
delivery. The shared mention resolver retains permission and artifact-byte
authority; independent directive and child inputs do not consume root dedup marks.

Selected gptel text context uses its existing collector and formatter. System
placement belongs to the native system baseline. User placement requires the
complete SDK prompt echo, and compaction reintroduces its formatted turn
snapshot through the same exact hook receipt used for other required guidance.
Asynchronous formatting remains inside admitted-turn cancellation, before
starting the native process.

Native compaction lifecycle events also maintain mevedel's effective transcript.
Completed root compaction publishes Claude's retained summary as a new segment,
preserving output received since its start as a recent tail. Retained children
use their private transcript archive. These summaries are Claude-authored
evidence and can refer to hook-injected context; segment publication does not
replace or acknowledge the separate restoration transaction. ACP notifications,
native hooks and tool admission wait for idle target transport in order, so
publication cannot nest inside an active target operation.

Root engine changes use that effective segment. Gptel reconstructs ordinary
roles and tools through its serializers, without foreign signed thinking.
Claude starts a new native conversation from labelled, projected evidence;
returning to it after a gptel dispatch cannot resume its stale external ID.
Known path-instruction scopes survive a transition, with nil acknowledgement
hashes until the new engine receives complete current contents. The persisted
path identity and the delivery acknowledgement have different lifetimes.

Explicit recovery of missing native history uses the same labelled evidence
projection. Detachment is persisted while the session is idle; it starts no
inference and cannot resume a paused Goal. A retained child uses its own
transcript and frozen configuration. Its unstarted reference survives until
the replacement native identity is published, so a failed startup does not
discard the excerpt on retry. Publication failure restores the old reference
and acknowledgement state in memory; uncertain publication retains the shared
recovery gate. Recovery does not replay historical tool effects.

Claude's inline hook output limit is checked in UTF-16 units. An oversized
whole message stays queued until the next prompt, which has no hook-string
limit and includes it in full. File offloading or a preview is not complete
delivery. Native compaction also uses exact hook receipts for changed selected
observations. The complete system baseline stays in Claude's system prompt;
restoration supplies current differences, which supersede its older sections.
System-prompt snapshotting is explicitly disabled so a resumed launch cannot
silently retain stale mevedel instructions. Missing restoration receipts block
subsequent tool effects and successful settlement. Ordinary PostToolBatch hooks
also deliver changed selected observations, comparing against the last accepted
snapshot. They carry queued turn events through the shared reminder owner;
receipt runs the captured commits and removes only those event objects, so a
newer event with the same key remains eligible. Required context must be
acknowledged before further effects or successful settlement. Oversized changes
stop the native prompt at its hook boundary. The admitted turn then sends a
continuation prompt containing the complete captured body, requiring its exact
SDK user receipt before more effects. This also covers oversized restoration
after compaction. SessionStart(compact) has no blocking control. A PreToolUse
hook therefore denies any call while a full restoration is waiting for its
continuation, and stops that native prompt. Denial and stopping are separate
hook decisions; a stop alone does not prevent the pending call. A denied
attempt already spent a child sample, so the next prompt reserves another
within the same cap. The native conversation, MCP endpoint, tool ledger and
transcript ownership stay live; final settlement and Goal turn accounting occur
once. Per-prompt usage contributes to a frozen cumulative base. User boundary
stops and cancellation take precedence; missing receipts still fail closed.

Configured reminders use one collector across engines. Claude collects once at
prompt submission and commits firing marks only on exact receipt, including
silent commits. The collector includes pending root hook context and preserves
later arrivals. Shared pending-event consumption removes only captured items.
Root recovery uses that same pending-event transaction, so it does not create a
second recovery notice. Direct-child rosters have one shared producer and are
restored in full after native compaction, alongside active root Plan guidance
and eligible accepted-plan references rendered by their existing producers.
The same restoration re-reads path instructions previously acknowledged by the
conversation, ordered broad to narrow with local overrides last. Removed files
explicitly withdraw old guidance. Fresh directives own request-local instruction
hashes; root acknowledgments neither suppress their discovery nor import unrelated
root scopes. Retained children continue to use their canonical path ownership.
Local transcript-based context-pressure estimates are not applied to external
model history.
Claude retained-agent sample warnings use that same acknowledged turn-event
transaction. Current sample-limit guidance, including the final-sample notice,
is also restored after native compaction through a shared reminder producer.

An uncertain external conversation carries recovery guidance through the same
exact user-message receipt boundary. Tool work and successful settlement wait
for that receipt. It records delivered guidance, not verified effects. Native
call identities are separately committed before execution and retained across
turns; replay rejection therefore does not depend on receiving the old tool
result or reconstructing an ID from rendered text.

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

- **Selected text context, 2026-10-06:** Collecting only media dropped selected
  files and buffer regions from native requests. Reusing gptel's formatter now
  preserves those selections and their system/user placement. Ordinary workflow
  tests cover asynchronous preparation, cancellation, duplicate callbacks,
  malformed output, frozen system prompt and acknowledged restoration before
  subsequent tools. A bounded live turn for each placement returned a marker
  supplied only through the selected file. This verifies use of that context;
  it does not turn a system-setting assignment into an exact SDK system receipt.

- **Native image input, 2026-10-06:** Native sends previously bypassed shared
  mention expansion, and temporary expansion buffers lost the selected model.
  Root, directive and child sends now reuse the shared resolver and send accepted
  images as ACP blocks. A text-only receipt cannot commit image delivery. The
  installed adapter's complete SDK image echo was verified in a bounded live
  color-identification turn; missing and altered echoes fail ordinary composer
  tests. PDF/blob input remains unadvertised because the inspected adapter does
  not convert those resources into native document blocks.

- **Scoped instructions and accepted plans, 2026-10-06:** Successful delivery
  before compaction did not ensure path instructions survived it. Restoration
  now re-reads each conversation's learned files, including changed or removed
  guidance, and uses the same exact receipt boundary. A live 22-Read turn
  compacted after 12 batches, received revised path instructions and used their
  new marker in its final answer. Workflow tests also exposed root hashes
  suppressing guidance in fresh directives; request-local hashes fix that scope
  error. Root, child and directive tests cover restoration ordering, isolation,
  missing receipts and oversized updates retried through the next prompt.
  Eligible accepted-plan references now restore through their existing producer
  as well; a missing receipt blocks subsequent tools. Automatic oversized-update
  continuation was not yet implemented at that stage.
  The separate-editor restart test also exposed local SessionStart discarding
  learned native scopes. Native owners now retain them across reopening and
  refresh current contents in each acknowledged resumed prompt; local-history
  owners keep their existing epoch reset behavior.

- **Shared roster and reminder owners, 2026-10-06:** The external path bypassed
  configured reminder collection and the gptel-only roster handler. Their
  producers now serve both engines, with commits deferred to each engine's
  delivery boundary. Normal workflow tests exercise initial and compacted
  rosters, large initial reminders, root/child isolation, Plan restoration,
  missing receipts, silent commits and late events. The existing recovery test
  caught duplicate guidance when configured reminders were enabled; routing
  root recovery through the shared pending-event owner removed the duplicate.

- **Ordinary native context updates, 2026-10-06:** Compaction-only restoration
  left a running Claude turn unaware of changed memory and path instructions
  queued by Read. The ordinary tool-batch hook now uses the shared observation
  renderer and turn-event queue. Workflow tests cover changed, unchanged,
  reverted and oversized observations, child selection, lost receipts and
  events replaced while a receipt is in flight. A bounded live Claude Code
  2.1.291 run accepted both a changed memory section and discovered path
  instructions in one exact hook receipt and used both markers in its answer.
  Compaction still compares against the system baseline, so acknowledgment
  before compaction does not incorrectly suppress restoration afterward.

- **External mail receipt, 2026-10-06:** ACP send completion alone did not prove
  that hook context reached the native conversation. The installed adapter
  exposes SDK hook lifecycle messages. A bounded live Claude Code 2.1.291 run
  emitted a successful receipt with exact output and the model used the mail's
  marker in its answer. Deterministic workflow cases reject foreign-session,
  malformed, mismatched and failed receipts, preserve late arrivals, and reject
  oversized emoji payloads without consuming them. The
  [native hook contract](https://code.claude.com/docs/en/hooks#json-output)
  confirms the per-string limit and preview replacement. This is evidence for
  bounded hook delivery, not for compaction retention or permanent delivery.

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

- **Initial mail receipt, 2026-10-06:** The hook-only path could not deliver a
  whole message larger than the native inline hook limit. Prompt submission now
  carries that mail as a separate text block, with consumption tied to its exact
  SDK echo. A bounded live run echoed all 12,223 characters and used the marker
  in the answer; root and retained-child tests also cover missing, altered,
  foreign and duplicate receipts. This extends delivery without treating a
  truncated hook preview as complete input.

- **Native compaction restoration, 2026-10-06:** SDK 0.3.287's systemPrompt
  contract documents a default first-prompt snapshot, including reuse on resume.
  The adapter forwards an explicit custom prompt with snapshot=false to preserve
  mevedel's current composed instructions. A live normal-session turn with more
  than 12,000 characters of system instructions compacted after 12 tool batches,
  accepted an exact SessionStart receipt carrying newly changed memory, and
  completed another 10 reads. Its answer used both the system marker and the
  marker introduced only after compaction. This replaces the previous absence
  of an in-turn restoration path; it does not establish full reminder coverage
  or automatic handling of oversized changed observations.

- **Interrupted external recovery, 2026-10-06:** Per-process duplicate-call
  tracking could not reject a replay after reopen, and uncertain history had no
  acknowledged recovery notice. Durable admission now precedes the pipeline,
  and normal root/child follow-ups require the SDK echo of recovery guidance.
  A real Bash mutation followed by peer death remains a single effect after
  reopen and duplicate-ID rejection. Live public abort exposed premature request
  release before the ACP cancellation reply; the existing settlement hold now
  covers that interval. The repeated live run saved, reopened and inspected the
  single write through Read, with two settled turns and no repeated mutation.

- **Native summary segments, 2026-10-06:** Hook restoration alone left the
  transcript with all pre-compaction history. That representation would revive
  archived raw turns when continuing through gptel. ACP compaction events now
  publish the actual retained summary using existing root/child archive paths.
  A busy-target regression showed publication could nest inside target I/O;
  ordered event/tool admission and asynchronous private hooks now wait for idle
  transport. A bounded live turn compacted after 12 of 22 Read batches, produced
  two segments and completed the remaining reads with the restored memory and
  path markers. This establishes native summary publication separately from
  the existing proof of context restoration.

- **Cross-engine continuation, 2026-10-06:** The user rejected the initial
  blanket history-switch guard. Effective native compaction segments and
  gptel's existing reasoning-safe projection provide a usable continuation
  boundary. Tests now exercise both directions through ordinary sends, including
  a request-only Claude policy returning to its saved API provider. Clearing
  path records would lose nested guidance after a failed startup; retaining
  their identities with unacknowledged hashes preserves a fresh delivery on
  retry or reopen.

- **Missing-history recovery, 2026-10-06:** Startup errors previously preserved
  the transcript but offered no explicit way to continue with Claude. The
  existing cross-engine excerpt projection supplies the necessary continuation
  semantics without a new summary model call. Root and retained-child workflow
  tests lose the native identity, observe the failure, explicitly detach it,
  and complete a fresh conversation with retained evidence. The root then
  resumes that replacement ID. Recovery is a user control, never an automatic
  assumption that missing history or uncertain effects can be retried safely.

- **Oversized context continuation, 2026-10-06:** The first native integration
  failed explicitly above the10k UTF-16 hook limit and needed another user turn.
  Keeping the ACP conversation idle after its acknowledged prompt result lets
  the same admitted turn submit the complete context in another prompt. A
  bounded live two-Read task stopped at its first hook, accepted the oversized
  update, completed the second Read and returned the marker at the end of that
  update in6.4s. Deterministic tests cover ordinary changes, compaction/path
  restoration, missing receipts, pause/cancel, delayed usage frames and child
  sample caps. No preview, offloaded reference or mere transmission counts as
  receipt, and a failed native prompt is not automatically retried.
  A real compaction run exposed the initial peer's incorrect assumption that
  SessionStart honors a stop. The CLI continued and the MCP admission guard
  correctly failed closed. PreToolUse also needed an explicit deny alongside
  its stop. With both decisions, a live 22-Read chain completed in76.2s across
  compactions after batches12 and21, two automatic continuations and three
  transcript segments. The final answer included the restored path and memory
  markers beyond the hook limit. The peer now models these observed semantics.
  Retained-child tests additionally caught a skipped sample charge at this
  boundary; a two-sample cap now stops after the denied attempt, while a
  three-sample cap permits exactly one restored sample and delivers its warning.
  The same reservation applies when the post-compaction sample returns text
  without a tool call; it cannot bypass the cap through automatic continuation.
