# Keep turn authority in mevedel across model engines

Claude subscription access runs the installed Claude agent through ACP, with
mevedel's tools exposed through a private asynchronous MCP bridge. Mevedel owns
request admission, execution authority, effects, canonical transcript and
settlement; the external agent owns sampling, native model history and
compaction. The gptel engine retains its real state machine, while external
turns carry workflow context on the admitted request rather than manufacturing
a gptel state machine.

This boundary preserves the existing session workflow and the tested tool
pipeline while using the supported subscription login. Treating Claude as an
HTTP backend would misrepresent its retained history and native tool loop.
Text-emulated tools would replace that loop and add another model-facing
protocol. Direct stream-json would couple session communication to one CLI;
ACP provides reusable session transport, with Claude-specific prompt, tool
isolation, authentication and receipt extensions kept in its engine modules.
Supporting ACP does not promise that arbitrary agents provide those controls.

The bridge resolves every call against current mevedel authority. Discovery is
not permission, and a request's cancellation revokes pending interactions and
effects. Native call identities are persisted before tool execution so process
failure cannot replay an admitted effect. Root, directive and child histories
remain separate; directive requests reconstruct only their selected context.
The existing root interaction and agent registry decisions remain in
[ADR 0060](0060-centralize-agent-interactions-in-the-root-session.md) and
[ADR 0063](0063-persist-the-agent-registry-explicitly.md).

Acknowledged typed reminders use the shared hidden injection record, including
restored guidance after compaction. Saving only their wire text lost source
labels and made initial guidance appear under the assistant. The shared record
preserves source types and lets the normal renderer place initial deliveries
with the user prompt and later deliveries at their point in assistant activity.

External history changes what the transcript can faithfully reconstruct.
Claude compaction publishes a successor transcript segment; selected context
must be acknowledged again before subsequent tools. Cross-engine continuation
uses the effective segment and labels a new native conversation as excerpt
continuation. Same-machine resume retains native identity. Operations requiring
an exact native checkpoint mapping, including Fork and Rewind, refuse before
effects when that mapping is unavailable. These constraints are documented in
[session lifecycle](../sessions.md#external-conversation-references), rather than hidden
behind a transcript copy that claims exact model-history equivalence.

Raw transcript edits mark the owning root or child native history divergent;
continuation requires explicit excerpt recovery. Recovery reads the effective
summary from its canonical root or child bounds and includes it once as labelled
compaction evidence. Ordinary transcript segmentation omits leading root
summaries for rendering, and edits can inherit the closing wrapper's ignored
property; neither determines which summary the recovered model receives.

Connection ownership includes the protocol library's deferred drains and
watchdogs. A connection-local filter retains the library's newly scheduled
continuations across ordinary TRAMP waits; those callbacks retain their
successors too. Watchdogs validate their captured identity before closing the
connection. An interrupted request still consumes its owned terminal
acknowledgement for usage and settlement while discarding further work.
Retained native invocations use the same bounded acknowledgement; their runtime
canceller defers terminal settlement until the connection completes it. Queued
hooks and tool calls receive failure replies after cancellation, allowing a
peer waiting on those calls to reach its terminal reply without executing them.

The choice was validated through real ACP/MCP permission waits, reviewed
patches, compaction and context restoration, independent conversations and
same-machine restart, plus deterministic session and failure tests. The bounded
live runs used an existing Enterprise subscription login through the same
supported login path; they do not establish Pro/Max-specific allowance or
performance claims. Detailed run evidence belongs in the working-material
handoff, not in this ownership contract.

## Decision history

The initial callback queue protected work after ACP notifications were decoded.
Deterministic subprocess tests then showed that an ordinary TRAMP wait could
discard ACP's earlier drain timer, leaving its queue permanently busy, and
restore a cancelled startup watchdog that closed an admitted conversation.
Transport ownership now begins at the connection's process filter and follows
its scheduled callbacks. A separate admitted-turn regression showed that
cancellation discarded final usage already acknowledged by the native agent;
terminal accounting therefore survives cancellation under the same owner,
without reopening tool or continuation authority.
