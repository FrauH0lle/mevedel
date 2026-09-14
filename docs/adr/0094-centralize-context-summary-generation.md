# Centralize context-summary generation without centralizing workflows

Status: accepted

The instruction-delivery audit found that compaction supplied skill names and
invocation metadata but omitted the prepared bodies already recorded in live
session state. A capped tool result or changed source file could therefore
leave the generator without the relevant guidance. The selected records now
include the original prepared body and source/conversation attribution as
historical evidence. This neither invokes the skill nor proves it remains
active; the summary contract retains applicable obligations, corrections, and
retirement explicitly. Full bodies remain subject to the existing input-size
gate, and cold resume still relies on durable transcript/summary evidence,
because invocation records are not a persisted sidecar field.
The cold-restore check then exposed the same loss in persisted `Skill` results:
ordinary tool-output truncation removed trailing obligations even though the
restored transcript still contained them. The evidence projector now retains
complete `Skill` instruction results, still labelled as tool evidence, without
adding a second durable skill-state format. Ordinary tool output remains capped.

Mevedel uses one generator to turn frozen model-visible context into context-summary text, while each consuming workflow retains ownership of source selection, hooks, retries, persistence, injection, and source mutation. The generator supports continuation, handoff, and digest purposes through a shared, validated summary core; only continuation summaries carry actionable next steps, while handoff source material remains evidence beneath a separately supplied authoritative task. Only continuation generation treats an earlier continuation summary as authoritative retained context; handoff generation instead re-filters the parent's complete effective context, including any anchored summary, against the receiving task. Source transcripts are projected as one delimited evidence document with provenance labels rather than replayed provider roles, so user turns, assistant text, and tool results remain untrusted evidence instead of live summarizer instructions. Caller guidance may focus content but cannot override the purpose, structure, or authority contract. This shares prompt and model-request behavior without making non-mutating plan, worktree, or agent handoffs inherit conversation-compaction lifecycle semantics.

Journal digest generation needs the same frozen evidence, model policy,
admission, and callback settlement as continuation and handoff generation.
That shared behavior is the reason for adding a third purpose rather than a
second request implementation. The digest purpose has a separate factual
bullet-list prompt and four headings (Done, Learned, Surprised, Unfinished),
a 16 KiB output cap, and at most a 4,000-token output reserve during input
admission. Supported server token controls are capped on the realized provider
payload. It treats prior summaries as evidence and
never turns unfinished state into instructions. Capture jobs, source
projection, persistence, deadlines, and retry policy belong to the journal
consumer. Existing continuation and handoff contracts remain intact.

The first real-model long-transcript evaluation exhausted the configured
DeepSeek model's 4,000-token allowance in reasoning and returned no digest.
Prompt shortening alone reproduced that failure. Newly resolved digest policy
therefore defaults unspecified effort to `disabled`, or `none`, when the model
declares that capability; explicit effort and already frozen policies remain unchanged.
The journal freezes this choice before inference. The shared generator owns
this digest default alongside its output cap, using gptel's model-declared
effort choices and request implementation rather than provider-specific JSON.

The configured gptel branch also encoded DeepSeek's disabled effort as
`reasoning_effort: "disabled"`, which the service rejected. A separate isolated
dependency patch omits that field when `thinking.type` is `disabled`, following
[DeepSeek's API contract](https://api-docs.deepseek.com/api/create-chat-completion/).
The quality evaluation uses that patch; deployment with the affected gptel
branch requires the same dependency fix. mevedel adds no provider adapter or
compatibility advice for it.

The subsequent configured-model comparison recovered the buried lesson with
GPT-5.6 Sol, which the user selected provisionally. Its Codex OAuth transport
removes `max_output_tokens`; requiring that field rejected every digest before
dispatch despite retaining the same input reserve, streaming byte limit, and
120-second journal deadline. The requirement is bounded evidence admission and
bounded accepted output, not universal provider support for a token parameter.
Generation now accepts providers without that control, omits the unsupported
Codex setting, and still clamps every server limit that is present. Client
cancellation bounds local processing and accepted output; it cannot guarantee
a server-side token or billing ceiling. This limitation is explicit instead of
being hidden by a test-only admission exception. Model selection stays in the
ordinary configurable workload map, not in the journal implementation.

The September 2026 configuration review exposed unnecessary coupling: tuning
journal extraction also changed compaction and handoffs, while tuning memory
consolidation also changed Buddy. Shared request mechanics do not require shared
model selection. The generator now resolves `journal` for digests and
`summarization` for continuation/handoff; journal capture freezes `journal`
before dispatch. Consolidation resolves `memory` instead of `buddy`. Both new
workloads default to `balanced` and use the existing tier/provider/effort map.
Existing queued captures keep their frozen policy. The generator and workflow
ownership boundaries remain unchanged.
