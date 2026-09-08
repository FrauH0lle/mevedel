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

Mevedel uses one generator and `summarization` model workload to turn frozen model-visible context into context-summary text, while each consuming workflow retains ownership of source selection, hooks, retries, persistence, injection, and source mutation. The generator supports continuation and handoff purposes through a shared, validated summary core; only continuation summaries carry actionable next steps, while handoff source material remains evidence beneath a separately supplied authoritative task. Only continuation generation treats an earlier continuation summary as authoritative retained context; handoff generation instead re-filters the parent's complete effective context, including any anchored summary, against the receiving task. Source transcripts are projected as one delimited evidence document with provenance labels rather than replayed provider roles, so user turns, assistant text, and tool results remain untrusted evidence instead of live summarizer instructions. Caller guidance may focus content but cannot override the purpose, structure, or authority contract. This shares prompt and model-request behavior without making non-mutating plan, worktree, or agent handoffs inherit conversation-compaction lifecycle semantics.
