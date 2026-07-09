# Hook Audit Surfaces

Hook decisions that change model-visible context, submitted content, control
flow, or permissions need a user-facing hook audit surface attached to the
transcript artifact they affect: user turns, tool attempts/results, Agent
tool rows, child agent initial prompts, or compaction summaries.  Raw
command-hook output and diagnostic runner logs stay out of the view by
default, so hook authors must use structured decision fields when they want
user-visible or model-visible effects.  This keeps successful automation quiet
while preserving auditability at the point where the hook changed what the
model saw or what the workflow did.
