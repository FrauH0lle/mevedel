# Compact agent conversations independently

Status: accepted

Each retained agent conversation uses the same compaction policy and machinery as the root session but owns independent token accounting, anchored summaries, and live tail. Its canonical
transcript path stays fixed; numbered compaction siblings provide recovery
archives rather than new transcript identities. Parent compaction does not mutate existing child conversations, and child compaction does not alter its parent or siblings. Agent save/resume preserves that per-conversation compaction state.

The original task anchor is not sufficient for a retained agent: a later
assignment can supersede it, and a generated summary can omit that assignment.
Compaction therefore carries the latest direct follow-up task or immediate-parent
MAIL verbatim outside the summary, unless the preserved tail or pending input
already includes it. Selection uses canonical transcript spans after the child's
own task marker, excluding inherited history, peer mail, quoted controls, and
queued-mail wake prompts. Reusing that transcript representation keeps the
assignment available across repeated compactions and save/resume without a
second persisted source of truth. Native compaction applies the same local
transcript rule; the external provider continues to own its history.
