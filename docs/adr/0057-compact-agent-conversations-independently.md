# Compact agent conversations independently

Status: accepted

Each retained agent conversation uses the same compaction policy and machinery as the root session but owns independent token accounting, anchored summaries, and live tail. Its canonical
transcript path stays fixed; numbered compaction siblings provide recovery
archives rather than new transcript identities. Parent compaction does not mutate existing child conversations, and child compaction does not alter its parent or siblings. Agent save/resume preserves that per-conversation compaction state.
