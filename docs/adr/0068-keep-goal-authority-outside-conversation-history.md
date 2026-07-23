# Keep Goal authority outside conversation history

Every Goal turn receives active Goal context regenerated from the durable Goal
record, while user steering remains ordinary conversation history. Compaction
may retain useful outcomes and facts but neither reconstructs Goal state nor
mechanically carries old steering forward. This repeats the Goal contract in
each root request, spending some context tokens to ensure that every completion
decision sees current authority and that compaction cannot resurrect stale
instructions.
