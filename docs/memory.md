# Persistent Memory

mevedel reads persistent memory from configured `.mevedel/memory/` and
`.agents/memory/` roots, both workspace-local and user-global. Memory is
model-writable and persists across conversations, but it is
selective by default: optional saves should preserve durable context useful to
future work. Explicit user requests to save, forget, or ignore memory take
precedence over package preferences about what is worth retaining.


Curated topics hold reusable knowledge; journal digests hold dated evidence
supporting that knowledge. Use `/remember` to review memory and journal evidence, then
inspect the proposed changes in the [memory cockpit](memory-review.md#memory-cockpit).
The `$learn` skill supports explicitly requested write-back during a conversation.


## Contracts by subsystem

- [Curated memory](memory-curated.md) — root selection, topic/index format,
  save/forget policy, prompt delivery and staleness. Read this part before
  changing memory or its index.
- [Review contract](memory-review.md) — on-demand review, model selection,
  evidence and proposals, bounded requests, pass ownership, automatic scheduling
  and the memory cockpit.
- [Decisions, application and recovery](memory-recovery.md) — accept/reject,
  exact checked writes, child bookkeeping, interrupted-write recovery and reversal.
- [Shared working-note lifecycle](memory-notes.md) — authoring, readiness,
  extraction dependencies, permanent disposal and attachment housekeeping.
- [Journal](memory-journal.md) — completed-turn capture, digest generation,
  discovery, abandoned checkpoints, inspection and retention.
