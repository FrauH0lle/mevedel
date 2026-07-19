# Limit active agent turns per session tree

Status: accepted

Mevedel will enforce one shared capacity across all active non-root agent turns in a root session tree, regardless of nesting depth. The capacity defaults to three, is stored as a session setting derived from the global default, and cannot be overridden by descendants. A turn holds its slot from dispatch through settlement, including while suspended in `WaitAgent`; completion, error, or interruption releases it, while durable identities and settled turns consume no slot. `Agent` and a follow-up that must start an idle target fail immediately with a capacity-exhausted tool error when full; a follow-up to an already running target and queue-only messages remain deliverable. V2 imposes no separate delegation-depth or total-agent limit; automatic admission queues, an unlimited mode, runtime-residency LRU, and accumulation limits are omitted until demonstrated need justifies their lifecycle complexity.
