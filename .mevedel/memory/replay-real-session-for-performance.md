---
name: "Reproduce performance with representative state and actual deferred work"
description: "Use isolated representative state, preserve the original, and count deferred work only when the real workflow requires it."
type: feedback
modified: 2026-11-15
---

# Reproduce performance with representative state and actual deferred work

**Rule:** a fast replay is not evidence about a stall unless it exercises the relevant session state and production paths. Restore an isolated copy of the affected session at the stalled state rather than assuming a transcript pasted into detached test buffers is representative.

**Why — observed 2026-10-03:** a detached-buffer replay omitted agent transcripts, archived segments, and published artifacts. It reported 5.9 ms updates while the real paths still cost about 0.36 seconds per streamed update. A replay from a restored session copy reached expensive paths the first fixture could not, including archive reads and expanded audit rendering. These are historical observations, not current performance guarantees.

**When and how to apply:**

- For a session-specific stall, preserve the relevant archived segments, agents, publications, expansion state, and in-progress turn in an isolated copy. Do not replay into the live original.
- Match loaded code, compilation state, dependencies, and necessary configuration before comparing timings. A faster interpreted or differently configured fixture is not a controlled comparison.
- Distinguish batch render cost from graphical redisplay and queued-input latency; choose the environment appropriate to the claim.
- Use a before/after comparison that actually reaches the costly path. A synthetic fixture remains useful for a narrow regression once its relationship to that path is established; restoring a full session is not mandatory for every performance test.

## Count deferred work without inventing redundant work

**Confirmed correction, recorded 2026-10-19:** a checkpoint benchmark initially forced an eventual root save even when the fallback implementation had already left the buffer clean. The parent caught the mismatch. The corrected comparison counted an eventual checkpoint only when the receiving buffer remained modified and drained observational saves only when actually pending. The earlier forced-save run was excluded from natural-workload evidence.

**Reusable lesson:** when an optimization moves work to a later checkpoint, flush, or callback, include the deferred work required to reach the same completed outcome. But do not force redundant work on a variant that has already reached that outcome merely to make operation counts look symmetric.

Record the completion condition and distinguish immediate-path improvement from end-to-end improvement. This applies to comparisons involving deferred work; it is not a requirement to add a checkpoint to every benchmark.

## Restoration can itself change the evidence

**Observed 2026-10-19:** batch maintenance of real sessions exposed unintended writes during restoration. Stub backend configuration changed persisted reasoning effort and chooser metadata; restoration also published repair state. A harness described as inspection was therefore capable of changing user state.

**Reusable lesson:** do not assume that opening or restoring persisted state is observational. Prefer isolated copies for investigations, preserve a baseline, and compare persisted content when checking that a workflow leaves state unchanged. Never treat a stub configuration as interchangeable with the user's configuration for operations that can republish it.

This is not standing permission to acquire leases, prune sessions, publish repairs, or alter live state. Authorized maintenance needs its own checks and current workflow.

Consult `docs/sessions.md` and the relevant source/history for current persistence, lease, restoration, and performance contracts. Historical harness paths, backend names, allowlisted metadata keys, and batch-pruning commands are not a maintained execution recipe.
