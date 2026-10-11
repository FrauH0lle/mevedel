---
name: "Keep live-state inspection probes bounded and non-perturbing"
description: "Return scalars or booleans from live Emacs probes; large object dumps and profiling workload distort what you are measuring"
type: feedback
---

# Keep live-state inspection probes bounded and non-perturbing

Observed 2026-09-28: an Eval returning `mevedel--turn-settlements-pending` serialized **592,281,931 characters**; the follow-up `(not (null ...))` check returned a compact status.

**How to apply.**
- When inspecting live Emacs state, request a bounded scalar or an explicit boolean rather than a request/FSM object. A huge serialized payload is a probe-shape artifact, not evidence about product behavior.
- Do not read cost or latency from a session whose profiling was already active: in that same capture the inspection introduced diagnostic workload, so the measured cost included the probe. Prefer bounded probes when sampling under a profiler.
