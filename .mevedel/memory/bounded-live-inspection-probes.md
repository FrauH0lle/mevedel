---
name: "Keep live inspection non-perturbing and normal-use diagnostics representative"
description: "Bound probe overhead, preserve live-editor restrictions, and let normal-use diagnostics tolerate expected navigation without losing later sensitivity."
type: feedback
---

# Keep live inspection non-perturbing and normal-use diagnostics representative

## Bound probe payload and overhead

Observed 2026-09-28: an Eval returning `mevedel--turn-settlements-pending` serialized **592,281,931 characters**; the follow-up `(not (null ...))` check returned a compact status.

- When inspecting live Emacs state, request a bounded scalar or an explicit boolean rather than a request/FSM object. A huge serialized payload is a probe-shape artifact, not evidence about product behavior.
- Do not read cost or latency from a session whose profiling was already active: in that same capture the inspection introduced diagnostic workload, so the measured cost included the probe. Prefer bounded probes when sampling under a profiler.
- Keep instrumentation out of the user's live Emacs: no `advice-add` timing wrappers and no `profiler-start`. Do that in a batch or test Emacs. On 2026-10-24 the live Emacs (started 18:43) was OOM-killed at 19:03 with 28.5 GB anon RSS. Within the minutes before, an agent had installed `:around` advice on 335 functions, another had started the CPU profiler, and seven agents ran parallel checks. The exact cause is unproven, and it is the only OOM kill in the journal since 2026-05-13.

## Match observation duration to intermittency

**User correction, 2026-11-05:** “2 min is not that long, especially considering how random this behavior is.” This concerned the intermittent cursor-jump investigation; a two-minute diagnostic capture was too short for that incident.

When choosing an observation window for intermittent behavior, account for its reported frequency rather than assuming a brief capture is representative. Bounding individual probe output and overhead does not require an arbitrarily short observation window. A short capture without an occurrence does not establish that the behavior is absent.

The correction supplies no universal minimum duration and does not require indefinite recording. Longer observation does not authorize installing instrumentation in the user's live Emacs or relaxing the restrictions above.

## Diagnostics offered for normal use must tolerate normal use

**User correction, 2026-11-05:** “Probably, you said I should use Emacs normally.” The assistant had asked whether navigation was intentional after offering an unattended diagnostic for ordinary editor use, and acknowledged the diagnostic flaw.

When a diagnostic is intended to run during normal editor use, ordinary navigation must not consume the capture and then require the user to justify that activity. Distinguish recognized navigation from unexpected movement, while retaining sensitivity after navigation ends and to movement in other views.

The reported isolated batch and private graphical checks exercised real command hooks with simulated command bodies. They supported the tested navigation-aware behavior, not native mouse-event delivery. Background movement coinciding with an intentional gesture can remain indistinguishable from that gesture: a navigation exception is not causal attribution.

This rule applies to diagnostics offered for normal use. A deliberately controlled reproduction may constrain user actions when that constraint is stated explicitly; it must not be presented as unattended normal-use observation. Neither kind of diagnostic grants permission to instrument the live editor contrary to the restrictions above.
