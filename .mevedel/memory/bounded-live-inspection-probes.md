---
name: "Keep live inspection non-perturbing and diagnostics representative"
description: "Bound probe overhead, preserve live-editor restrictions, distinguish navigation and observer effects, and persist intermittent evidence."
type: feedback
---

# Keep live inspection non-perturbing and diagnostics representative

## Bound probe payload and overhead

Observed 2026-09-28: an Eval returning `mevedel--turn-settlements-pending` serialized **592,281,931 characters**; a follow-up boolean check returned a compact status.

- Request a bounded scalar or explicit boolean rather than a request/FSM object. A huge serialized payload is a probe-shape artifact, not evidence about product behavior.
- Do not infer ordinary cost or latency from a session whose profiling or inspection adds workload.
- **Keep instrumentation out of the user's live Emacs: no `advice-add` timing wrappers and no `profiler-start`.** Use a batch or test Emacs instead. On 2026-10-24, a live Emacs OOM kill followed extensive advice, profiling, and concurrent agent checks; the exact cause was not established.

Further November 12 journal evidence records live-Emacs OOM kills with roughly 58 GB resident memory. Retained agent conversations were suggested as a cause, but that attribution remains unconfirmed. Neither temporal proximity to a probe nor recurrence establishes a cause, and these observations do not relax the live-editor restrictions.

## Match observation duration to intermittency

**User correction, 2026-11-05:** “2 min is not that long, especially considering how random this behavior is.” This concerned intermittent cursor jumps.

Choose an observation window appropriate to the reported frequency. Bounding each probe's output and overhead does not require an arbitrarily short window; a short capture without an occurrence does not establish absence.

The correction supplies no universal minimum duration and does not require indefinite recording. Longer observation does not authorize instrumentation in the user's live Emacs.

## Normal-use diagnostics must tolerate normal use

**User correction, 2026-11-05:** “Probably, you said I should use Emacs normally.” The assistant had asked whether navigation was intentional after offering a diagnostic for ordinary editor use.

Ordinary navigation must not consume the capture and then require the user to justify it. Distinguish recognized navigation from unexpected movement while retaining sensitivity after navigation ends and in other views.

November 12 recorder evidence reinforces the distinction: one apparent composer exit followed `mwheel-scroll`; later captures recorded input-event deltas and the last command, and their exits coincided with user navigation. Those signals help distinguish input-associated movement from movement without recorded input. They do not rule out background movement coinciding with a gesture.

Earlier isolated checks exercised real command hooks with simulated command bodies, not native mouse-event delivery. A navigation exception is not causal attribution.

A deliberately controlled reproduction may constrain user actions if stated explicitly; it must not be presented as unattended normal-use observation. Neither kind of diagnostic overrides the live-editor restrictions above.

## Persist intermittent evidence when the event occurs

Observed in the November 12 cursor investigation: an unsaved diagnostic ring was lost on restart, leaving overnight exits unattributed. Later monitoring saved event-specific recorder and guard data, although saves could time out when Emacs was busy.

**Reusable inference:** where diagnostic capture is authorized, persist bounded evidence at each relevant event rather than relying on a later dump of an in-memory ring. Record capture failures as missing evidence, not clean results. This is an evidence-retention lesson, not permission to install live instrumentation.

## Separate observer activity from the measured workload

November 12 evidence exposed an invalid redisplay probe: its hook expected a list but received a window and signalled errors repeatedly. Its reported zero count was not a valid measurement. Confirm the callback contract and that the probe itself executes successfully before interpreting counts.

The same investigation initially blamed a shimmer spinner without checking whether its native animation path was active. That path was subsequently observed active while redisplays continued, so the initial attribution was unsupported.

**Reusable inference:** use a discriminating check before naming a cause. For an idle measurement, exclude the measuring agent's own streaming/tool activity and ordinary user activity from the sample; a delayed quiet-window sample may help where authorized. Do not call an active-session sample “idle,” or treat an unexplained redraw count as proof against a particular component.
