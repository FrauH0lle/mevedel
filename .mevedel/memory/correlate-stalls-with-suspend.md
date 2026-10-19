---
name: "Correlate apparent stalls with suspend and resume"
description: "Check system sleep windows before attributing telemetry gaps or post-resume connection errors to mevedel"
type: feedback
---

Recorded 2026-09-04: two desktop suspend windows explained the longest gaps
in a supposedly stalled mevedel session and coincided with a request error.
Telemetry alone made these look like application hangs.

**Why:** A sleeping host and a stalled application can produce similar gaps;
network recovery after resume can also interrupt an in-flight stream.

**How to apply:** When standby is reported or suspected, correlate the affected
time range with system sleep records before attributing it to mevedel. On
this Linux setup, the investigation used `journalctl` filtered for
`PM: suspend entry` and `returned from sleep`. Check the relevant boot,
timestamps, and available messages on the current system. Establish the clock
source and suspend semantics before interpreting elapsed/wall-clock deltas.
Post-resume DNS or connection failures are clues, not proof of suspend; verify
current transport and retry behavior rather than importing the old curl-code
and manual-retry observations as rules.

**Source:** Claude project memory `standby-gaps-journalctl.md` under
`/home/roland/.claude/projects/-home-roland-Projekte-mevedel/memory/`;
curated for import at the user's request on 2026-10-19.
