---
name: "Keep graphical presentation measurements valid"
description: "Confirm completed presentation, not merely protocol responsiveness, and isolate and warm graphical workloads."
type: feedback
---

# Keep graphical presentation measurements valid

Recorded 2026-10-12: locking the desktop and powering down its outputs stopped KWin frame callbacks, making a pgtk workload appear almost CPU-free because it was no longer presenting. Low CPU without active presentation is not evidence of an optimization.

**Why:** an overnight comparison lost useful measurements to desktop locking. Fresh-HOME native compilation also introduced subprocess activity and redisplays that contaminated the initial workload.

Controlled observations, 2026-10-30: under confinement, a minimized stock `emacs -Q` reproduction mapped a frame, acknowledged configuration, and completed Wayland sync, yet its requested frame callback never completed; KWin reported `Found no render device!`. The corresponding reproduction outside confinement completed 28/28 callbacks. A subsequent confined suite passed after the required rendering device was made available.

**Reusable lesson (model inference supported by those observations):** protocol responsiveness does not establish completed presentation. Before attributing a graphical failure to rendering code or interpreting low CPU as a gain, establish that the compositor is actually presenting.

**How to apply:**

- Check lock/output state and actual presentation before interpreting CPU numbers. Recorded probes included `loginctl show-session <id> -p LockedHint` and `/sys/class/drm/*/dpms`; verify their applicability on the current setup.
- Do not use a compositor socket, mapped frame, configure acknowledgement, or completed sync roundtrip alone as proof of presentation readiness. Check completed frame callbacks or other evidence appropriate to the visual claim.
- For unattended measurements, use a nested virtual compositor isolated from the desktop lock. The recorded KWin setup used `kwin_wayland --virtual --no-lockscreen`; without `--no-lockscreen`, the nested compositor inherited locking and stopped frame callbacks too. Verify current command support.
- Warm the fixture and let native compilation settle before collecting data.
- Account for Emacs, the nested compositor, and any Xwayland process separately. Do not assume a particular X display number or conflate the desktop compositor with the nested one.

These observations do not prescribe a blanket device grant or establish that every missing callback has the same cause. Keep confinement and descriptor-lifetime implementation details with their maintained code and documentation.

**Earlier source:** Claude project memory `presentation-measurement-harness.md` under `/home/roland/.claude/projects/-home-roland-Projekte-mevedel/memory/`, curated for import at the user's request on 2026-10-19. Historical timings and machine settings are not current guarantees.
