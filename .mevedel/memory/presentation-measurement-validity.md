---
name: "Keep graphical presentation measurements valid"
description: "Locking and powered-down outputs can suppress presentation; isolate the compositor and warm the fixture"
type: feedback
---

Recorded 2026-10-12: locking the desktop and powering down its outputs stopped
KWin frame callbacks, making a pgtk workload appear almost CPU-free because
it was no longer presenting. Low CPU without active presentation is not
evidence of an optimization.

**Why:** An overnight comparison lost useful measurements to desktop locking.
Fresh-HOME native compilation also introduced subprocess activity and
redisplays that contaminated the initial workload.

**How to apply:**
- Check lock/output state and actual presentation before interpreting CPU
  numbers. Recorded probes included `loginctl show-session <id> -p LockedHint`
  and `/sys/class/drm/*/dpms`; verify their applicability on the current setup.
- For unattended measurements, use a nested virtual compositor isolated from
  the desktop lock. The recorded KWin setup used `kwin_wayland --virtual
  --no-lockscreen`; without `--no-lockscreen`, the nested compositor inherited
  locking and stopped frame callbacks too. Verify current command support.
- Warm the fixture and let native compilation settle before collecting data.
- Account for Emacs, the nested compositor, and any Xwayland process
  separately. Do not assume a particular X display number or conflate the
  desktop compositor with the nested one.

**Source:** Claude project memory `presentation-measurement-harness.md` under
`/home/roland/.claude/projects/-home-roland-Projekte-mevedel/memory/`;
curated for import at the user's request on 2026-10-19. Historical timings
and machine settings were not promoted to current guarantees.
