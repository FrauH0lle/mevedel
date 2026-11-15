---
name: "Verify graphical results in an isolated, presenting environment"
description: "Match graphical evidence to the claim; keep module loading and evaluation out of the user's running Emacs."
type: feedback
---

# Verify graphical results in an isolated, presenting environment

## Match verification to the claim without replacing live modules

Verify Emacs view presentation in an isolated graphical Emacs before reporting a visual result. Batch tests and CSS mockups do not establish how Emacs renders faces.

**User prohibition, preserved in the November 13 journal from the implement-spec prompt:** “Never load or evaluate mevedel modules in my running Emacs; test in separate Emacs processes.” In particular, do not load worktree modules into the user's running Emacs for graphical checks: that would replace modules used by other sessions. Isolation is not a preliminary step that makes later live replacement permissible.

The user explicitly asked “Can you load the code into Emacs?” in turn 248. That was a one-off request, not a standing relaxation of the prohibition or authorization for future live loading. Use separate Emacs processes for subsequent testing; do not infer permission from that historical exception.

During turn-separator work on 2026-10-19, mockups and batch tests missed that tool rows inherited `default`'s background and that strike-through on an extended newline reached the window edge. These were observed rendering differences, not user-selected styling rules.

Consult the graphical-check section of `docs/development.md` for the maintained worktree-rendering workflow and fixture extension guidance. The captured correction records that this tooling superseded the hand-built recipe on 2026-10-24 (`mevedel@982d9811`).

**Historical fixture correction, 2026-10-24:** copying `font-lock-face` to `face` was a workaround, not the preferred setup. Missing fontification was attributed to fixture buffer names beginning with a space; rename such fixtures rather than retaining the face-copy recipe.

Static captures can establish appearance but do not establish hover behavior, frame timing, or sustained performance. Use checks appropriate to each claim. Keep live inspection bounded and non-perturbing; an earlier row-property dump returned 20 MB without being necessary to establish the visual result. This does not authorize module evaluation or instrumentation in the live editor.

## Preserve realistic chrome in native geometry fixtures

An Emacs 31.1 viewport investigation recorded 2026-11-10 found that a header-free native fixture moved only the viewport, while a corresponding fixture with a static header reproduced viewport-first point relocation. The header-free result had masked the mechanism rather than establishing its absence.

**Reusable inference:** for native scrolling, point-preservation, and redisplay claims, include relevant header/tab chrome and body geometry in the isolated fixture. Check both viewport and point across redisplay; point movement may follow viewport movement rather than initiate it. Simplify a fixture only after establishing that the removed chrome does not control the behavior being tested.

**Attribution limit:** this experiment demonstrated a native mechanism under an artificial stimulus. It did not establish that the mechanism caused the archived live composer exits or identify the origin of their pending state. A matching isolated mechanism is not a historical event trace.

## Protocol responsiveness is not completed presentation

Recorded 2026-10-12: locking the desktop and powering down its outputs stopped KWin frame callbacks, making a pgtk workload appear almost CPU-free because it was no longer presenting. An overnight comparison lost useful measurements to desktop locking. Fresh-HOME native compilation also introduced subprocess activity and redisplays that contaminated the initial workload.

Controlled observations, 2026-10-30: under confinement, a minimized stock `emacs -Q` reproduction mapped a frame, acknowledged configuration, and completed Wayland sync, yet its requested frame callback never completed; KWin reported `Found no render device!`. The corresponding reproduction outside confinement completed 28/28 callbacks. A subsequent confined suite passed after the required rendering device was made available.

**Reusable inference:** before attributing a graphical failure to rendering code or interpreting low CPU as an optimization, establish that the compositor is actually presenting. A socket connection, mapped frame, configure acknowledgement, or completed sync roundtrip alone is insufficient.

## Apply presentation and workload controls when measuring

- Check lock/output state and actual presentation before interpreting CPU numbers. Recorded probes included `loginctl show-session <id> -p LockedHint` and `/sys/class/drm/*/dpms`; verify their applicability on the current setup.
- Check completed frame callbacks or other presentation evidence appropriate to the claim.
- For unattended measurements, use a nested virtual compositor isolated from the desktop lock. The recorded KWin setup used `kwin_wayland --virtual --no-lockscreen`; without `--no-lockscreen`, the nested compositor inherited locking and stopped frame callbacks too. Verify current command support.
- Warm the fixture and let native compilation settle before collecting data.
- Account for Emacs, the nested compositor, and any Xwayland process separately. Do not assume a particular X display number or conflate the desktop compositor with the nested one.

These observations do not prescribe a blanket device grant or establish that every missing callback has the same cause. Keep confinement and descriptor-lifetime implementation details with their maintained code and documentation.

Earlier measurement source: Claude project memory `presentation-measurement-harness.md` under `/home/roland/.claude/projects/-home-roland-Projekte-mevedel/memory/`, curated for import at the user's request on 2026-10-19. Historical timings and machine settings are not current guarantees.
