# Name fail-open confinement best-effort

Status: accepted

Sandbox modes are named `best-effort`, `required`, and `off` everywhere, with
no aliases or legacy persisted values. `Best-effort` states that unavailable
confinement may fall back to direct execution, whereas `auto` obscured this
security-relevant fail-open behavior.

In `off`, execution is already direct and unrestricted, so additive network
and full-escalation requests change no authority and create no prompt. Exact
filesystem resource authorization remains independent of sandbox mode.
