# Scope sandbox policy to the session

Status: accepted

Sandbox mode is a persisted session setting, initially copied from a global
default for new sessions. Confinement policy expresses risk tolerance for one
workflow rather than a host-wide fact, so concurrent sessions may independently
choose `best-effort`, `required`, or `off`.

The stored setting is a preference. One effective policy maps Edits to `required`,
Full Access to `off`, and Ask to that preference. Child launches, native helpers,
remote readiness and pending-review facts use the effective value; switching
permission modes does not mutate the preference. Fork/resume preserves both
session settings. Queued children cannot launch under an obsolete captured mode.

## Decision history

Previously sandbox preference was independent of permission mode. The accepted
September 2026 mode contract requires real confinement for automatic Edits and
no confinement in Full Access. Deriving policy centrally avoids contradictory
native, child and remote execution behavior.
