# Repair model tool input before pipeline execution

Mevedel repairs only raw model-produced tool arguments at gptel's pre-tool-call
seam, using deterministic schema-directed rules, and commits changes only
after the complete input validates. The existing tool pipeline remains the
final validation and permission gate, so
hooks, permissions, snapshots, and handlers all observe the same repaired
arguments; hook rewrites and direct programmatic calls remain validation-only.
This preserves raw argument distinctions needed for safe repair without a
global preprocessing pass that could rewrite already-valid content.

Generic repair is a bounded, ordered catalogue rather than a growing set of
model-specific branches. The previously accepted tool-owned callback had no
production declarations, while it required a second repair phase, callback
audit validation, and cross-phase cycle tracking. It is removed; add a
concrete tool-specific repair only when a production relational invariant
cannot be represented by the catalogue. `path` is a mevedel-internal semantic
schema type lowered to JSON string for providers.

Successful repairs run without a retry and add transparent model feedback.
Incomplete candidates are abandoned atomically. Every raw call emits redacted
session telemetry, while affected transcript rows reuse the hidden hook-audit
side channel. Neither surface stores argument values. These diagnostics are
best-effort and must never block a validated tool call.
