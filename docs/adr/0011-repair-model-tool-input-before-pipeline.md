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
model-specific branches. `path` is a mevedel-internal semantic
schema type lowered to JSON string for providers.

Successful repairs run without a retry and add transparent model feedback.
Incomplete candidates are abandoned atomically. Every raw call emits redacted
session telemetry, while affected transcript rows reuse the hidden hook-audit
side channel. Neither surface stores argument values. These diagnostics are
best-effort and must never block a validated tool call.

The catalogue also clamps numeric values to declared `:minimum`/`:maximum`
bounds. Conditional bounds, such as WriteStdin's input-versus-poll range, remain
handler policy with requested-versus-effective telemetry.

## Decision history

The tool-owned repair callback had no production declarations but required a
second phase, audit validation, and cross-phase cycle tracking. The bounded
catalogue replaced it; tool-specific relational repair is not part of the current
interface.

On 2026-08-23 numeric clamping joined the catalogue because Bash `yield_time_ms`
and WaitAgent `timeout_ms` silently normalized out-of-range input without repair
feedback or telemetry. Deterministic declared bounds replaced that invisible
normalization at raw model admission. JSON parsing permits intermediate range
issues so the later clamp can finish the same bounded pass. WriteStdin exposes
the union of its ranges while retaining its argument-dependent handler rule.
