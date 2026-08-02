# 08 — Preserve directives when their source disappears

**What to build:** Keep directives usable when their source file is deleted or unavailable. Users can reach them from workspace activity, safely reattach them when source returns, and archive historical directives without invalidating their activity or recovery links.

**Blocked by:** 07 — Detach directives without losing activity

**Status:** resolved

- [x] Deleting a directive’s source file changes its anchor to Source missing without discarding the directive, activity, patch, or checkpoint links.
- [x] Killing a buffer for a missing file does not silently remove persisted directive records.
- [x] Source-missing directives remain inspectable and actionable from the workspace activity list.
- [x] Single and batch action eligibility uses the same prompt-context validation rather than a semantic guess about whether the missing source matters.
- [x] When source returns, one exact unambiguous match reattaches automatically using retained anchor evidence.
- [x] Missing or ambiguous matches leave the directive Source missing and offer explicit reattachment rather than choosing a location heuristically.
- [x] Explicit reattachment updates the durable anchor and recreates the compact attached presentation without changing directive identity.
- [x] A directive with activity can be Archived, hiding it from source and the active workspace list while retaining inspectable activity and checkpoint links.
- [x] A directive with activity cannot be permanently deleted through the ordinary remove action; a directive without activity remains removable.
- [x] Restoring a workspace preserves Source missing and Archived directives without requiring their source buffers to exist.
- [x] Tests cover file deletion, buffer kill, workspace restore, exact and ambiguous reattachment, explicit reattachment, Archive, and removal eligibility.

## Answer

Implemented in `7ceb57b`.
