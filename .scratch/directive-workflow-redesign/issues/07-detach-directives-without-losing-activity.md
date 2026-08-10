# 07 — Detach directives without losing activity

**What to build:** Preserve a directive and all of its activity when its entire source region is deleted. The source becomes a compact detached anchor at the deletion position, including a stable independent row when several directives detach together.

**Blocked by:** 04 — Capture an isolated implementation attempt

**Status:** resolved

- [x] Deleting an entire directive region changes its anchor from Attached to Detached instead of deleting the directive record.
- [x] The detached presentation uses a zero-width anchor and inserts no source-buffer text.
- [x] A detached row shows a concise anchor/state label, shortened request, and access to the directive’s actions.
- [x] Multiple directives detached at the same position render as one stable source-ordered block with independently actionable rows.
- [x] Detached rows are initially unfolded and add no grouping or garbage-collection controls.
- [x] Partial region edits retain and resize the Attached overlay through normal boundary behavior without introducing a heuristic stale state.
- [x] Detachment during an in-flight request keeps the directive discoverable and allows the attempt to settle normally.
- [x] Saving and restoring retains Detached state, former source order, position evidence, activity, and actions.
- [x] Attached overlays continue to show only tint and short lifecycle/outcome labels.
- [x] References retain their existing evaporating behavior when their complete region is deleted.
- [x] Tests use real buffer edits and cover Ready and attempted directives, in-flight deletion, partial edits, co-located rows, persistence, and unchanged reference behavior.

## Answer

Implemented in `4fcbf39`.
