# Plan 06: Evaluate and migrate live-tail/tool rendering to section updates

## Goal

After lower-risk zones are fragment-backed, migrate pending tool live-tail rows to fragment section updates and decide separately whether assistant streaming/tool transcript rendering should move. This plan targets the highest-risk/highest-payoff area last while making pending tool live-tail decommissioning mandatory.

## Scope

In scope:

- Pending tool live-tail rows in the history region.
- Tool-boundary/status rows that change during a request.
- Optional section-level append/update APIs for fragment bodies.
- Mandatory migration of pending tool live-tail rows.
- A measured decision on whether assistant streaming itself should remain full/incremental rerendered or move to section updates.

Out of scope:

- Replacing the authoritative data buffer.
- Rewriting transcript parsing.
- Changing persisted tool result/render-data format.
- Porting the entire history renderer in one step.

## Current context

- `docs/view.md:67` distinguishes history-region live-tail content from status-zone content.
- `docs/view.md:99` requires full rerenders, live lines, and targeted refreshes to preserve composer text and point.
- `docs/view.md:133` warns live-tail duplicate detection should compare literal lines and avoid giant regexps.
- `mevedel-view.el:900` tracks pending tool calls.
- `mevedel-view.el:914` and `mevedel-view.el:921` define stream and tool-boundary render timers.
- The analysis note cites the correctness-first full rerender path near `mevedel-view.el:8245` and debounced renders near `mevedel-view.el:8114`.

## Implementation steps

1. Inspect current live-tail and incremental render functions before editing:
   - Pending tool row insertion/removal.
   - `mevedel-view--render-incremental`.
   - Tool-boundary render scheduling.
   - Duplicate live-tail detection.
2. Define evaluation criteria before migrating:
   - Does fragment section update reduce marker/insertion-type complexity?
   - Does it reduce full rerenders during tool activity?
   - Can it preserve scroll/point behavior better than current code?
   - Does it avoid divergence from the data-buffer transcript?
3. Extend fragment API only if needed:
   - Add `mevedel-view-fragment--update-section` for label/body updates without replacing the whole fragment.
   - Add append support only for fragments whose body source is explicitly ephemeral UI cache.
   - Keep an escape hatch to delete and rebuild from the data buffer.
4. Migrate pending tool live-tail rows first and mandatorily:
   - Namespace `history-live` or `live-tail`.
   - One fragment per pending tool call key from `mevedel-view--pending-tool-calls`.
   - Render at the existing live-tail anchor in the history region, not in status or interaction zones.
   - Preserve history classification: fragment-backed pending rows must still carry or replace the semantics currently represented by `mevedel-view-pending-tool-live`, and history/tail recovery predicates must recognize them as pending history live-tail rows.
   - Explicitly route or retire the existing pending-row insertion/deletion in `mevedel-view--render-incremental` so tool-boundary renders do not erase fragment-backed rows or duplicate them beside legacy rows.
   - When the incremental renderer deletes/rebuilds the in-flight history region, either include the live-tail managed region in the rebuild contract or recreate live-tail fragments immediately afterward from `mevedel-view--pending-tool-calls`.
   - Update the full-rerender tail preservation path so it strips/recreates pending tool fragments from `mevedel-view--pending-tool-calls` rather than copying stale `Calling ...` rows as ordinary assistant tail text.
   - Remove the fragment when the tool completes through the same path that removes the pending call key.
5. Reassess assistant streaming after pending tool rows:
   - If stream rendering still needs full transcript parsing for correctness, leave it as-is and document why in code comments or docs.
   - If only a small visible tail changes, use section update for that tail while preserving final rerender from the authoritative data buffer.
6. Keep duplicate detection literal-line-based:
   - Do not introduce regexp matching over streamed transcript text.
   - Prefer stable fragment keys for ephemeral live-tail rows.
7. Remove the superseded pending-row/live-tail renderer after fragment migration:
   - The old pending-row insertion/deletion path must either be deleted or become a thin call into the fragment live-tail renderer.
   - Full/incremental rerender remains a correctness fallback for transcript content, but it must not independently render a second copy of pending tool rows.
   - Any compatibility helper must rebuild fragment-owned live-tail rows from `mevedel-view--pending-tool-calls`, not maintain separate live-tail text.
8. Update docs if the live-tail rendering model changes:
   - `docs/view.md` should still state that pending tool rows are history content, not status content.

## Tests

Add or extend tests for:

- Pending tool fragments appear in the history region above the status marker.
- Completing a tool removes only that tool's live-tail fragment.
- Multiple pending tool rows preserve arrival order and max-visible truncation behavior.
- Live-tail refresh does not duplicate rows after a full rerender.
- Composer text and point survive stream/tool-boundary refreshes.
- Long streamed/agent output does not build or require a giant regexp.

## Validation

Run:

```bash
npx @emacs-eask/cli clean elc
npx @emacs-eask/cli test ert test/test-mevedel-view.el test/test-mevedel-view-fragment.el
npx @emacs-eask/cli compile
```

If there are existing tests dedicated to tool rendering, include them too.

## Decommissioning / no-parallel end-state

After this plan, pending tool live-tail rows have one active renderer: the fragment-backed live-tail renderer. The old incremental pending-row text path must be gone or reduced to a wrapper that reconciles fragment-owned rows. Assistant transcript streaming may remain on the existing renderer if the evaluation says not to migrate it; that is not a parallel implementation of pending tool live-tail rows.

## Acceptance criteria

- Pending tool live-tail rows are fragment-backed and remain in the history region.
- Full/incremental transcript rerendering remains the correctness fallback.
- No model-visible state depends on view text properties.
- Composer preservation and duplicate-detection regression tests pass.
- The plan explicitly leaves assistant streaming unchanged if fragment section updates do not improve correctness or simplicity.
- There is no active legacy pending-tool live-tail renderer running alongside the fragment renderer.
- Incremental and full rerender fallback paths recreate pending live-tail fragments from `mevedel-view--pending-tool-calls` and do not preserve stale `Calling ...` rows as ordinary transcript text.
- History/tail recovery predicates recognize fragment-backed pending rows as history-region live-tail content.
