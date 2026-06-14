# Source-backed transcript rendering boundary

## Purpose

This note records why source-backed transcript rendering and streaming are intentionally outside the current fragment cleanup, and when it would make sense to revisit that decision.

## Core distinction

The fragment migration targets disposable view-owned chrome: UI blocks that can be recreated from session/view state and do not constitute the model-visible conversation transcript.

Examples in scope for the current fragment work:

- pending tool live-tail rows such as `Calling Read...`, derived from `mevedel-view--pending-tool-calls`,
- status-zone task rows,
- aggregate running/blocked agent status rows,
- interaction prompts and queued controls,
- request progress, once Plan 08 migrates it.

Source-backed transcript rendering is different. It is the projection of the authoritative data buffer into the view buffer. It carries source-coordinate behavior and is part of the rendered conversation history, not just surrounding chrome.

## Why transcript streaming was left out

`mevedel-view--render-incremental` rebuilds the in-flight assistant turn from the data buffer. Its responsibilities include transcript parsing, source-coordinate preservation, collapse recovery, and final response reconciliation. Those are correctness-critical transcript-renderer responsibilities, not leftover compatibility shims from the fragment migration.

Migrating transcript streaming now would combine two separate goals:

1. Finish the current fragment migration by removing redundant or dead chrome renderers.
2. Redesign the source-backed transcript renderer.

Keeping those separate reduces risk. If source-backed transcript sections were migrated before the chrome migration is clean, regressions could come from fragment reconciliation, source-coordinate mapping, streaming replacement, collapse-state recovery, or old transcript parsing. That would make the cleanup harder to validate.

## Why pending tool live-tail rows are different

Pending tool live-tail rows visually sit in the history region, but they are not source-backed transcript content. They are ephemeral status rows derived from `mevedel-view--pending-tool-calls`. Later, real tool output arrives through the data buffer and is rendered by the transcript renderer.

That makes pending tool rows a good fragment target even though they live above the status marker.

## When to revisit transcript fragments

Consider a separate source-backed transcript-fragment migration only after the chrome fragment cleanup reaches a stable end state:

1. Plan 07 is complete: status, interaction, and pending-tool fragment migrations have no redundant/dead legacy renderer paths.
2. Plan 08 is complete: request progress is fragment-backed and no longer overlay-owned.
3. Boundary management is consolidated so status, interaction, progress, and input boundaries are updated coherently.
4. Tests cover composer preservation and source-backed behavior across status, interaction, progress, pending live-tail, and transcript refreshes.

At that point, transcript migration would be justified if there is a concrete problem to solve, such as:

- streaming rerenders are too slow,
- source-backed handle refresh remains fragile,
- collapse state across streamed updates remains hard to reason about,
- duplicate/tail recovery keeps accumulating special cases,
- agent-shell-style section append/update would simplify or speed source-backed response bodies.

## Likely first slice

If transcript sections are migrated later, start narrowly:

- migrate only the in-flight assistant turn body to source-backed fragment or section updates,
- keep full rerender from the data buffer as the correctness fallback,
- leave historical turns, tool summaries, and agent transcript handles on the current renderer until the in-flight slice proves stable.

The migration should be planned as a separate architecture change, not as cleanup of the current chrome-fragment branch.
