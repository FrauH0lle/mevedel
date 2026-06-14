# Plan 04: Port status-zone task and agent rows to fragments

## Goal

Use the fragment/block primitive for status-zone chrome after the interaction-zone migration has proven the abstraction. This plan targets task rows and aggregate live agent status rows, which currently have separate materialization paths.

## Scope

In scope:

- Task overlay/status rows in the status zone.
- Aggregate running/blocked agent status rows in the status zone.
- A status model function if it simplifies tests and keeps rendering deterministic.
- Reuse the fragment primitive for stable identity and ordering.

Out of scope:

- Agent transcript inspection rendering.
- Live-tail/tool rendering in the history region.
- Changing task system semantics or task overlay commands.

## Current context

- `docs/view.md:51` defines the status zone as task rows plus aggregate agent status rows.
- `mevedel-tool-task.el:1077` deletes materialized task text through an overlay.
- `mevedel-tool-task.el:1118` inserts task display text at the status-zone anchor and changes marker insertion types.
- `mevedel-view.el:813` and `mevedel-view.el:816` track aggregate agent status overlay/expanded state.
- The analysis note cites agent status rendering near `mevedel-view.el:10311`.

## Implementation steps

1. Inspect the current aggregate agent status functions around `mevedel-view.el:10311` before editing. Identify the single entry point that refreshes status rows.
2. Define status fragments:
   - Namespace `status`.
   - Task fragment id: `tasks`.
   - Agent aggregate fragment id: `agents` or one fragment per visible aggregate section if current UI has separate regions.
   - Priorities keep task rows above agent rows, matching `docs/view.md:172` examples.
3. Add a mandatory status model/reconcile layer:
   - `mevedel-view--status-model` returns a deterministic plist/list of status items from session and view-buffer state.
   - Preserve existing aggregate-agent collector semantics: visible agent handles, live FSM state, blocked permission/plan queues, and view-local expanded/collapsed state must continue to affect the model.
   - `mevedel-view--status-fragments` converts that model to fragments.
   - One authoritative status render function reconciles the complete `status` managed region, including both task and aggregate agent fragments, so independent refreshes cannot delete each other's fragments.
   - Task and agent refresh entry points should update source state and call the shared status render function rather than reconciling separate fragments at the same anchor.
   - Status reconciliation must update the interaction boundary/managed-region start when status height changes, without deleting or overlapping already-rendered permission/Ask/plan interaction text.
   - Keep task formatting in `mevedel-tool-task.el` unless moving it is necessary to avoid cycles.
4. Adapt task display:
   - Replace only the view-buffer status-zone materialized task text insertion branch in `mevedel-tool-task--display-overlay` with the shared status render path.
   - Keep the non-view tracking-marker/data-buffer overlay fallback working unless this plan deliberately migrates it with equivalent tests; do not remove it as part of status-zone decommissioning.
   - Preserve task toggle compatibility: `mevedel-toggle-tasks` currently relies on an overlay at point with `mevedel-tool-task--show-completed` and `mevedel-tool-task--refresh` properties.
   - Either keep a spanning compatibility overlay for the task fragment with the same properties, or explicitly rewrite the toggle state path and update all callers/tests in the same change.
   - Keep `mevedel-session-task-overlay` coherent until no code depends on it.
5. Adapt aggregate agent status:
   - Render the aggregate status body as one or more status fragments through the shared status render path.
   - Preserve expanded/collapsed state currently held by `mevedel-view--agent-status-expanded-p`.
   - Preserve clickable handles/status keymaps if present.
   - Preserve or migrate the aggregate-status region predicate backed by `mevedel-view--agent-status-overlay`, because targeted inline agent-handle refresh should continue to exclude aggregate status rows.
6. Remove the superseded status materialization paths after the shared status renderer works:
   - Task display should no longer independently insert/delete status-zone text outside the shared fragment status renderer.
   - Agent aggregate status should no longer independently own a separate rendered text region outside the shared fragment status renderer.
   - Any remaining task or agent overlays are compatibility handles over fragment-owned text, not independent renderers.
7. Remove now-unneeded marker insertion-type flips from task/status code only after passing tests. Do not remove high-level zone markers.

## Tests

Add or extend tests for:

- Task rows render above agent rows in the status zone.
- Task refresh updates the existing task fragment rather than duplicating text.
- Clearing all active tasks removes the task fragment and leaves agent status intact.
- Agent status refresh updates the agent fragment without moving composer point.
- Status refresh while a permission/Ask/plan interaction is visible preserves interaction text, overlay identity, and zone ordering.
- Expanded/collapsed agent status state survives a refresh.
- Multiline composer draft beginning with `>` survives task and agent status redraws.
- Keymaps/help text for task toggles and agent handles remain reachable from the rendered status text.
- `mevedel-toggle-tasks` still finds the task region at point and preserves show-completed refresh behavior.
- Targeted agent handle refresh still ignores aggregate agent status rows via the existing or migrated aggregate-status region predicate.

## Validation

Run:

```bash
npx @emacs-eask/cli clean elc
npx @emacs-eask/cli test ert test/test-mevedel-view.el test/test-mevedel-tool-task.el test/test-mevedel-view-fragment.el
npx @emacs-eask/cli compile
```

If a focused agent-status test file exists, include it as well.

## Decommissioning / no-parallel end-state

After this plan, status-zone chrome has one authoritative render path: the shared status model plus fragment reconciler. Task and aggregate-agent code may produce model data or compatibility handles, but must not keep separate insertion/deletion implementations that can render the same rows in parallel.

## Acceptance criteria

- Status-zone task and aggregate agent rows are fragment-backed.
- Existing task and agent status behavior is preserved.
- Status redraws no longer rely on duplicated ad hoc materialized-region code.
- Composer text/point preservation tests pass for task and agent status refresh paths.
- There is no active legacy task or aggregate-agent status renderer running alongside the fragment renderer.
- Status reconciliation preserves any visible interaction-zone fragments/prompts when status height changes.
- The non-view task overlay fallback remains covered or is explicitly migrated with equivalent behavior and tests.
