# Plan 05: Add shared fragment navigation, collapse, and action behavior

## Goal

Build shared behavior on top of fragment metadata so interactive view-owned blocks behave consistently. This adapts the useful agent-shell ideas around block navigation and collapse without splitting mevedel into separate edit/view major modes.

## Scope

In scope:

- Navigation between navigatable fragments by text property.
- Per-fragment collapse state for eligible view-owned blocks.
- A small action/keymap convention for fragment labels and bodies.
- Optional isearch integration only if cheap and consistent with existing view mode hooks.

Out of scope:

- New major modes.
- Changing queue semantics for Ask, permission, plan approval, or previews.
- Migrating live-tail/tool rendering.

## Current context

- `docs/view.md:84` says interaction keybindings are active only when point is on interaction text; composer input must never settle or cycle prompts.
- `mevedel-view.el:825` already has source-backed disclosure state for rendered transcript/tool sections.
- `mevedel-view.el:9731` and nearby code provide clickable handles/status for rendered agent transcript views.
- The analysis note points out that agent-shell supports per-block fold state, next/previous navigation, global fold toggle, and isearch expansion.

## Implementation steps

1. Add navigation helpers to `mevedel-view-fragment.el` or `mevedel-view.el`:
   - `mevedel-view-fragment-next` / previous move point to the next text range with `mevedel-view-fragment-navigatable`.
   - Bound scans to view-owned chrome: navigation must stop before `mevedel-view--input-marker` / `mevedel-view--input-start` and must never move into the editable composer.
   - Respect narrowing and buffer bounds.
2. Route existing navigation bindings through fragment helpers instead of adding a parallel navigation system:
   - Inspect current display-text navigation bindings such as `n`/`p` on `mevedel-view--display-map`.
   - Replace or wrap those existing commands so fragment navigation is the implementation for migrated display/chrome surfaces.
   - Add new `mevedel-view-mode-map` bindings only if they do not conflict with existing composer bindings and only when they delegate to the same fragment navigation helpers.
   - Keep activation commands local to point on fragment/display text, never globally active in the composer.
3. Add collapse state storage:
   - Use a buffer-local hash keyed by fragment key and, when necessary, a source-backed key supplied by the producer.
   - Store only UI disclosure state, not durable conversation state.
   - Provide `mevedel-view-fragment-toggle-collapsed` and helpers for producers to render collapsed bodies.
4. Decide which surfaces opt in first:
   - Interaction prompts: navigatable yes, collapsible no by default.
   - Task status: navigatable yes if it has actions; collapsible optional for completed detail.
   - Agent status: navigatable yes; collapsible state must be single-sourced. Convert `mevedel-view--agent-status-expanded-p` / `mevedel-view-agent-status-toggle` to compatibility wrappers around fragment collapse state, or remove them after callers use fragment collapse directly.
   - Tool/response source-backed sections should keep existing source-collapse logic until Plan 06 or a later migration.
5. Standardize action metadata:
   - Allow a fragment `:activate` function and/or `:entry` payload, mirroring current interaction descriptor behavior.
   - Define one fragment activation helper for migrated surfaces; RET, mouse, and keymap handlers should dispatch through that helper.
   - Convert old interaction/status activation handlers to thin callers of the shared helper or keep their overlay properties only as compatibility data consumed by the helper.
   - Ensure RET/mouse activation only fires when point/click is on fragment text, never from the composer.
6. Replace any per-surface navigation/collapse/action implementations that now duplicate fragment behavior:
   - Keep source-backed disclosure for transcript/tool sections until those sections explicitly migrate.
   - For migrated interaction/status surfaces, route navigation, activation, and collapse through fragment helpers rather than maintaining surface-specific equivalents.
   - Remove obsolete per-surface keybindings/helpers only after preserving their behavior through the shared commands and tests.
7. Optional isearch behavior:
   - If existing view mode already hooks isearch for collapsed source sections, extend it to expand fragment matches.
   - Otherwise defer this to a follow-up; do not add broad advice around isearch in this plan.

## Tests

Add tests for:

- Next/previous navigation skips non-navigatable fragments and stops before the composer.
- Activation runs only when point is on an actionable fragment.
- Composer keypresses do not trigger fragment activation.
- Collapse toggle hides body text for an opted-in fragment and restores it on second toggle.
- Collapse state survives a fragment refresh with the same key.
- Existing agent status expanded/collapsed behavior still works after being backed by fragment collapse state, with no independent legacy state path left active.
- Existing display navigation bindings route through fragment navigation for migrated surfaces.
- RET/mouse/keymap activation for migrated surfaces dispatches through one fragment activation helper.

## Validation

Run:

```bash
npx @emacs-eask/cli clean elc
npx @emacs-eask/cli test ert test/test-mevedel-view.el test/test-mevedel-view-fragment.el test/test-mevedel-tool-task.el
npx @emacs-eask/cli compile
```

## Decommissioning / no-parallel end-state

After this plan, migrated fragment-backed surfaces should use the shared fragment navigation/collapse/action helpers. Surface-specific behavior remains only where the surface has not migrated yet or where source-backed transcript disclosure has different invariants.

## Acceptance criteria

- Shared navigation works for opted-in fragments without affecting composer editing.
- Shared collapse state is disposable UI state and does not leak into the model-visible transcript.
- Interaction/action behavior remains explicit: no accidental prompt settling from the composer.
- Existing source-backed disclosure tests continue to pass.
- Migrated surfaces do not keep duplicate navigation/collapse/action code paths alongside the shared fragment helpers.
- Agent status collapse state is single-sourced through fragment collapse state or a documented compatibility wrapper that reads/writes that state.
