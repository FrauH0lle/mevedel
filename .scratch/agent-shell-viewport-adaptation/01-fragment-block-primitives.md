# Plan 01: Add view fragment/block primitives

## Goal

Introduce a small internal fragment/block UI layer for view-owned chrome without changing user-visible behavior. This gives later plans a stable primitive for status, interaction, task, agent, and live-tail regions while preserving mevedel's data-buffer-first architecture.

## Scope

Create a new internal module, tentatively `mevedel-view-fragment.el`, plus focused tests. Do not migrate existing view surfaces in this plan.

In scope:

- Fragment identity and ordering.
- Label/body section rendering.
- Text-property-backed section metadata.
- Update/delete/reconcile operations by stable identity.
- Optional keymap/help-echo/read-only/navigatable/collapsed metadata.
- Preservation helpers for point/window state within updated fragment regions.

Out of scope:

- Replacing `mevedel-view--status-marker`, `mevedel-view--interaction-marker`, or `mevedel-view--input-marker`.
- Changing the interaction descriptor API.
- Moving task, agent, tool, or live-tail rendering.
- Durable conversation state in view text properties.

## Current context

- `docs/view.md:18` says the view is reconstructable and durable state must not live only in overlays or text properties.
- `docs/view.md:37` defines the zone order that fragments must respect.
- `mevedel-view.el:755` through `mevedel-view.el:790` define zone markers and current interaction overlay tables.
- `mevedel-view.el:967` defines `mevedel-view--call-preserving-input-point`, which fragment mutations should reuse rather than duplicate.

## Proposed API

Keep the first API intentionally private and plist-based so migrations can evolve it without committing to a public struct.

Fragment shape:

```elisp
(:namespace interaction
 :id permission-123
 :priority 100
 :label-left "Permission"
 :label-right "Bash"
 :body BODY
 :keymap MAP
 :help-echo HELP
 :read-only t
 :navigatable t
 :collapsed nil)
```

Core functions:

- `mevedel-view-fragment--key` -> return `(REGION NAMESPACE . ID)` or another container-scoped key that includes the managed-region identity.
- `mevedel-view-fragment--render` -> return propertized text for one fragment.
- `mevedel-view-fragment--insert` -> insert a fragment into a managed region at point and return its overlay or bounds record.
- `mevedel-view-fragment--update` -> replace an existing fragment region by container-scoped key.
- `mevedel-view-fragment--delete` -> remove a fragment region by container-scoped key.
- `mevedel-view-fragment--reconcile` -> render a namespace inside an explicit managed region from a sorted fragment list, deleting stale fragments only inside that region.
- `mevedel-view-fragment--bounds-at` -> find fragment bounds from text properties at point, including managed-region identity.

Text properties:

- `mevedel-view-fragment-key` -> container-scoped key, such as `(REGION NAMESPACE . ID)`.
- `mevedel-view-fragment-region` -> managed-region identity.
- `mevedel-view-fragment-namespace` -> namespace symbol.
- `mevedel-view-fragment-id` -> stable id.
- `mevedel-view-fragment-section` -> `label`, `body`, or `separator`.
- `mevedel-view-fragment-navigatable` -> non-nil when navigation should visit it.
- `read-only`, `front-sticky`, and `rear-nonsticky` consistent with existing view-owned chrome.

## Implementation steps

1. Add `mevedel-view-fragment.el` with lexical-binding header, commentary, code, provide footer, and no top-level `require` beyond compile-time-only dependencies if needed.
2. Implement fragment normalization helpers:
   - Validate that `:namespace` and `:id` are present.
   - Default `:priority` to `0`.
   - Default `:read-only` to `t`.
   - Treat missing body as empty string.
3. Implement text rendering:
   - Render an optional label line from `:label-left` and `:label-right`.
   - Preserve existing body `font-lock-face`, `face`, `keymap`, `help-echo`, mouse, and action-related properties.
   - Apply fragment-level `:keymap` and `:help-echo` only where the body does not already provide more specific local properties.
   - Add fragment metadata over label and body sections without erasing embedded button/action spans.
   - Ensure exactly one trailing newline per fragment when inserted by reconcile.
4. Implement buffer mutation helpers:
   - Use `inhibit-read-only` and `inhibit-modification-hooks` around view-owned changes.
   - Do not make `mevedel-view-fragment.el` require `mevedel-view.el`; avoid a circular dependency.
   - Accept an optional caller-supplied preservation wrapper/callback, so `mevedel-view.el` can pass `mevedel-view--call-preserving-input-point` when fragment updates occur in view zones.
   - Preserve point/window offsets when the point is inside the fragment being updated, matching the current intent of `mevedel-view--interaction-register` in `mevedel-view.el:11423`.
5. Implement managed-region reconciliation:
   - Require callers to pass an explicit region identity, such as a container overlay, start/end markers, or a region object returned by the fragment module.
   - Include that region identity in all fragment lookup/update/delete keys; do not rely on `(NAMESPACE . ID)` being globally unique.
   - Sort fragments by descending `:priority` to match existing interaction ordering, then insertion order supplied by caller for equal priorities.
   - Delete stale fragment regions for the namespace only inside the managed region.
   - Leave same-namespace fragments in other zones or containers untouched.
   - Keep reconciliation anchored by the caller; do not discover or move high-level zone markers.
6. Add the loader path explicitly:
   - Add `(require 'mevedel-view-fragment)` to the top-level loader if `mevedel.el` currently requires all runtime modules for downstream `(require 'mevedel)`.
   - Use lazy `require` at call sites only if the existing loader pattern clearly excludes private view helpers; document that choice in the implementation notes.

## Tests

Create `test/test-mevedel-view-fragment.el`.

Test cases:

- Rendering tags label and body with the expected container-scoped fragment key, managed-region identity, and section properties.
- Reconcile inserts fragments in descending priority order, preserves caller order for equal priorities, and removes stale fragments from the same namespace.
- Updating an existing fragment preserves unrelated text before and after the region.
- Updating a fragment while point is inside that fragment restores point to the nearest equivalent offset.
- Updating a fragment preserves window point and window-start offsets for windows whose point/start is inside the replaced fragment.
- Read-only properties are applied to view-owned fragment text.
- Existing body font-lock, keymap, help-echo, and action text properties survive rendering unless the fragment deliberately supplies a default for missing properties only.
- Reconcile deletes stale fragments inside its managed region but does not delete same-namespace fragments in a different managed region.

If the test helper supports real temporary buffers, use those rather than mocks.

## Validation

Run:

```bash
npx @emacs-eask/cli clean elc
npx @emacs-eask/cli test ert test/test-mevedel-view-fragment.el
npx @emacs-eask/cli compile
```

## Decommissioning / no-parallel end-state

This foundational plan adds an unused primitive only. It must not create a second active rendering path for any existing surface. Existing surfaces continue to use their current renderers until their dedicated migration plans replace and delete the old paths.

## Acceptance criteria

- New fragment module compiles without warnings.
- New tests pass.
- No existing user-visible view behavior changes.
- Fragment metadata remains disposable UI cache; no session state depends on it.
- No existing UI surface has both fragment-backed and legacy rendering active after this plan.
