# Plan 03: Port interaction-zone rendering to fragments

## Goal

Move the bounded interaction-zone painter onto the fragment/block primitive while preserving the existing descriptor API. This is the first production migration because the interaction zone already has stable descriptor identities and is high-value for composer-preservation risk.

## Scope

In scope:

- Keep `mevedel-view--interaction-register` and `mevedel-view--interaction-unregister` as the public internal API for permission, Ask, plan approval, preview, and queued-message UI.
- Replace ad hoc materialized interaction text insertion with fragment reconciliation.
- Preserve existing overlay/keymap/help-echo/read-only behavior observable from interaction text.
- Keep the composite interaction separator behavior.

Out of scope:

- Changing permission/Ask/plan queue semantics.
- Migrating task or agent status.
- Adding collapse/navigation behavior beyond metadata needed by fragments.

## Current context

- `docs/view.md:84` says callers should register controls with `mevedel-view--interaction-register` rather than directly inserting ad hoc UI near the composer.
- `mevedel-view.el:780` and `mevedel-view.el:783` hold descriptor and overlay hash tables.
- `mevedel-view.el:11302` deletes the materialized interaction region, inserts all descriptor bodies, and moves per-descriptor overlays.
- `mevedel-view.el:11423` preserves point/window state when updating a descriptor.
- `mevedel-view.el:11401` rebuilds descriptors from plan, permission, and queued-message state.

## Implementation steps

1. Add a dependency from the interaction rendering functions to `mevedel-view-fragment.el` using the repo's preferred lazy `require` style.
2. Establish overlay compatibility before translating bodies:
   - Keep a stable live spanning overlay object per descriptor id, either created by the fragment module or maintained in `mevedel-view--interaction-overlays`.
   - Reconcile must move/reuse the existing overlay object for an existing descriptor id; do not replace it during rerender.
   - Preserve all arbitrary overlay properties already on that overlay, not only the standard keymap/help/entry fields, because preview and prompt callers may attach cleanup state, temp-file state, diff markers, or other private properties after registration.
   - Ensure that overlay exists before calling `mevedel-view--interaction-body`, because existing prompt/action text properties may store `mevedel-view-interaction-overlay` and expect a live overlay reference.
3. Translate interaction descriptors into fragments:
   - `:namespace` -> `interaction`.
   - `:id` -> descriptor `:id`.
   - `:priority` -> existing descriptor priority or `mevedel-view--interaction-kind-priority`.
   - `:body` -> existing `mevedel-view--interaction-body` result built with the compatibility overlay.
   - `:keymap`, `:help-echo`, `:read-only` -> existing descriptor properties, applied as defaults without erasing body-local button properties.
   - `:navigatable` -> true for descriptors with an activation callback or keymap.
4. Keep separator behavior initially:
   - Continue using `mevedel-view--interaction-separator-overlay` for the composite separator, or represent the separator as a non-navigatable fragment only if that is simpler and does not change visible text.
5. Replace the insertion loop in `mevedel-view--interaction-render`:
   - Stop manually inserting descriptor text in this function.
   - Define a concrete interaction managed region before reconciliation: start at `mevedel-view--interaction-anchor`; end immediately before request-progress/input chrome.
   - When request progress is visible, use the request-progress overlay/start position as the interaction region end; otherwise use the input boundary.
   - Alternatively, use a dedicated interaction container overlay whose bounds explicitly exclude request-progress text and the input prompt; do not use raw `mevedel-view--input-marker` as the end when progress text may live before that marker.
   - Reconcile the interaction namespace only inside that explicit managed region.
   - Keep deletion of stale descriptors scoped to that interaction managed region.
   - Update descriptor compatibility overlays to span the reconciled fragment regions after insertion.
6. Preserve compatibility for callers expecting an overlay return:
   - Keep `mevedel-view--interaction-overlays` populated with the live descriptor overlays returned or updated by fragment rendering.
   - Do not break callers that put prompt overlays in `mevedel--prompt-overlays`.
7. Preserve point/window behavior from `mevedel-view--interaction-register`:
   - If point or any window point is inside an existing descriptor fragment, restore equivalent offsets after reconcile.
   - If point is in the composer, preserve composer point and text exactly.
8. Keep both interaction clear paths correct:
   - `mevedel-view--interaction-clear-for-rebuild` must remove rebuild-owned descriptors, preserve descriptors marked for rebuild preservation, and delete only interaction fragment text/overlays.
   - `mevedel-view--interaction-clear` must fully delete interaction managed-region fragment text, separator overlays, descriptor overlays, and compatibility state so teardown does not leave orphaned interaction text.
   - Neither clear path may delete prompt/composer text.
9. Remove the legacy interaction materialization path after fragment rendering is complete:
   - Delete or inline obsolete materialized-region helpers that only served the old insertion loop.
   - Remove marker insertion-type juggling that is no longer needed for interaction text.
   - Replace any non-interaction code that moves or repairs `mevedel-view--interaction-materialized-overlay` after status/agent insertion with fragment-managed region movement or a shared status/interaction boundary refresh.
   - Remove remaining `mevedel-view--interaction-materialized-overlay` ownership references once fragment region bounds and descriptor compatibility overlays cover the same behavior.
   - Keep only compatibility overlay state that existing callers still use; it must be owned by the fragment path, not a parallel renderer.
10. Update comments/docstrings to describe fragments as disposable view chrome, not session state.

## Tests

Extend the tests from Plan 02:

- Existing interaction descriptor tests still pass without caller changes.
- Registering, updating, and unregistering descriptors updates by id and does not duplicate old bodies.
- Descriptor priority ordering remains unchanged.
- Descriptor keymap, help-echo, activation callback, entry, and read-only behavior remain reachable from the rendered text.
- Point inside an interaction descriptor is restored to the nearest equivalent offset after updating that descriptor.
- Composer text and point are preserved for a multiline draft beginning with `>`.
- Interaction rebuild preserves direct prompt UI where the previous code did.
- Explicit `mevedel-view--interaction-clear` teardown removes fragment-backed interaction text and leaves no orphaned regions.
- Prompt action body text still carries a live `mevedel-view-interaction-overlay` reference where existing action code expects one.
- Updating an existing descriptor reuses the same overlay object and preserves arbitrary overlay properties attached by preview/prompt callers.

## Validation

Run:

```bash
npx @emacs-eask/cli clean elc
npx @emacs-eask/cli test ert test/test-mevedel-view.el test/test-mevedel-view-fragment.el
npx @emacs-eask/cli compile
```

Also manually smoke test one queued permission or Ask prompt if an interactive reproduction is available.

## Decommissioning / no-parallel end-state

After this plan, the interaction zone has one renderer: the fragment-backed reconciler. The old descriptor insertion loop and materialized-region ownership must be gone or reduced to wrappers around the fragment path. Compatibility overlays may remain only as stable handles returned to existing callers; they must not drive a second rendering implementation.

## Acceptance criteria

- Callers still use `mevedel-view--interaction-register` / unregister with the same descriptor shape.
- Interaction-zone visible text and ordering are unchanged except for harmless property implementation details.
- Composer preservation regressions are covered and passing.
- Marker insertion-type juggling in the interaction rendering path is reduced or isolated behind the fragment reconciler.
- There is no active legacy interaction text renderer running alongside the fragment renderer.
- Status/agent refresh paths no longer mutate legacy interaction materialized-region state; they preserve interaction fragments through shared boundary/region handling.
