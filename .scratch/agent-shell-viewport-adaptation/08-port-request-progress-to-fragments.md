# Plan 08: Port request-progress spinner to fragments

## Goal

Move the foreground request progress row from overlay-owned text to a fragment-owned progress block. This removes the last major overlay-backed view-owned chrome row that sits between the interaction zone and the composer.

`docs/view.md` currently calls request progress "the overlay-backed foreground spinner" at `docs/view.md:78`. That is accurate for the current branch, but it is inconsistent with the desired no-redundant-renderer end state where view-owned chrome is fragment-backed and reconciled by stable identity.

## Scope

In scope:

- The bottom request progress row (`Working...`, `Thinking...`, `Compacting...`, elapsed time, active-agent count).
- Spinner frame refresh for the request-progress row.
- Region boundaries between interaction fragments and the input prompt.
- Cleanup of stale spinner text/overlay restore helpers after migration.
- Tests and docs that currently expect `mevedel-view--spinner-overlay`.

Out of scope:

- Pending tool live-tail rows; they are already `history-live` fragments and are cleaned up in Plan 07.
- Inline spinner display properties inside pending tool fragments.
- Assistant transcript streaming and source-backed tool/agent sections.
- Changing request lifecycle semantics.

## Current context

Current overlay/materialized owner:

- `mevedel-view--spinner-overlay` stores the request progress overlay: `mevedel-view.el:891`.
- `mevedel-view--ensure-request-progress` starts or updates the overlay row: `mevedel-view.el:2822`.
- `mevedel-view--refresh-spinner-overlay` deletes and reinserts the overlay text: `mevedel-view.el:3041`.
- `mevedel-view--start-spinner` inserts text and creates the overlay: `mevedel-view.el:3083`.
- `mevedel-view--update-spinner` rewrites the overlay region: `mevedel-view.el:3165`.
- `mevedel-view--stop-spinner` deletes the overlay region and then runs stale-line cleanup: `mevedel-view.el:3207`.
- `mevedel-view--restore-spinner-overlay-in-region` recreates an overlay after full/incremental tail preservation: `mevedel-view.el:3298`.
- `mevedel-view--interaction-region-end` uses the spinner overlay start to keep interaction fragments from overlapping progress text: `mevedel-view.el:11795`.

Related marker complexity:

- `mevedel-view--call-with-request-progress-boundaries` temporarily flips zone marker insertion types so progress text advances only the input boundary: `mevedel-view.el:1086`.
- Full rerender comments still treat request progress as separate rows above input: `mevedel-view.el:8627`.

Agent-shell inspiration:

- `agent-shell-ui-update-fragment` updates a block by namespace/id and can update only the changed section: `/home/roland/agent-shell/agent-shell-ui.el:61`.
- It explicitly keeps append/update local to the body section instead of replacing unrelated content: `/home/roland/agent-shell/agent-shell-ui.el:75`.

The mevedel request-progress row should follow that ownership style: one progress fragment keyed by stable request/progress identity, not overlay text plus stale-text scavenging.

## Proposed model

Add a dedicated progress managed region:

```elisp
(defvar-local mevedel-view--request-progress-region-overlay nil
  "Overlay bounding fragment-managed request-progress text.")
```

Render one fragment:

```elisp
(:namespace progress
 :id request
 :priority 0
 :body (mevedel-view--format-spinner-line status)
 :keymap mevedel-view--display-map
 :navigatable nil)
```

The region should live after interaction fragments and before `mevedel-view--input-marker`. The input marker remains the boundary before the read-only prompt/composer prefix.

## Implementation steps

1. Add a request-progress fragment region helper:
   - when `mevedel-view--request-progress-region-overlay` is live and contains a `progress/request` fragment, reuse that overlay's current bounds before reconciling so repeated updates replace the existing row instead of appending after it,
   - fall back to the current progress anchor (`mevedel-view--request-progress-anchor`) only for initial creation or recovery when no live progress fragment exists,
   - end at the input marker for initial creation/recovery,
   - store the region in `mevedel-view--request-progress-region-overlay`,
   - tag it with a clear property such as `mevedel-view-request-progress-region`.
2. Split request-progress body formatting from overlay formatting:
   - keep elapsed-time and active-agent metadata in `mevedel-view--spinner-display-status`,
   - keep frame spans tagged with `mevedel-view-spinner-frame` so animation can update the display property without full text replacement,
   - remove prefix logic that depends on `mevedel-view--agent-status-overlay`; use zone/fragment spacing instead.
3. Replace overlay start/update with fragment reconciliation:
   - `mevedel-view--ensure-request-progress` should build/reconcile a `progress/request` fragment when active,
   - `mevedel-view--start-spinner` should become a state setter plus render call,
   - `mevedel-view--update-spinner` should set `mevedel-view--spinner-status` and reconcile the fragment,
   - `mevedel-view--stop-spinner` should reconcile the `progress` namespace to nil and clear progress state,
   - every progress reconcile and clear operation must run under `mevedel-view--call-with-request-progress-boundaries` or an equivalent shared zone mutation helper so only `mevedel-view--input-marker` advances across progress text; otherwise progress inserted at the input marker can become part of the composer/input region.
4. Update spinner ticks:
   - `mevedel-view--spinner-tick` should still advance `mevedel-view--spinner-frame-index`,
   - it can refresh frame display properties inside both pending-tool fragments and the progress fragment,
   - it should recompute `mevedel-view--spinner-display-status` on each tick and reconcile the progress body when elapsed-time or active-agent metadata changes,
   - metadata refresh must reuse the live progress region and must not duplicate the row or move point,
   - it should not delete/reinsert the progress row solely for a frame change when only the spinner glyph display property changed.
5. Remove overlay-specific and stale-prefix helpers after tests pass:
   - `mevedel-view--spinner-overlay-live-p`, unless replaced by a fragment-region live predicate,
   - `mevedel-view--refresh-spinner-overlay`,
   - `mevedel-view--delete-stray-spinner-lines`,
   - `mevedel-view--discard-spinner-overlay`,
   - `mevedel-view--restore-spinner-overlay-in-region`,
   - `mevedel-view--stale-spinner-line-p`,
   - `mevedel-view--strip-stale-spinner-prefix`,
   - stale overlay branches in `mevedel-view--stop-spinner` and `mevedel-view--update-spinner`,
   - stale-prefix call sites in `mevedel-view--input-text`, `mevedel-view--queued-user-message-edit-text`, and `mevedel-view--queued-user-message-model-input`.
6. Update interaction boundary logic:
   - `mevedel-view--interaction-region-end` should use the start of the progress fragment region, not `mevedel-view--spinner-overlay`.
   - When no progress fragment exists, it should fall back to the input marker as it does now.
7. Update full/incremental rerender behavior:
   - do not preserve progress text as ordinary tail text,
   - recreate it from request/progress state after the history/status/interaction rebuild,
   - ensure deleting history/status/interaction regions never leaves a detached progress fragment region.
8. Update docs:
   - change `docs/view.md:78` from overlay-backed request progress to fragment-backed request progress,
   - keep the statement that request progress is not history/status/interaction content.

## Tests

Update existing spinner tests in `test/test-mevedel-view.el` so they assert fragment ownership instead of overlay ownership:

- Starting progress creates exactly one `progress/request` fragment above the input marker.
- Repeated ensure/update calls reuse the live progress region and still leave exactly one `progress/request` fragment.
- Updating status replaces the progress fragment body and does not append duplicate metadata.
- Progress reconciliation advances the input marker correctly: `mevedel-view--input-start` remains after the prompt and `mevedel-view--input-text` excludes progress text.
- Spinner ticks update frame display properties without moving point.
- Spinner ticks also refresh elapsed-time and active-agent metadata when `mevedel-view--spinner-display-status` changes, without duplicating the row or moving point.
- Stopping request progress removes only the progress fragment.
- Full rerender during an active request recreates progress from state rather than preserving copied text.
- Interaction region excludes the progress fragment and preserves queued interaction text.
- Progress updates preserve a multiline composer draft whose first editable character is `>`.
- No stale overlayless spinner-line cleanup is needed; remove tests that only assert cleanup of detached `mevedel-view--spinner-overlay` text.
- Input and queued-message tests assert progress fragments never enter composer/model input, replacing stale-prefix stripping coverage.

Existing test areas to update include:

- spinner overlay creation/removal around `test/test-mevedel-view.el:2034`,
- interaction/progress ordering around `test/test-mevedel-view.el:2054`,
- managed interaction region excluding progress around `test/test-mevedel-view.el:5134`,
- request progress preservation around `test/test-mevedel-view.el:2384`,
- stale detached overlay tests around `test/test-mevedel-view.el:2509`.

## Validation

Run:

```bash
npx @emacs-eask/cli clean elc
npx @emacs-eask/cli test ert test/test-mevedel-view.el test/test-mevedel-view-fragment.el
npx @emacs-eask/cli compile
```

If request lifecycle tests live elsewhere, include them too.

## Acceptance criteria

- Request progress has one renderer: `progress/request` fragment reconciliation.
- Repeated progress ensure/update paths reuse the live progress region and do not duplicate rows.
- Progress reconciliation/clear operations preserve zone boundaries so progress text never becomes composer input.
- `mevedel-view--spinner-overlay`, overlay restore/scavenging helpers, and stale-prefix stripping helpers are gone.
- Interaction fragments are bounded by the progress fragment region, not a spinner overlay.
- Progress text is never preserved as ordinary transcript/history tail text during rerender.
- Existing request lifecycle behavior and elapsed/agent-count labels are preserved and refresh while active.
- Composer text and point survive progress start/update/tick/stop.
- Docs no longer describe request progress as overlay-backed.
