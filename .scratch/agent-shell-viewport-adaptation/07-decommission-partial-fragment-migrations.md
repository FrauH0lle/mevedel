# Plan 07: Decommission partial fragment migrations

## Goal

Turn the current fragment migration into a no-parallel-renderer end state. The branch already makes status, interaction, and pending-tool live-tail rows fragment-backed, but some legacy cleanup, compatibility overlays, and duplicate state remain. This plan removes code that only exists to bridge the old renderers, and explicitly classifies any remaining non-fragment code as either still necessary or a separate migration target.

The guiding rule comes from the original agent-shell inspiration: one block identity should own a rendered UI block. `agent-shell-ui-update-fragment` updates blocks by namespace/id and preserves section state rather than keeping an overlay renderer plus a text renderer in parallel: `/home/roland/agent-shell/agent-shell-ui.el:61`, `/home/roland/agent-shell/agent-shell-ui.el:75`.

## Scope

In scope:

- Pending tool live-tail cleanup that still scans/deletes legacy rows.
- Status-zone compatibility overlays and duplicate collapse/expanded state.
- Interaction-zone separator/chrome that still uses standalone overlays.
- Activation/navigation branches that duplicate fragment metadata for migrated chrome.
- Tests that currently assert compatibility overlays instead of fragment ownership.
- Documentation updates that describe the post-cleanup ownership model.

Out of scope:

- Replacing the authoritative transcript/history renderer.
- Migrating source-backed transcript/tool sections or agent transcript inspection sections.
- Replacing prompt callback semantics unless the interaction overlay replacement is done in the same change with equivalent abort/settle tests.
- Migrating request progress; that is Plan 08 because it is still explicitly overlay-backed in the docs and has a larger spinner lifecycle.

## Audit findings

### A. Pending tool live-tail has a fragment renderer plus legacy text cleanup

Current fragment owner:

- `mevedel-view--pending-tool-region` creates the history-live managed region: `mevedel-view.el:5532`.
- `mevedel-view--pending-tool-fragments` builds one `history-live` fragment per pending call: `mevedel-view.el:5574`.
- `mevedel-view--insert-pending-tool-lines` reconciles those fragments: `mevedel-view.el:6651`.

Leftover bridge/legacy cleanup:

- `mevedel-view--delete-pending-tool-property-lines` deletes rows by old text properties: `mevedel-view.el:5608`.
- `mevedel-view--pending-tool-live-tail-line-p` recognizes unpropertized `Calling ...` rows heuristically: `mevedel-view.el:5638`.
- `mevedel-view--delete-pending-tool-looking-lines` deletes unpropertized pending-looking rows: `mevedel-view.el:5668`.
- `mevedel-view--strip-pending-tool-live-lines-from-string` strips pending rows out of preserved live-tail strings: `mevedel-view.el:5711`.
- `mevedel-view--delete-pending-tool-live-lines` reconciles the fragment region to nil, then also calls the legacy scanners: `mevedel-view.el:6632`.
- Incremental rendering calls the legacy deletion wrapper before fragment reinsert: `mevedel-view.el:6346` and `mevedel-view.el:6464`.
- Final response and full rerender still call the same wrapper: `mevedel-view.el:5828`, `mevedel-view.el:8600`.

Classification: true migration debt. The fragment renderer is now the owner, so heuristic deletion of text that merely resembles `Calling ...` is redundant and can delete legitimate transcript text if a guard regresses.

### B. Task status still uses a view compatibility overlay as state

Current fragment owner:

- `mevedel-view--status-model` computes task and agent status: `mevedel-view.el:11175`.
- `mevedel-view--status-fragments` builds `status/tasks` and `status/agents` fragments: `mevedel-view.el:11206`.
- `mevedel-view--render-status` reconciles the status namespace: `mevedel-view.el:11314`.

Leftover duplicate state:

- `mevedel-view--status-sync-task-overlay` creates/moves `mevedel-session-task-overlay` over the task fragment: `mevedel-view.el:11245`.
- `mevedel-toggle-tasks` still finds an overlay carrying `mevedel-tool-task--show-completed` and a refresh callback: `mevedel-tool-task.el:1023`.
- The view status model still reads old overlay state as a fallback: `mevedel-view.el:11137`.
- Tests still assert that view rendering creates a task overlay: `test/test-mevedel-tool-task.el:1033`.

Classification: partial migration. The overlay is no longer the renderer, but it is still a view-state carrier. That is avoidable because the branch already has fragment collapse state keyed by `mevedel-view--status-task-collapse-key` at `mevedel-view.el:852`.

### C. Aggregate agent status has fragment text plus legacy overlay/state

Current fragment owner:

- Agent status body is produced by `mevedel-view--status-model`: `mevedel-view.el:11188`.
- Agent status becomes a `status/agents` fragment with a collapse key: `mevedel-view.el:11226`.

Leftover duplicate state:

- `mevedel-view--agent-status-overlay` still exists as an overlay covering materialized status text: `mevedel-view.el:846`.
- `mevedel-view--agent-status-expanded-p` still exists as a buffer-local expanded-state variable: `mevedel-view.el:849`.
- `mevedel-view--status-sync-agent-overlay` creates/moves the aggregate-status overlay over the fragment: `mevedel-view.el:11282`.
- `mevedel-view--agent-status-region-position-p` excludes aggregate rows from source-backed handle refresh using the overlay: `mevedel-view.el:11345`.
- `mevedel-view--agent-status-string` reads the legacy expanded variable to choose `[+]` or `[-]`: `mevedel-view.el:10902`.
- `mevedel-view-agent-status-toggle` writes fragment collapse state and then mirrors it into the legacy variable: `mevedel-view.el:11510`.

Classification: partial migration. Fragment collapse state should be the single source of truth. The overlay can be replaced by fragment metadata when code needs to distinguish aggregate status rows from source-backed transcript handles.

### D. Interaction descriptor text is fragment-backed, but separator and callback overlays remain

Current fragment owner:

- `mevedel-view--interaction-fragment` creates one `interaction` fragment per descriptor: `mevedel-view.el:11934`.
- `mevedel-view--interaction-render` reconciles interaction fragments: `mevedel-view.el:11987`.

Remaining overlay use:

- `mevedel-view--interaction-separator-overlay` renders the composite separator as virtual overlay chrome: `mevedel-view.el:813`, `mevedel-view.el:12000`.
- `mevedel-view--interaction-overlays` stores descriptor overlays keyed by id: `mevedel-view.el:810`.
- Prompt settlement still needs overlay identity and properties such as `mevedel--callback`, `mevedel--owning-request`, and `mevedel-settled`: `mevedel-tool-ui.el:454`.
- Prompt lookup intentionally falls back from text at point to the `mevedel-view-interaction-overlay` property: `mevedel-tool-ui.el:501`.

Classification: mixed. The separator overlay is redundant render chrome and should become a fragment. Descriptor overlays are not dead as long as prompt settlement and abort semantics use overlay identity. They should either remain as explicitly named callback handles, or be removed only with a replacement settlement handle and equivalent tests.

### E. Source-backed transcript rendering is intentionally not a fragment migration target

`mevedel-view--render-incremental` documents that assistant streaming stays data-buffer-derived and that the fragment migration is limited to ephemeral pending-tool rows: `mevedel-view.el:6246`. This is not dead code. The transcript/data-buffer renderer remains the correctness fallback.

### F. Dead helpers and stale wrappers remain from the old renderers

These are low-risk cleanup targets because they either have no current production caller or only delegate to the new shared renderer:

- `mevedel-tool-task--delete-materialized-region` still handles overlays tagged `mevedel-tool-task--materialized`: `mevedel-tool-task.el:1095`. Current fragment status sync does not set that flag.
- `mevedel-tool-task--delete-overlay` still branches on `mevedel-tool-task--materialized`: `mevedel-tool-task.el:1113`.
- `mevedel-tool-task--view-anchor` still computes the old direct insertion point, but the view path delegates to `mevedel-view--render-status`: `mevedel-tool-task.el:1124`, `mevedel-tool-task.el:1158`.
- `mevedel-view--render-task-status` is now only a private wrapper around `mevedel-view--render-status`: `mevedel-view.el:8464`.
- `mevedel-view--call-with-render-boundaries-advancing` is unused; the active history path uses `mevedel-view--with-render-boundaries-advancing`: `mevedel-view.el:1058`, `mevedel-view.el:5746`.

Classification: dead or compatibility-only code. Delete it before deeper refactors so later diffs are easier to review.

### G. Old aggregate-agent row rendering is only partly used

Current expanded aggregate agent status renders Agent handles through `mevedel-view--agent-status-handles-string`: `mevedel-view.el:10973`, selected by the status model at `mevedel-view.el:11191`.

Older row-style helpers still exist:

- `mevedel-view--agent-status-row-string`: `mevedel-view.el:10851`.
- `mevedel-view--agent-status-buttonize-toggle`: `mevedel-view.el:10882`.
- `mevedel-view--agent-status-string`: `mevedel-view.el:10902`.
- `mevedel-view-agent-status-activate-row`: `mevedel-view.el:11580`.

Classification: partial migration. The branch already renders expanded aggregate status via Agent handles, so the old expanded row/reveal path is dead code. Keep a collapsed header renderer if needed, but make removal of the old expanded row helpers mandatory.

### H. Fragment paths still duplicate boundary and preservation logic

These are not dead yet, but they are remaining partial-migration seams:

- Status, interaction, and pending fragments each have their own marker insertion-type wrapper: `mevedel-view.el:11097`, `mevedel-view.el:11825`, `mevedel-view.el:5779`.
- `mevedel-view--interaction-register` still manually captures/restores descriptor and window offsets even though fragment reconciliation has preservation machinery: `mevedel-view.el:12063`.
- Status/interaction boundary repair is still spread across status anchor/region helpers and interaction relocation: `mevedel-view.el:11031`, `mevedel-view.el:11049`, `mevedel-view.el:11332`, `mevedel-view.el:11853`.

Classification: keep until replaced. The wrappers preserve zone ordering and composer safety today; consolidate them only after equivalent status/interaction/request-progress ordering tests are in place.

## Implementation phases

### Phase 0: Delete dead wrappers and stale task materialization helpers

1. Delete `mevedel-tool-task--view-anchor` and any declarations used only by it.
2. Delete `mevedel-tool-task--delete-materialized-region`.
3. Simplify `mevedel-tool-task--delete-overlay` so it deletes `mevedel-session-task-overlay` and clears the session slot without a materialized-region branch.
4. Delete `mevedel-view--render-task-status`, or keep it only if an in-tree caller remains and document it as a compatibility alias.
5. Delete unused `mevedel-view--call-with-render-boundaries-advancing`; keep `mevedel-view--with-render-boundaries-advancing` because history rendering still uses it.
6. Remove tests that fabricate `mevedel-tool-task--materialized`, or rewrite them to assert fragment-backed status cleanup.

End state: no old direct task materialization helper remains in the view-status path.

### Phase 1: Make pending tool live-tail fragment-only

1. Change `mevedel-view--delete-pending-tool-live-lines` into a fragment-only clear, or replace it with a more explicit helper such as `mevedel-view--clear-pending-tool-fragments`.
2. Update `mevedel-view--volatile-live-tail-line-p` before deleting the pending heuristic helper it currently calls at `mevedel-view.el:8492`:
   - classify pending live-tail volatility from fragment metadata or from `mevedel-view--pending-tool-calls` directly,
   - do not call `mevedel-view--pending-tool-live-tail-line-p` once that helper is deleted.
3. Delete the heuristic/legacy helpers once no current path emits old pending rows and no current caller depends on them:
   - `mevedel-view--delete-pending-tool-property-lines`
   - `mevedel-view--pending-tool-spinner-prefix-regexp`
   - `mevedel-view--pending-tool-live-tail-labels`
   - `mevedel-view--pending-tool-live-tail-line-p`
   - `mevedel-view--line-has-transcript-history-p`, if it has no non-pending callers after cleanup
   - `mevedel-view--delete-pending-tool-looking-lines`
   - `mevedel-view--pending-tool-live-tail-cleanup-bounds`
4. Replace `mevedel-view--strip-pending-tool-live-lines-from-string` with a non-heuristic path:
   - either preserve only source-backed in-flight transcript text and never copy `history-live` fragments, or
   - strip runs whose text has `mevedel-view-fragment-namespace` equal to `history-live`.
5. Keep `mevedel-view-pending-tool-live` only if it is still needed for spinner-frame refresh or history-tail recovery. It must not be used as a deletion key.
6. Update `mevedel-view--render-incremental`, `mevedel-view--refresh-pending-tool-lines`, final response, and full rerender callers so pending rows are recreated only from `mevedel-view--pending-tool-calls`.

End state: the only pending live-tail owner is `mevedel-view-fragment--reconcile` over the `history-live` namespace.

### Phase 2: Remove view task compatibility overlay state

1. Teach `mevedel-toggle-tasks` to prefer fragment metadata when point is on a `status/tasks` fragment:
   - read `mevedel-view-fragment-collapse-key` or `mevedel-view-fragment-id`,
   - toggle `mevedel-view--status-task-collapse-key`,
   - call `mevedel-view--render-status` when available.
2. Keep the existing overlay branch only for the non-view/data-buffer fallback path in `mevedel-tool-task--display-overlay`.
3. Remove the view-buffer creation of `mevedel-session-task-overlay` from `mevedel-view--status-sync-task-overlay`.
4. Delete `mevedel-view--status-sync-task-overlay` and `mevedel-view--status-sync-overlays` if agent overlay sync is also removed in Phase 3.
5. Simplify `mevedel-view--status-task-show-completed-p` so fragment collapse state is the only view source. Do not read `mevedel-session-task-overlay` in the view renderer.
6. Update tests that currently assert a view task overlay to assert fragment ownership instead:
   - `mevedel-view-fragment-namespace` is `status`,
   - `mevedel-view-fragment-id` is `tasks`,
   - toggling changes fragment collapse state and rerenders exactly one task block.

End state: `mevedel-session-task-overlay` may still exist for the older data-buffer after-string fallback, but the view status zone does not use it as rendering or disclosure state.

### Phase 3: Remove aggregate agent status compatibility overlay/state

1. Replace `mevedel-view--agent-status-expanded-p` with an explicit `expanded-p` argument through the status rendering helpers:
   - make `mevedel-view--agent-status-string` accept `expanded-p`, or add a new helper that does,
   - keep collapsed/expanded truth in `mevedel-view-fragment-collapse-state` for `mevedel-view--status-agent-collapse-key`.
2. Before deleting `mevedel-view--agent-status-overlay`, handle every current consumer:
   - replace `mevedel-view--agent-status-region-position-p` with a predicate that checks text properties at POS (`mevedel-view-fragment-namespace` is `status` and `mevedel-view-fragment-id` is `agents`),
   - update `mevedel-view--agent-handle-ids-in-buffer` and `mevedel-view--agent-handle-refresh-points` to use the new predicate when skipping aggregate status rows,
   - either update `mevedel-view--request-progress-prefix` to derive spacing from status/progress fragment boundaries, or defer deleting `mevedel-view--agent-status-overlay` until Plan 08 replaces request progress; the current formatter reads the overlay at `mevedel-view.el:2959`.
3. Delete `mevedel-view--status-sync-agent-overlay` only after the above consumers no longer need an overlay handle.
4. Delete `mevedel-view--agent-status-overlay` after request-progress spacing no longer depends on it.
5. Update tests that currently assert the overlay to assert the `status/agents` fragment bounds and properties instead.

End state: aggregate agent status has one owner: the `status/agents` fragment plus fragment collapse state.

### Phase 4: Move interaction separator into the interaction fragment region

1. Represent the composite interaction separator as non-navigatable fragment chrome, either:
   - a dedicated `interaction/:separator` fragment, or
   - label/separator support inside `mevedel-view-fragment--reconcile` for the interaction namespace.
2. Delete `mevedel-view--interaction-separator-overlay` and its relocation/clear paths.
3. Keep descriptor callback overlays only if prompt settlement still depends on them. If they remain, rename comments/docstrings to make clear they are callback handles, not render owners.
4. If removing descriptor overlays too, replace all prompt overlay state used by `mevedel--prompt--settle` with an explicit descriptor/handle object and update abort, stale-prompt, and double-settle tests in the same change.

End state: interaction visible chrome is fragment-owned. Any remaining overlay is a prompt settlement handle, not a parallel text renderer.

### Phase 5: Consolidate activation for migrated chrome

1. After task/agent overlay removal, simplify `mevedel-view-activate-at-point` so fragment activation is the normal path for migrated chrome.
2. Keep source-backed agent transcript handles separate because they are transcript rendering, not status chrome.
3. Preserve the guard that interaction prompts must not be settled through generic activation from the composer: `mevedel-view.el:9988`.
4. Update keymaps for migrated status/interaction chrome to call the same activation helper.

End state: migrated fragments use fragment activation/collapse metadata. Source-backed transcript sections keep their existing disclosure/activation model until a separate transcript migration.

### Phase 6: Remove old aggregate-agent expanded-row rendering

1. Treat expanded aggregate agent status as Agent-handle based; the current status model already uses `mevedel-view--agent-status-handles-string` for the expanded branch.
2. Delete `mevedel-view--agent-status-row-string` and `mevedel-view-agent-status-activate-row`.
3. Keep or rewrite only the collapsed aggregate header renderer, and make it take explicit collapsed/expanded state rather than reading `mevedel-view--agent-status-expanded-p`.
4. Update tests that manually insert old row strings to exercise fragment-backed aggregate Agent handles instead.

End state: aggregate agent status has one visible expanded representation: the `status/agents` fragment body built from Agent-handle renderings.

### Phase 7: Move preservation and boundary management into shared helpers

1. Extend `mevedel-view-fragment--reconcile` or add a small capture/restore API so callers can preserve point/window offsets by fragment key.
2. Remove the manual point/window offset block in `mevedel-view--interaction-register` once the fragment helper covers the same behavior.
3. Consolidate status, interaction, pending-tool, and later request-progress marker wrappers into one zone mutation helper.
4. Keep existing zone-order repair until the shared helper updates `status-marker`, `interaction-marker`, request-progress start, and `input-marker` in one place.

End state: migrated fragment surfaces do not each carry bespoke boundary and preservation machinery.

## Tests

Add or update focused tests before deleting compatibility code:

- Pending live-tail:
  - repeated refresh does not duplicate fragments,
  - post-tool completion removes only the completed fragment,
  - full rerender recreates pending fragments from `mevedel-view--pending-tool-calls`,
  - ordinary transcript text containing `Calling Read...` is preserved because no heuristic cleanup remains,
  - remove/replace the old test that only proves cleanup of pre-fragment spinner-frame rows at `test/test-mevedel-view.el:8724`.
- Task status:
  - task status text has `status/tasks` fragment metadata,
  - `mevedel-toggle-tasks` works without a view task overlay,
  - data-buffer fallback overlay still works if that fallback remains,
  - completed-only task groups still do not render a view fragment until expanded by active status.
- Agent status:
  - aggregate status text has `status/agents` fragment metadata,
  - collapse state survives refresh without `mevedel-view--agent-status-expanded-p`,
  - source-backed handle refresh skips aggregate status via fragment metadata.
- Interaction:
  - separator text appears/disappears with descriptor count without `mevedel-view--interaction-separator-overlay`,
  - descriptor callback overlays, if kept, still settle/abort exactly once,
  - `mevedel--prompt--overlay-at-point` still finds prompt callback state or is replaced by an equivalent handle lookup.
- Composer preservation:
  - all migrated cleanup paths preserve multiline composer drafts whose first editable character is `>`.
- Stale terminology/test rewrites:
  - rename `test/test-mevedel-tool-task.el:1019`, `test/test-mevedel-tool-task.el:1037`, and `test/test-mevedel-tool-task.el:1066` away from "materialized" overlay language,
  - rename stale "materialized" view test comments/docstrings around `test/test-mevedel-view.el:4041`, `test/test-mevedel-view.el:4100`, `test/test-mevedel-view.el:5421`, `test/test-mevedel-view.el:11639`, and `test/test-mevedel-view.el:11770`,
  - remove or rewrite the fabricated `mevedel-tool-task--materialized` setup at `test/test-mevedel-view.el:11751`,
  - replace old aggregate row activation coverage around `test/test-mevedel-view.el:12685` with fragment-backed Agent-handle activation/collapse coverage.

## Validation

Run the focused checks first:

```bash
npx @emacs-eask/cli clean elc
npx @emacs-eask/cli test ert test/test-mevedel-view.el test/test-mevedel-tool-task.el test/test-mevedel-tool-ui.el test/test-mevedel-permission-queue.el test/test-mevedel-view-fragment.el
npx @emacs-eask/cli compile
```

If prompt settlement handles change, also run:

```bash
npx @emacs-eask/cli test ert test/test-mevedel-async-prompt.el test/test-mevedel-tool-plan.el
```

## Acceptance criteria

- Pending tool live-tail rows have exactly one renderer: `history-live` fragment reconciliation.
- No heuristic cleanup deletes unpropertized `Calling ...` lines from history text.
- The view status zone no longer creates task compatibility overlays for rendering/state.
- The aggregate-agent compatibility overlay is removed once request-progress spacing no longer depends on it; if Plan 07 is implemented before Plan 08, the plan must leave an explicit temporary dependency rather than deleting the overlay prematurely.
- Task and aggregate-agent collapse state is single-sourced in fragment collapse state.
- Interaction visible separator chrome is fragment-owned.
- Any remaining interaction overlays are justified callback handles, documented as such, and not responsible for rendering visible text.
- Tests assert fragment ownership instead of compatibility overlays for migrated chrome.
- Dead task materialization helpers, unused render-boundary wrappers, and stale task-status aliases are gone or documented as compatibility aliases with real callers.
- Old aggregate-agent expanded-row rendering is removed; expanded aggregate status is Agent-handle based.
- Fragment surfaces no longer duplicate point/window preservation and marker insertion-type logic once the shared boundary helper lands.
- Stale "materialized" test/docstring wording is removed where the text is fragment-backed.
- Source-backed transcript rendering remains documented as intentionally outside this cleanup.
