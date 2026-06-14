# Plan 02: Add redraw regression coverage for fragment migrations

## Goal

Create focused regression coverage for composer preservation before moving existing UI surfaces onto fragments. This plan lowers migration risk by encoding the known failure mode: async redraw/status changes must not corrupt active composer text or point.

## Scope

Add or extend tests around view-owned redraw paths, without changing production behavior except for small testability helpers if absolutely necessary.

In scope:

- Active composer draft preservation during status/interaction redraws.
- Multiline drafts whose first editable character is `>`.
- Point preservation inside the composer when UI chrome above it changes.
- A reusable test helper for setting up a view buffer with status, interaction, request-progress, and input zones.

Out of scope:

- Porting interaction/status/task/agent surfaces to fragments.
- Reworking marker insertion-type behavior.
- New user-facing commands.

## Current context

- `docs/view.md:99` states redraw paths must preserve composer text and point.
- `mevedel-view.el:967` preserves input point around chrome changes.
- `mevedel-view.el:991` preserves input text around rendering changes.
- `mevedel-view.el:11302` renders the interaction zone and deletes/reinserts materialized interaction text.
- `mevedel-tool-task.el:1118` materializes task text in the status zone and temporarily changes marker insertion types.

## Implementation steps

1. Inventory existing view redraw tests before adding new cases:
   - Check `test/test-mevedel-view.el` for spinner/request-progress composer preservation coverage.
   - Check `test/test-mevedel-tool-task.el` for task/status redraw coverage, including multiline drafts beginning with `>`.
   - Record in the test doc string or nearby comment which gap the new case fills so this plan does not duplicate existing regression coverage.
2. Locate existing view tests in `test/test-mevedel-view.el` and any helper setup in `test/helpers.el`.
3. Add a small helper if there is no existing one:
   - Create a data buffer and view buffer.
   - Initialize the view mode/markers using existing public or private setup helpers.
   - Insert a prompt and composer draft.
   - Return marker positions and a function to read the editable composer body.
4. Add the missing regression cases, prioritizing interaction-zone changes unless the inventory proves another gap is missing:
   - Start with composer text `"> first line\nsecond line"` immediately after the prompt.
   - Place point after `first` or on the second line.
   - Use `mevedel-view--interaction-register` for a direct materialization case.
   - If testing `mevedel-view--interaction-rebuild`, set up a descriptor that survives rebuild (`preview`, `request`, `ask`, or another preserve-on-rebuild kind) or create real queue-backed state that rebuild will recreate; do not call rebuild on an ordinary descriptor that gets cleared before any interaction text is rendered.
   - Assert composer text is byte-for-byte unchanged.
   - Assert point remains in the composer at the same logical offset.
   - Assert the interaction region contains exactly one copy of the current descriptor body after repeated register/update/rebuild calls.
   - Assert old descriptor body text is absent after an update, unregister, rebuild-clear, or full interaction clear.
5. Add status-zone/task or request-progress cases unless the inventory proves current coverage already checks both composer preservation and stale/duplicate chrome behavior:
   - For task/status, prefer the existing task display path around `mevedel-tool-task--display-overlay` and assert exactly one current task/status body plus zero stale previous bodies.
   - For request progress, start or update the row and assert it remains above the input marker, does not enter the composer, and does not leave stale progress rows behind.
6. Ensure tests avoid brittle full-buffer string assertions unless layout is the subject. Prefer checking zone order, composer body, point offset, and marker ordering.

## Suggested test names

Follow the project convention of one `mevedel-deftest` per function when possible. If these tests primarily cover invariants rather than one function, use the closest function under test and doc strings with a shared prefix.

Possible cases:

- `mevedel-view--interaction-register/test` with docs for composer preservation.
- `mevedel-view--interaction-rebuild/test` with docs for multiline `>` draft preservation.
- `mevedel-tool-task--display-overlay/test` with docs for status redraw composer preservation, but only if the existing task test does not already cover the needed invariant.

## Validation

Run:

```bash
npx @emacs-eask/cli clean elc
npx @emacs-eask/cli test ert test/test-mevedel-view.el test/test-mevedel-tool-task.el
npx @emacs-eask/cli compile
```

If `test/test-mevedel-tool-task.el` does not exist or does not cover view materialization, keep the task coverage in the nearest existing view test file and state that explicitly in the test doc string.

## Decommissioning / no-parallel end-state

This test-only plan should not add an alternate implementation. Its job is to make later migrations safe by failing when old and new render paths both mutate the same view-owned region or when a migration leaves duplicate chrome behind.

## Acceptance criteria

- Tests fail if a redraw inserts, deletes, or moves text inside the editable composer.
- Tests cover a multiline draft whose first editable character is `>`.
- Tests cover the missing interaction-zone redraw gap and rely on existing status/spinner tests when those already cover the invariant.
- No production behavior changes are required for this plan.
- Tests are capable of detecting duplicate old/new chrome for the surfaces they exercise.
- Tests include occurrence-count and stale-body absence assertions, not only composer text/point assertions.
