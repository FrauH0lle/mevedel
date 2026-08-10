# 01 — Persist workspace-owned directives

**What to build:** Make a Ready directive a durable workspace-owned record whose source overlay is only its attached presentation. Users can create, edit, remove, save, and restore the directive without splitting identity or lifecycle state across overlays and sessions.

**Blocked by:** None — can start immediately

**Status:** resolved

- [x] Creating a directive establishes one durable identity containing its current request and source-anchor description.
- [x] The attached overlay resolves that record for rendering and actions rather than owning a second authoritative copy of directive state.
- [x] Editing a Ready directive updates its current request without changing its identity.
- [x] A Ready directive without activity can be removed completely.
- [x] Saving and restoring the workspace recreates the same directive identity, request, anchor, and Ready state in a real source buffer.
- [x] Lifecycle labels and available actions are derived from the record rather than independently persisted overlay status.
- [x] Existing references remain source-bound instructions and do not acquire durable directive behavior.
- [x] Superseded persisted directive shapes are not migrated or read through a compatibility path.
- [x] Tests exercise creation, editing, removal, persistence, and overlay recreation with real temporary files.

## Answer

Resolved by commit `02f3d00`.
