# 03 — Open directives in a dedicated activity surface

**What to build:** Give every durable directive a workspace activity surface outside the main chat. Users can open it from source or a workspace list and inspect the directive’s request, anchor, state, and chronological activity without crowding the source overlay.

**Blocked by:** 01 — Persist workspace-owned directives

**Status:** resolved

- [x] An attached directive can open its dedicated activity surface from its source actions.
- [x] A workspace-level directive list can open the same surface without requiring point to be inside a source overlay.
- [x] The surface shows the current directive request, derived lifecycle state, anchor state, and an initially empty chronological activity area.
- [x] The surface provides navigation back to a live attached source anchor when one exists.
- [x] Source overlays remain compact and do not embed substantive directive responses, patches, or history.
- [x] Opening or refreshing directive activity does not insert its contents into the main-chat transcript or model context.
- [x] Multiple views of the same directive resolve one workspace record rather than copying state.
- [x] Activity rendering uses the established managed view and interaction behavior rather than introducing a second UI framework.
- [x] Tests cover opening from source and the workspace list, refreshing after a request edit, source navigation, and an empty Ready directive.

## Answer

Resolved by commit `df858f8`.
