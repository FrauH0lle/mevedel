# Keep directive identity outside source overlays

Status: accepted. Activity presentation follows [ADR 0091](0091-render-directive-turns-in-the-shared-session-view.md).

## Current decision

Directives and their activity are durable workspace records. An overlay presents
an attached source anchor; losing it does not destroy the directive. References,
which own no activity or recovery entry point, remain source-bound and disappear
with their complete anchor or source file. Creating a directive requires a
file-visiting buffer so its durable anchor has a valid file identity.

Partial edits resize the anchor. Deleting its whole region creates a detached
zero-width anchor with one compact actionable visual row; co-located detached
rows remain ordered and unfolded. Source-file deletion marks the source missing.
When it returns, only an exact unambiguous match reattaches automatically;
otherwise reattachment is explicit. Source presentation contains a short
lifecycle/outcome label; the shared view and read-only inspector expose answers,
patches, and history. See [directive views](../view.md#directive-turns-and-inspector).

Nested directives belong to their topmost parent rather than owning independent
activity. Prompt construction uses those durable records even if source deletion
removed their overlays. Successful implementation consumes submitted nested
details; failure and abort leave them available.

A directive without activity can be removed. Archive hides an active-list/source
presentation while retaining inspectable, restorable activity and execution
checkpoint links. There is no permanent activity-deletion command. Implementation
attempts name their execution session and turn without transferring directive
ownership into that session.

## Rationale and consequences

An implementation can delete its own source and must remain inspectable and
rewindable afterward. Overlay-owned identity would lose precisely that recovery
entry point. Permanent activity deletion would also need rules for broken
session links and Rewind resurrection.

The instruction registry owns workspace buckets, IDs, lookup, and links;
`mevedel-directive-source.el` coordinates durable records and presentations;
overlay UI owns actions/redraw; overlay core owns geometry and prompt context.
This localizes record/presentation consistency instead of distributing it across
callers.

## Decision history

ADR 0087 established workspace ownership and originally placed substantive
activity on a dedicated activity surface. ADR 0091 replaced that placement with
first-class shared-session turns plus a read-only record inspector; ownership,
source-loss recovery, and archival decisions remain in force.

Source-deletion and nested-detail failures motivated separating registry,
source coordination, UI, and overlay geometry. A fileless directive could also
be archived into a shape the codec rejected, making the whole workspace list
unloadable. Requiring a visiting source file rejects that invalid identity at
creation instead of losing work on a later save/load. Separate incident dates
were not recorded.
