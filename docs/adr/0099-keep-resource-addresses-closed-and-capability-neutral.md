# Keep resource addresses closed and capability-neutral

Status: accepted

Mevedel exposes one closed resolver for `local://`, `artifact://`, `skill://`,
`agent://`, `history://`, `memory://`, and `mcp://`. A resource address is a
plain serialization of a canonical locator, not a grant: preparation validates
and resolves an opaque attempt plus logical authority facts before permission,
and authorized execution consumes that attempt without reparsing or exposing
backing paths. Existing `Read`, `Glob`, `Grep`, and reviewed `ApplyPatch`
surfaces keep their operation-specific capabilities. Standalone and sticky Plan
mode permit `ApplyPatch` only when every source and destination operand is a
non-bare `local://` descendant, so durable plans and notes stay editable while
workspace mutation remains denied tree-wide; Directive Planning stays fully
read-only.

This rejects a public scheme registry, one model tool per resource kind,
generic caching, URL/path fallbacks, and address-driven permission grants.
Keeping the dispatch closed makes the trust boundary auditable, preserves each
resource's existing freshness and persistence owner, and prevents a copied
address from silently acquiring authority or changing execution target.
