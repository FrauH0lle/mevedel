# Keep resource addresses closed and capability-neutral

Status: accepted

## Current decision

Mevedel exposes one closed resolver for `work://`, `artifact://`, `skill://`,
`agent://`, `history://`, `memory://`, `mcp://`, and `mevedel://`. An address
serializes a canonical locator; it grants no authority. Preparation validates
an opaque attempt and logical authority facts before permission. Authorized
execution consumes that attempt without reparsing or exposing private backing
paths. Existing Read, Glob, Grep and ApplyPatch retain operation-specific
capabilities. The [resource manual](../address-to-resource.md) owns address
syntax, supported operations, selectors, completion and failure behavior.

Freshness and persistence remain resource-specific. Session-owned working files
and workspace-owned `work://shared/` files use the same family but have distinct
lifetimes. Shared files survive cleanup of an individual session; separate
workspace roots remain isolated. Explicit memory-root descendants are writable
through the native patch transaction, while `memory://journal/` exposes only
validated, temporary, read-only evidence. Saved conversation search uses
`history://saved`. Concrete live `history://root[/PATH]` supports Read and Grep
over the same projected text, including unsaved turns. Scoped item and directive
conversations use it for deliberate retrieval of parent decisions; no new scheme
or automatic context import is needed. Bare `history://` remains Read-only
discovery. Curated memory and journal retention are independent of it.

Standalone and sticky Plan permit ApplyPatch only when every source and
destination is a non-bare session-owned `work://` descendant outside
`work://shared/`. This permits durable plans and private notes while preserving
the tree-wide workspace mutation boundary. Shared and memory writes cannot
borrow that exception. Directive Planning remains fully read-only.

Explicit skill-origin aliases resolve to the current exact full-hash locator
while preserving the authored alias in model-visible output. There is no
unqualified name alias. `mevedel://` is always advertised and exposes installed
Markdown documentation through Read, Glob and Grep. It does not expose source
code or a mutation interface. Its owner directory is resolved from an existing
source sibling of the loaded bytecode, or from the loaded library itself when
no source sibling is available. Resource descendants retain their containment
checks; package-manager symlinks identify the owner, not exceptions for content.

Search exclusions follow the logical resource family even after descendant
dispatch resolves a backing directory. Artifact discovery omits unpublished
execution spools, and installed-document discovery remains Markdown-only.
Bare `skill://` supports Read discovery only; searches require a selected
package. Model-facing prepared skills disclose each contribution's exact source
address without changing raw preparation, hook inputs, or collapsed render
bodies. Source disclosure identifies content for inspection; it grants no
authority to read or invoke the skill.

Active retained-agent reads return a successful, explicit NOT READY status,
separate from settled payloads. Valid pagination and JSON fragments cannot hide
the status or expose a previous turn's result; text-only option validation still
applies. Missing agents and inconsistent idle records remain errors. Nonempty
text reads past EOF remain errors with observed line bounds and a concrete
last-page retry in the visible address domain. Ordinary files count those bounds
during the failed traversal, without a second scan; virtual text counts only on
the error path. Neither recovery silently clamps offsets or changes settlement,
publication, containment, permission, or mailbox semantics.

## Rationale and alternatives

A closed dispatch keeps the trust boundary auditable and avoids a tool per
resource kind. A copied address cannot silently acquire permission or change
execution target. A public scheme registry, URL/path fallbacks and address-driven
grants would weaken that boundary. Generic caching would obscure the different
freshness and authority requirements, so disposable caches remain with the
owning resource.

Journal searches use validated document snapshots rather than raw traversal,
which could expose private or malformed records. Authorized operations validate
current storage; prompt discovery and completion may use a disposable,
ten-second workspace observation. That observation cannot authorize access or
decide retention.

Saved-history discovery, pinned reads, projection and search preparation yield
cooperatively after authorization. The native execution lifecycle owns the
search helper. Selected paths avoid whole-workspace discovery, and a bounded
projection cache reuses CPU work only after fresh authority and byte validation.
It creates no second durable transcript store.

Unused discovery roots return successful empty results without materialization.
Missing explicit descendants and unavailable owners remain errors. Diagnostics
retain the authored address and safe causes while distinguishing invalid syntax,
unsupported operations, missing selections, unavailable owners and unavailable
content. Shared and memory patch operations expose prepared paths only to
filesystem permission policy and reject root rebinding.

## Consequences

The model uses familiar file tools across resources but must still respect each
operation's supported families and each resource's scope. The resolver absorbs
backing-path mechanics; callers retain permission, review and cancellation
boundaries. Empty discovery does not create storage, and an address is never a
substitute for checking a settled operation's result.

## Decision history

All revisions below refine ADR 0104's closed resolver decision.

- **Local discovery and recovery.** Published session evidence showed descendant
  artifact Glob advertising private execution-spool addresses that Read and
  Grep correctly refused, outdated Grep guidance, bare-skill search retries,
  active-agent reads treated as failures, and EOF guesses without line bounds.
  Logical-family filtering and producer-side source/status/range disclosure
  replace those blind retries. No new tail operation, catalog-wide skill search,
  alias fallback, or permission grant is needed.
- **Mandatory documentation filtering.** A positive caller glob and a positive
  Markdown glob form a union in ripgrep, exposing non-Markdown content and counts
  despite a safe file-list projection. Directory Grep now selects filenames using
  caller filters, admits only contained Markdown files, and searches that set
  within one cancellation and timeout budget. Ordinary-path search is unchanged.
- **Resource search pagination.** Single-root searches applied line limits before
  projecting ripgrep's NUL-delimited filenames, so a two-entry page could return
  every file; content offsets could also discard the only source heading. Logical
  filename projection now precedes pagination, and all resource content matches
  carry a source address, including explicit-file searches. Delivered helper
  startup errors use the same private-path-safe settlement as signalled errors,
  including direct exact-file Grep and Glob helpers. Ordinary filesystem
  searches retain their native startup diagnostics.
- **Compiled installation roots.** Straight's build tree contained regular
  `.elc` files but symlinked source and documentation. Deriving the documentation
  root from the bytecode made valid reads fail containment and discovery return
  no files. Source-aware owner resolution, shared with prompt loading, replaced
  bytecode-directory selection without weakening resource containment.
- **Saved-history responsiveness.** The synchronous prototype blocked an
  independent graphical edit for 3.725 seconds at 100 portable sessions.
  Moving only the final search subprocess off the call stack could not address
  the preceding blocking work. Cooperative discovery, reads, projection and
  preparation replaced that path; resource-local validated projection caching
  avoids repeating classification without weakening authority checks.
- **Shared working scope.** A private scratchpad with explicit publication
  missed two cross-session corrections with Luna. Shared-default working notes
  recovered all six cases; a freeform follow-up recovered all six per model
  without prescribed filenames or directories. This supported one `work://`
  family and unstructured shared-default notes, with shared storage outside
  individual session persistence.
- **Lazy discovery.** A first-use session returned “unavailable” for
  `Read(work://shared)` even though the root was usable. The audit also found
  failing empty artifact searches, an internal empty-memory descriptor leaking
  into the result, and a zero-file result rendered as one file. Successful
  empty discovery replaced those failures; result counts now travel separately
  from explanatory text in render data. Explicit missing selections still fail.
- **Memory writes.** Explicit memory-root descendants gained ApplyPatch through
  the same native transaction and filesystem authority policy. They did not
  gain the session-only Plan exception. No separate measurement explaining the
  timing of this extension was recorded.
- **Journal namespace.** On 2026-09-13, `memory://journal/` replaced the separate
  journal scheme, which added no capability. The change kept journal storage
  and internal recovery authority while reserving a read-only namespace inside
  memory; sharing a scheme did not confer curated-memory write authority. The
  old scheme has no alias.

- **Live history search.** Dedicated item conversations exposed the need to find
  earlier room decisions before they have been saved. Live history previously
  supported only Read. Grep now searches the same projection and line references
  at concrete root/agent addresses; saved-history search remains the route to
  archived segments. This extends an existing resource rather than adding a
  conversation-specific lookup interface.
- **Lenient address spelling.** The resolver accepted only the canonical
  spelling. Across 17 sessions from 2026-09-14 to 2026-10-06, models were
  rejected 27 times for a directory's trailing slash, almost always
  `work://shared/`, the spelling mevedel's own prompts used, and twice for a
  library name written as the listing printed it, with spaces. Each rejection cost a
  round trip and taught nothing, because both spellings have exactly one
  meaning. The resolver now normalizes literal characters, either hex case, and
  one trailing slash to the canonical locator. Identity, containment and the
  rejection of traversal, decoded separators and malformed escapes are
  unchanged.
