# Keep resource addresses closed and capability-neutral

Status: accepted

Mevedel exposes one closed resolver for `work://`, `artifact://`, `skill://`,
`agent://`, `history://`, `memory://`, `mcp://`, and `mevedel://`. A resource
address is a plain serialization of a canonical locator, not a grant:
preparation validates and resolves an opaque attempt plus logical authority
facts before permission, and authorized execution consumes that attempt without
reparsing or exposing backing paths. Existing `Read`, `Glob`, `Grep`, and
reviewed `ApplyPatch`
surfaces keep their operation-specific capabilities. Standalone and sticky Plan
mode permit `ApplyPatch` only when every source and destination operand is a
non-bare session-owned `work://` descendant outside `work://shared/`, so durable plans and notes stay editable while
workspace mutation remains denied tree-wide; Directive Planning stays fully
read-only.

This rejects a public scheme registry, one model tool per resource kind,
generic caching, URL/path fallbacks, and address-driven permission grants.
Keeping the dispatch closed makes the trust boundary auditable, preserves each
resource's existing freshness and persistence owner, and prevents a copied
address from silently acquiring authority or changing execution target.

Skill origin aliases make discovered packages readable without weakening exact
identity: each explicit local, global, bundled, managed, or plugin alias resolves
to the current exact full-hash skill locator, while model-visible output keeps
the authored alias. There is no unqualified name alias. `mevedel://` is the one
always-advertised family and exposes only installed Markdown documentation
through Read, Glob, and Grep; it adds no source browser, compression layer,
registry, mutation path, or extra aliases.

Workspace journals add cross-session evidence without exposing private capture
state. `memory://journal/` therefore admits only validated public records through Read,
Glob, and Grep. Searches operate on validated document snapshots using the
existing search helpers, so a private or malformed file cannot enter results
through raw directory traversal. Authorized operations validate current storage;
only prompt discovery uses a disposable, ten-second workspace observation.
Completion consumes that observation without filesystem access. The observation
does not confer permission or decide retention, and can be discarded at any time.

Saved conversations now use `history://saved[/SESSION[/SEGMENT]]` through the
same Read/Glob/Grep surfaces. The evaluated synchronous prototype blocked an
independent graphical edit for 3.725 seconds at 100 portable sessions, so moving
only the final search subprocess off the call stack was insufficient.
Discovery, pinned reads, canonical projection and disposable-file preparation
now yield cooperatively after authorization; the native execution lifecycle
owns the search helper. Selected source paths avoid whole-workspace discovery.
A bounded, disposable projection cache saves classification work only after
fresh authority and byte validation. This resource-specific CPU reuse does not
introduce a generic resolver cache or another durable transcript store. Journal
retention and curated memory remain independent of history search.

Working files now use `work://` with a workspace-owned `shared/` subtree and
session-owned descendants elsewhere. The scope evaluation found two missed
cross-session corrections with Luna when publication from a private scratchpad
was required; shared-default recovered all six cases. A follow-up freeform run
recovered all six cases per model without prescribed filenames or directories.
This supports one address family and unstructured shared-default working notes.
Shared storage is outside session persistence so cleanup of one session cannot
remove another session's working files. Separate workspace roots remain isolated.

A first-use session exposed a mismatch between lazy storage and discovery:
`Read(work://shared)` reported an unavailable resource before the first write,
although the root was usable. The diagnostic audit also reproduced failed
empty artifact searches, an internal empty-memory descriptor returned to the
model, and a zero-file result rendered as one file. Read/Glob/Grep now treat
unused discovery roots as successful empty results without materializing them.
Missing explicit descendants and unavailable owners remain errors. Search
empty-result counts travel in render data, independently of explanatory text.
Errors identify their authored target and distinguish syntax, unsupported
operations, missing selections, unavailable owners, and content readiness;
underlying safe causes are retained without exposing private backing paths.

Explicit memory-root descendants now admit ApplyPatch through the same native
transaction. Shared and memory writes expose their prepared backing paths only
to filesystem permission policy, retain address presentation, and reject root
rebinding. Neither can borrow the session-only Plan exception. Root discovery
addresses and journal/artifact evidence remain non-writable.

The 2026-09-13 journal lifecycle change reserves `memory://journal/` within the
memory scheme. Journal evidence is temporary and read-only; sharing the scheme
with curated memory does not share its write permission. The separate journal
scheme added no capability and is removed without an alias. Existing journal
storage and its internal recovery authority are unchanged.
