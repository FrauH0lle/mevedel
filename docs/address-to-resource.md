# Address-to-resource

Resource addresses are the model-facing way to name Mevedel-owned content
through the existing filesystem-shaped tools. They are consumed by `Read`,
`Glob`, `Grep`, and `ApplyPatch` only where the operation matrix below allows
it; they do not add another model tool or replace ordinary target-native paths.

## Vocabulary

**Resource locator** is the canonical identity of a selected resource. It is
shared by atomic mention bindings and resource resolution, and names neither
content nor permission.

**Resource address** is the context-qualified `scheme://` serialization of a
locator for a model tool argument. An address may be exact, stable only in its
own root session, or a dynamic discovery query. A resource address is not a
mention operation, a resource grant, a target-native path, or an MCP-native
resource URI.

## Supported families

| Address family | Canonical forms | Read | Glob | Grep | ApplyPatch |
| --- | --- |:---:|:---:|:---:|:---:|
| Working files | `work://`, `work://RELATIVE-PATH`, `work://shared/RELATIVE-PATH` | yes | yes | yes | yes |
| Persisted output | `artifact://`, `artifact://HANDLE` | yes | yes | yes | no |
| Skill package | `skill://NAME@SOURCE-KEY[/RELATIVE-PATH]` | yes | yes | yes | no |
| Retained agent | `agent://`, `agent://root/PATH[#POINTER]` | yes | no | no | no |
| Conversation history | `history://`, `history://root`, `history://root/PATH` | yes | no | concrete root/path | no |
| Saved workspace conversations | `history://saved[/SESSION[/SEGMENT]]` | yes | yes | yes | no |
| Persistent memory | `memory://root`, `memory://ROOT-KEY/RELATIVE-PATH` | yes | yes | yes | explicit file descendants |
| Workspace journal | `memory://journal/`, `memory://journal/FILE` | yes | yes | yes | no |
| MCP resource | `mcp://`, `mcp://ENCODED-SERVER`, `mcp://ENCODED-SERVER/ENCODED-URI` | yes | no | no | no |
| Shared item | `shared://`, `shared://ID[/PART]`, `shared://library[/NAME][/sheet.png]` | yes | no | text views | no |
| Packaged documentation | `mevedel://`, `mevedel://RELATIVE-PATH` | yes | yes | yes | no |

## Prompt availability

The main and built-in agent prompts render a compact request-time roster.
`mevedel://` is always advertised because packaged documentation needs no
session. A valid request session advertises `work://` and `artifact://` as
normal session capabilities: local state is materialized on its first write,
and artifact output may arise during the request, so neither family requires
an existing save path. The remaining families are advertised only when the
current resource metadata has a usable surface:

- `skill://` requires at least one enabled, discoverable skill;
- `agent://` requires at least one retained agent record;
- live `history://root[/PATH]` requires a live root conversation or a retained
  agent conversation; `history://saved` requires the request's workspace;
- `memory://` requires at least one configured memory root; a first permitted
  file write can create a missing directory;
- `memory://journal/` requires at least one validated published entry in the workspace;
  private pending state alone does not qualify;
- `mcp://` requires at least one configured MCP server; and
- `shared://` requires at least one shared whiteboard or document in the
  session.

With no valid request session, the roster still contains `mevedel://` but no
session-owned families. The roster reports request-time availability metadata; it does not itself
authorize an operation or replace resolver validation. It does not change the operation matrix, permissions, or lifecycle rules below.

Unsupported scheme/operation pairs fail explicitly. Ordinary target-native
filesystem paths retain their existing operation behavior. A bare address is a
listing only where the family defines one; it is never an implicit attachment,
invocation, or mutation.

## Canonical addresses and locator classes

Only the eight `scheme://` prefixes above are internal addresses. An
unknown `scheme://` prefix, malformed known address, traversal, or containment
failure is a validation error and is not treated as a filesystem path. Other
strings containing a colon remain ordinary tool input.

Diagnostics identify the authored address and the actual reason. Unsupported
operations are described as unsupported operations, rather than malformed
addresses. Missing explicit files remain failures; missing resource owners,
unknown selections, unreadable storage, and results that are not ready have
distinct messages. Discovery guidance names the appropriate resource listing.
Private storage paths stay out of model-visible errors, including helper
failures. Internal execution-contract failures are identified as internal errors.

Canonical serialization uses UTF-8 RFC 3986 percent encoding: leave only
unreserved bytes literal and use uppercase hexadecimal escapes. For
path-oriented families, split on literal `/` before decoding each component
once. Reject malformed or noncanonical escapes, empty interior components,
decoded separators, NUL, `.`, `..`, and absolute components. A display name
never replaces the authoritative identity.

The address forms have these identity rules:

- `work://` discovers both working scopes. Descendants are session-relative,
  except `work://shared/...`, which is relative to the current workspace root.
- `artifact://` handles are session-relative names derived from existing
  persisted output; there is no artifact index or generated ID allocator.
- `skill://NAME@SOURCE-KEY` uses the full lowercase SHA-256 digest of the
  canonical skill source key. `NAME` labels the source; the digest is the
  authority. Readable aliases are `skill://local-mevedel/SKILL`,
  `skill://local-agents/SKILL`, `skill://global-mevedel/SKILL`,
  `skill://global-agents/SKILL`, `skill://bundled/SKILL`,
  `skill://managed/SKILL`, and `skill://plugin/PLUGIN/SKILL`, each with optional
  descendants. An alias resolves to the exact current full-hash locator without
  changing the authored address shown in transcripts, errors, or results.
- `agent://root/PATH` and `history://root/PATH` name the canonical retained
  agent path without its leading slash. Caller-relative paths and opaque
  storage IDs are rejected. `history://root` names the owning session's main
  agent conversation; it is session-relative and does not make `agent://root`
  a valid result address.
- `history://saved[/SESSION[/SEGMENT]]` is workspace-relative. SESSION is a
  returned saved-session directory name and SEGMENT a canonical
  `segment-NNNN.chat.org` name. It does not accept retained agent paths or
  arbitrary files inside session storage.
- `memory://root` is a dynamic union/index query. A listed topic uses its
  root's key in `memory://ROOT-KEY/RELATIVE-PATH`. A configured `.mevedel` or
  `.agents` root that is the only one of its kind uses the readable key
  `local-mevedel`, `local-agents`, `global-mevedel`, or `global-agents`; every
  other root, and any readable key claimed twice, uses the root's full
  lowercase SHA-256 digest. Both key forms resolve to the same root, and the
  digest remains the authority a readable key resolves to.
- `mcp://ENCODED-SERVER/ENCODED-URI` encodes the complete server name and
  native resource URI as separate components. Internal slashes, colons,
  fragments, percent signs, spaces, and Unicode are encoded.
- `mevedel://RELATIVE-PATH` names a packaged Markdown document relative to the
  installed `docs/` root. Bare `mevedel://` is a dynamic documentation listing.

Exact locators may be atomically bound and later resolve current content.
Session-relative locators are stable only within their owning root session.
Bare scheme listings and `memory://root` are dynamic discovery queries and
must not become bound target identity.

Agent JSON extraction uses the first literal `#` as a URI fragment. Decode the
fragment once as UTF-8 URI data, then apply RFC 6901 `~1` and `~0` token
decoding. No fragment returns the complete payload as ordinary text. An
explicitly empty fragment selects the complete parsed JSON value. The complete
selected agent payload must be one valid JSON value after surrounding
whitespace is ignored; fenced Markdown and heuristic selectors are not JSON.
Missing pointers are distinct from JSON `null`. Scalars render as readable
text and arrays or objects as deterministic JSON.

## Shared resolution and permission seam

The resolver has two stages. Preparation receives the authored operation,
resource operands, options, and session context. It validates and resolves
the address into an opaque attempt plus only the logical authority facts the
permission pipeline needs; it does not read resource content. After
authorization, execution consumes that attempt without reparsing the authored
address and returns the logical operation result.

Preparation happens after deterministic input repair, final validation,
`PreToolUse`, and validation of any hook rewrite, but before permission,
snapshots, helper execution, patch review, or the handler. A malformed known
address, unknown scheme, unsupported operation, traversal, or containment
failure stops at validation with no permission or post-use hook. A valid but
currently missing, disconnected, stale, or unreadable target reaches the
authorized handler and follows the ordinary tool-failure path.

The authored address is retained in errors, render headings, listings, search
results, truncation guidance, and persisted tool arguments. Backing paths,
helper roots, virtual loaders, and mutation mappings remain behind the opaque
attempt. One mixed ordinary/local `ApplyPatch` remains one atomic proposal and
one review transaction. An address never creates a filesystem grant,
broadens roots, authorizes another tool, or bypasses permission mode.

## Family contracts

### `work://`

`work://` exposes working files through Read, Glob, Grep, and ApplyPatch.
Bare Read lists both scopes and bare Glob/Grep searches both. Directory operands
use canonical names without a trailing slash, such as `work://shared`; the
`work://shared/` spelling denotes the prefix for its file descendants.

An unused shared root is a successful empty discovery result for Read, Glob,
and Grep. The result explains that an authorized ApplyPatch can create a file
under `work://shared/`; discovery itself never creates storage. Bare `work://`
lists the union without claiming the entire union is empty when only the
session scope is empty. A missing session or workspace owner is still an error.
An explicit missing descendant, including an arbitrary directory descendant,
is an error rather than an empty discovery root.

- `work://shared/...` maps to `WORKSPACE-ROOT/.mevedel/shared/`. All agents and
  sessions in that workspace see the same files. The directory is created only
  by a permitted write and has no prescribed children. Working notes, drafts,
  findings and handoffs default here: search before creating and update relevant
  existing files. Separate workspaces have separate shared areas. Worktree
  sessions created through mevedel retain the parent workspace and share its
  files; a worktree opened as a separate workspace has its own shared area.
  Use `work://shared/...` to address the workspace's files: a literal relative
  `.mevedel/shared/...` path follows the session's working directory instead.
  There is no cross-project publication mechanism.
- Other descendants map to the session's lazily materialized `local/` directory.
  The parent and retained agents share this session scope. First write establishes
  durable session persistence. Save, resume and rename retain it; Fork copies it
  independently, Rewind leaves it unchanged, and session cleanup removes it.
  Ephemeral requests cannot create or mutate session-owned work files.

Shared files survive session rename, fork, rewind and deletion, because they
are workspace-owned. They are mutable working material, distinct from curated
`memory://` and immutable dated `memory://journal/` evidence. Neither scope participates
in source snapshots, touched-file tracking, diagnostics or directive patch
capture. Shared writes use the real backing path for normal filesystem edit
permissions; they receive no session scratch exception in Plan mode.

Shared note contents are not automatically added to every conversation. Models
discover and read them with Read, Glob, and Grep; an agent handing off work
provides the note's address to the receiving agent.

`/clean-work [focus]` explicitly reviews shared files and removes confirmed
obsolete copies through normal ApplyPatch permissions and review. It preserves
unresolved work and does not infer obsolescence from age or session deletion.
There is no background expiry timer; `/learn` handles durable promotion.

The physical session directory remains named `local/` to describe its ownership.

The `local/plans/` subtree is shared by the parent and retained agents for
current and accepted plans. Working notes, findings, contracts and handoffs
default to `work://shared/`. It is addressed as `work://plans/...`.
Accepted archives always use canonical `accepted-TIMESTAMP.md` names, so every
managed plan is addressable. `local/plans/` is also the one part of `local/`
that a Fork does not copy verbatim: the child keeps only the artifact already
accepted at the fork point, after its recorded hash is re-verified.

### `artifact://`

Artifacts are a read-only logical view over existing session-owned persisted
tool results and retained output from yielded executions. The empty address
lists current handles; descendants resolve only existing artifacts. A yielded
execution may still append to its spool: each `Read` observes a bounded
snapshot of bytes available when that read begins, while later pagination may
observe growth. A foreground execution that has not yielded is not listed.

Before the first result is persisted, bare Read, Glob, and Grep return successful
empty discovery results. Discovery does not create session storage. An explicit
missing handle is a missing-target error, and a missing session is an owner error.

Oversized tool and execution notices emit followable `artifact://` addresses,
never absolute session-storage paths. No artifact address can rewrite,
truncate, or otherwise mutate the captured evidence.

### `skill://`

Skill addresses are read-only and resolve the exact discovered source named by
the source-key digest. Resolution verifies the current discovery entry and
does not fall back to a different same-named skill. Hot reload may change
content at that source without retargeting the address. Missing, disabled,
changed, or no-longer-discoverable sources fail as unavailable; package
descendants remain contained by the selected skill root.

Bare skill listings expose both readable origin aliases and exact full-hash
addresses. Aliases are conveniences for the currently discovered source: the
resolver still authorizes the resulting exact source identity, and a missing
or conflicting origin/name pair does not fall back to another skill.

### `agent://` and `history://`

Bare `agent://` lists retained agent results in path order. Bare `history://`
lists the live root conversation and available retained agent conversations
in the same order. Root history is available even when there are no retained
agents. Listings and completion inspect availability metadata only; they do
not read transcripts or create sessions.
When a workspace is available, bare history also links to `history://saved`.

An agent address returns the complete payload and terminal outcome of its
latest settled turn. Completed, errored, and interrupted turns expose the same
visible final or partial payload used by the agent result contract. Active
agents expose no streaming text and are reported as not ready. JSON selection
is read-only and cannot alter the conversation, settled result, or `RESULT`
mailbox.

`history://root` renders the session's registered root data buffer, including
unsaved transcript content. A retained agent reading this address gets its
owning session's root conversation, regardless of its caller buffer. A missing
or killed root buffer is unavailable; reading does not resume a session.
Authorized execution rechecks the current root buffer after permission.

`history://root/PATH` renders the retained conversation through the same
transcript classification and source mapping. Live and resumed conversations
use the same concise Markdown projection, excluding hidden audit encoding,
provider bookkeeping, and persistence scaffolding. History is observational
and cannot rewrite the transcript. Both addresses retain the existing tool
output truncation and Read offset/limit behavior. They project the current
conversation representation, without traversing pre-compaction archives.
Grep accepts either concrete address and searches exactly that Read projection,
including unsaved content, with matching line references. Bare `history://`
is a Read-only discovery listing. A directive or shared-item conversation can
retrieve relevant parent decisions this way without including the entire room
in every request. The resource catalog advertises availability; the model
chooses a search or read when the task needs it. Addresses grant neither new
permissions nor cross-session access.

`history://saved` searches saved conversations across the current workspace.
Read or Glob lists sources; adding a returned SESSION narrows discovery, and
adding its SEGMENT selects a single saved source. Read of that source returns
the filtered transcript, while Grep searches its projected text. Saved access
includes archived segments, needs no live view buffer, and never resumes a
session. Read and Grep line references address the same projection rather than
raw Org file offsets. Search result order is unspecified. Unsaved changes remain
available only through live history.

After authorization, preparation runs in cooperative steps: discovery, pinned
batch reads, canonical bounds restoration and classification, then temporary
file preparation. Ordinary search helpers operate on those client-local files.
Read also uses an asynchronous handler. Cancellation settles once with a
cancelled outcome; owner teardown suppresses delivery. Both release the iterator,
temporary files and any search helper. Individual filesystem operations and
classification steps still execute synchronously between yields.

Every operation observes current session authority. Portable discovery follows
only a committed, validated publication and verifies artifact bytes before
projection reuse. PID sidecars are also read in a fresh pinned batch and passed
through the native schema classification; previous summary fingerprints do not
stand in for those reads. A disposable 64 MiB projection payload cache is keyed by
source identity and freshly read SHA-256; it is neither an authority cache nor
a second permanent transcript store. Reuse cannot substitute for validation.
Canonical filtering excludes hidden notes, reasoning and provider/bounds
metadata while preserving ordinary user examples and useful tool evidence.
Discovery omits unavailable or incompatible session records and reports their
count with the result. Failed verification of selected transcript bytes is an
error rather than a successful empty search.
Saved-history access does not repair corrupted stored bounds.

### `memory://`

`memory://root` is a read-only, dynamic union of configured persistent-memory
roots. Its bare read uses the same ordered and labeled index content as the
system prompt. `Glob` and `Grep` inspect every existing configured root; collision
precedence remains the existing local/global and `.mevedel`/`.agents` policy,
while search results disclose root-bound addresses and source labels for
shadowed matches. Listed topics and disclosed search results use the readable
root key when the configured roots make it unambiguous, and the digest key
otherwise; the union query itself is never atomically bound.

An empty union returns plain text for Read, Glob, and Grep, with zero search
results. No configured roots and configured roots without stored files are
reported separately. Resolver descriptors never become public result values.

Memory reads are fresh against the configured roots. ApplyPatch accepts only
explicit `memory://ROOT-KEY/RELATIVE-PATH` file descendants; neither the union nor
a root-only address is writable. Add, Update, Move and Delete use the same
transaction, review and conflict checks as ordinary patches. Filesystem edit
permissions inspect the actual backing path, including protected paths and
out-of-workspace global roots. A prepared shared or memory write fails if its
owning root changes before execution; the previous approval cannot authorize a
new destination. Authored addresses remain in patch results and review headings.

### `memory://journal/`

`journal` is a reserved branch of the memory scheme, distinct from configured
curated-memory root keys. Its root spelling includes the final slash; descendants
name one public entry.

The workspace owns `.mevedel/journal/` on its execution target. Root Read lists
validated published records within the ordinary recall age limit, newest first; an exact filename reads the full
public Markdown with ordinary Read pagination. Timestamp colons use canonical
`%3A` encoding in addresses. Private `.mevedel/state/journal/`, nested names, traversal, malformed
records, and symlink escapes are excluded. ApplyPatch is unsupported.
Completed consolidation reviews are public records alongside digests; their
metadata names examined digest IDs, focus, reference checks, and proposal IDs.
Private proposal bodies and captured memory before-state are not public entries.
The default 14-day limit applies from immutable entry creation to exact reads,
search, discovery and completion, including prepared or cached access. Retention
for unfinished review, proposals or recovery does not extend ordinary recall.
Internal review/recovery and human inspection retain their storage access.
Accepted expiry markers also exclude entries immediately, including when
physical deletion is interrupted. Private capture coverage survives expiry
without exposing old digest text through this address family.

Glob and Grep use the ordinary options over freshly validated public document
snapshots. The existing managed search helper processes the transferred snapshot
locally, including for remote workspaces, and returns canonical journal addresses.
Snapshots are deleted on completion, failed admission, or helper teardown. No
private journal directory enters the helper's read scope. An explicit workspace
supports these operations without constructing a session.

Prompt discovery observes published entries at most once every ten seconds;
local digest publication invalidates that observation. External additions and
removals become visible at the next eligible observation. Completion uses only
previously observed filenames, with no filesystem access. Both check age on
every use, so an observation does not extend the ordinary recall period. An initially cold
composer can still insert `memory://journal/`; request-time discovery populates its
descendants. Authorized operations always validate current storage, independently
of the discovery observation and whether automatic capture is enabled.

### `shared://`

Shared items are the session's collaborative whiteboards and documents. Bare
`shared://` lists them with `shared://library`. An item address is
session-relative: `shared://ID` reads its overview of hashed element or block
lines, and its parts are `view.png`, `comments`, `history`,
`elements/ELEMENT` and `images/KEY`. `shared://library` lists element library
items, `shared://library/NAME` one library, and `sheet.png` beneath either
renders their numbered sheet. Item, element and image identities match
`[a-zA-Z0-9_-]{1,80}`; any other shape is refused before execution, and an
item missing from the session is unavailable.

Preparation, authorization and availability are synchronous. The content is
computed by the session's editing host, which answers asynchronously: the
resolver returns a descriptor naming the components, and Read and Grep fetch
the view through `mevedel-tool-editing-view` before paging or searching it.
Text follows the ordinary virtual-text Read bounds; images are written to a
private temporary copy and delivered through ordinary Read media handling.
Grep accepts text views only. Writes go through `SharedEdit`, never through a
resource address. [Shared editing](shared-editing.md) owns the view formats.

### `mcp://`

Bare `mcp://` lists configured servers and availability. A server-only
address lists resources advertised by the current connected server. A complete
address passes the decoded native URI unchanged to the same configured-server
read interface used by `@mcp` expansion.

MCP reads are fresh on every call and use the current connection. Unknown,
disconnected, or failed resources return no content and follow ordinary tool
failure handling. MCP addresses are read-only and create no filesystem grant,
cache, or server mutation route.

Read exposes MCP text content. A successful response without content is
distinguished from a response containing no usable text, and both notices name
the requested address. Failures also name that address and preserve the server
or provider reason.

### `mevedel://`

`mevedel://` is a read-only view of the Markdown files packaged under `docs/`.
It is available without a session. Bare Read returns a sorted recursive listing
of canonical documentation addresses; descendant Read, Glob, and Grep stay
contained under the packaged documentation root and report logical addresses.
Each operation observes the installed files at execution time.

The documentation owner uses the same source-aware package directory as system
and tool prompts. When loading bytecode, an existing sibling `.el` file identifies
that directory through its real path; otherwise the loaded library's real
directory is used. This supports package managers such as Straight, which keep
regular compiled files beside source symlinks in a separate build tree, as well
as source-less installations. Resolving the package owner does not authorize
symlinked resource descendants: the usual documentation containment checks still
apply below `docs/`.

The family exposes no package source files, generated source index, compression
layer, registry, or aliases beyond the relative documentation paths. It cannot
be used with ApplyPatch.

## Mentions and addresses

Mentions remain user-facing operations, not alternate address syntax:

- `@file` and `@mcp` attach current content to a prompt;
- `$skill` invokes instructions; and
- `@agent` requests delegation.

Where they identify the same file, skill source, or MCP server/URI, mentions
and resource addresses share the canonical locator, target lookup, freshness,
and authority decisions. Atomic bindings retain the exact locator as hidden
text properties through drafts, queues, retries, history, and persistence.
Binding preserves identity, not contents or approval. Model-visible addresses
are plain text serializations and never depend on Emacs text properties.

Resource addresses do not attach content, invoke a skill, delegate work, or
replace `@agent` semantics. Dynamic discovery queries remain unbound. Adding a
new binding kind requires an explicit schema and lifecycle branch; there is no
generic binding registry or migration.

## Completion and side effects

Composer completion offers scheme prefixes first, then bounded
scheme-specific descendants. Candidates may show kind, display name, origin,
and known availability, but insertion always writes plain canonical address
text. Completion does not read content, bind a mention, attach context, invoke
a skill, delegate an agent, make a network request, materialize a session, or
change durable state. MCP completion uses metadata already held by the current
connection and never starts or refreshes a connection.
Where a family has both a readable and a hashed key for the same resource,
completion offers only the readable one: `skill://` exposes a skill's exact
`NAME@SOURCE-KEY` locator once the typed tail already contains `@`, or when
the skill has no unique alias, and `memory://` offers a root's digest key only
when it has no unique readable key. Both hashed forms remain valid input.

Once a scheme prefix is present, completion constructs only that scheme's
metadata. Remote backing roots are not enumerated during completion; their
bare prefix remains usable until an explicit resource operation resolves it.

Addresses in the transcript view are buttons (see [the view](view.md)).
Rendering only recognizes their spelling; a click resolves the address through
the same Read preparation, with the same validation and containment, and
`mevedel-resource-visit-path` returns the backing file of a file-backed
address. No attempt outlives the click.

## Execution target and Plan mode

Session-owned `work://` descendants, `artifact://`, `agent://`, and live `history://root[/PATH]` resources
belong to the current session's execution target. Their addresses cannot cross
sessions or targets. `history://saved` spans saved sessions in the current
workspace's storage, without changing execution target or selecting another
workspace. Validated projections are searched locally. Client-local skills and memory roots retain their origin;
their client pathname is not reinterpreted as a target-native workspace path.
MCP authority remains with the configured connection. No address changes the
session's target or turns a local path into cross-target authority.

Standalone/sticky Plan mode keeps session-only `ApplyPatch` available, including
calls from retained agents, so plans and other durable local artifacts can be
updated through the ordinary `ApplyPatch` path. Before materialization, the
pipeline denies any proposal with an ordinary, shared, memory, or bare endpoint:
mixed local/ordinary and ordinary-only proposals are denied tree-wide, and no
local directory or ordinary target is touched. Permission mode and allow rules
cannot widen that boundary. Other edit tools and `Eval` remain unavailable;
resource recognition does not reopen those capabilities. Directive Planning
has a separate strictly read-only boundary and does not allow `ApplyPatch`,
including session-only proposals, or `Eval`.

See [`tools.md`](tools.md#resource-addresses-in-filesystem-shaped-tools),
[`mentions.md`](mentions.md#atomic-binding-lifecycle),
[`agents.md`](agents.md#agent-resource-results), and
[`sessions.md`](sessions.md#session-owned-local-state) for subsystem
contracts. The closed resolver and capability boundary are recorded in
[`ADR 0104`](adr/0104-keep-resource-addresses-closed-and-capability-neutral.md).
