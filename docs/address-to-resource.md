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
| Session scratch | `local://`, `local://RELATIVE-PATH` | yes | yes | yes | yes |
| Persisted output | `artifact://`, `artifact://HANDLE` | yes | yes | yes | no |
| Skill package | `skill://NAME@SOURCE-KEY[/RELATIVE-PATH]` | yes | yes | yes | no |
| Retained agent | `agent://`, `agent://root/PATH[#POINTER]` | yes | no | no | no |
| Retained history | `history://`, `history://root/PATH` | yes | no | no | no |
| Persistent memory | `memory://root`, `memory://ROOT-KEY/RELATIVE-PATH` | yes | yes | yes | no |
| MCP resource | `mcp://`, `mcp://ENCODED-SERVER`, `mcp://ENCODED-SERVER/ENCODED-URI` | yes | no | no | no |

## Prompt availability

The main, tutor, and built-in agent prompts render a compact request-time
roster. A valid request session advertises `local://` and `artifact://` as
normal session capabilities: local state is materialized on its first write,
and artifact output may arise during the request, so neither family requires
an existing save path. The remaining families are advertised only when the
current resource metadata has a usable surface:

- `skill://` requires at least one enabled, discoverable skill;
- `agent://` and `history://` require at least one retained agent record;
- `memory://` requires at least one configured memory root directory that
  exists; and
- `mcp://` requires at least one configured MCP server.

With no valid request session, the roster contains no session-owned families.
An omitted family is not usable in that request and must not be inferred from
the closed family list above. The roster does not change the operation matrix,
permissions, or lifecycle rules below.

Unsupported scheme/operation pairs fail explicitly. Ordinary target-native
filesystem paths retain their existing operation behavior. A bare address is a
listing only where the family defines one; it is never an implicit attachment,
invocation, or mutation.

## Canonical addresses and locator classes

Only the seven exact `scheme://` prefixes above are internal addresses. An
unknown `scheme://` prefix, malformed known address, traversal, or containment
failure is a validation error and is not treated as a filesystem path. Other
strings containing a colon remain ordinary tool input.

Canonical serialization uses UTF-8 RFC 3986 percent encoding: leave only
unreserved bytes literal and use uppercase hexadecimal escapes. For
path-oriented families, split on literal `/` before decoding each component
once. Reject malformed or noncanonical escapes, empty interior components,
decoded separators, NUL, `.`, `..`, and absolute components. A display name
never replaces the authoritative identity.

The address forms have these identity rules:

- `local://` is relative to the current durable root session.
- `artifact://` handles are session-relative names derived from existing
  persisted output; there is no artifact index or generated ID allocator.
- `skill://NAME@SOURCE-KEY` uses the full lowercase SHA-256 digest of the
  canonical skill source key. `NAME` labels the source; the digest is the
  authority. Descendants stay inside that package.
- `agent://root/PATH` and `history://root/PATH` name the canonical retained
  agent path without its leading slash. Caller-relative paths and opaque
  storage IDs are rejected.
- `memory://root` is a dynamic union/index query. A listed topic uses its
  root's full lowercase SHA-256 key in `memory://ROOT-KEY/RELATIVE-PATH`.
- `mcp://ENCODED-SERVER/ENCODED-URI` encodes the complete server name and
  native resource URI as separate components. Internal slashes, colons,
  fragments, percent signs, spaces, and Unicode are encoded.

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

### `local://`

The session owns a lazily materialized `local/` directory. The empty address
lists it; descendants are relative files beneath it. The root and every
retained agent in the tree share the directory. Local content is available to
`Read`, `Glob`, and `Grep`; `ApplyPatch` is the only mutation surface and may
combine local and ordinary targets atomically.

The first local write materializes durable session persistence. Local state
survives save, resume, and session rename. Session Fork copies it into an
independent child; Rewind leaves it unchanged; session cleanup removes it with
the owner. Local files are excluded from workspace snapshots, touched-file
tracking, instruction discovery, LSP diagnostics, directive patch capture,
and Git summaries. An ephemeral request without durable session ownership may
inspect already available read-only resources but cannot create or mutate
`local://` state.

The `local/plans/` subtree is shared by the parent and retained agents for
current and accepted plans, alongside durable notes, findings, contracts, and
handoffs. It is addressed as `local://plans/...`; there is no compatibility
migration from a separate top-level plans directory or older plan format.
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

### `agent://` and `history://`

Bare `agent://` and `history://` are path-sorted listings from the explicit
retained-agent registry. Historical transcript files without an addressable
retained identity are not listed.

An agent address returns the complete payload and terminal outcome of its
latest settled turn. Completed, errored, and interrupted turns expose the same
visible final or partial payload used by the agent result contract. Active
agents expose no streaming text and are reported as not ready. JSON selection
is read-only and cannot alter the conversation, settled result, or `RESULT`
mailbox.

`history://root/PATH` renders the retained conversation through the existing
transcript classification and source mapping. Live and resumed conversations
use the same concise Markdown projection, excluding hidden audit encoding,
provider bookkeeping, and persistence scaffolding. History is observational
and cannot rewrite the transcript.

### `memory://`

`memory://root` is a read-only, dynamic union of configured persistent-memory
roots. Its bare read uses the same ordered and labeled index content as the
system prompt. `Glob` and `Grep` inspect every configured root; collision
precedence remains the existing local/global and `.mevedel`/`.agents` policy,
while search results disclose root-bound addresses and source labels for
shadowed matches. Listed topics use stable root keys, but the union query
itself is never atomically bound.

Memory reads are fresh against the configured roots. Addressing memory does
not choose a write scope or resource family; memory mutation continues through
explicit filesystem targets and existing memory policy.

### `mcp://`

Bare `mcp://` lists configured servers and availability. A server-only
address lists resources advertised by the current connected server. A complete
address passes the decoded native URI unchanged to the same configured-server
read interface used by `@mcp` expansion.

MCP reads are fresh on every call and use the current connection. Unknown,
disconnected, or failed resources return no content and follow ordinary tool
failure handling. MCP addresses are read-only and create no filesystem grant,
cache, or server mutation route.

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

## Execution target and Plan mode

Session-owned `local://`, `artifact://`, `agent://`, and `history://` resources
belong to the current session's execution target. Their addresses cannot cross
sessions or targets. Client-local skills and memory roots retain their origin;
their client pathname is not reinterpreted as a target-native workspace path.
MCP authority remains with the configured connection. No address changes the
session's target or turns a local path into cross-target authority.

Standalone/sticky Plan mode keeps all-local `ApplyPatch` available, including
calls from retained agents, so plans and other durable local artifacts can be
updated through the ordinary `ApplyPatch` path. Before materialization, the
pipeline denies any proposal with an ordinary, non-local, or bare endpoint:
mixed local/ordinary and ordinary-only proposals are denied tree-wide, and no
local directory or ordinary target is touched. Permission mode and allow rules
cannot widen that boundary. Other edit tools and `Eval` remain unavailable;
resource recognition does not reopen those capabilities. Directive Planning
has a separate strictly read-only boundary and does not allow `ApplyPatch`,
including all-local proposals, or `Eval`.

See [`tools.md`](tools.md#resource-addresses-in-filesystem-shaped-tools),
[`mentions.md`](mentions.md#atomic-binding-lifecycle),
[`agents.md`](agents.md#agent-resource-results), and
[`sessions.md`](sessions.md#session-owned-local-state) for subsystem
contracts. The closed resolver and capability boundary are recorded in
[`ADR 0099`](adr/0099-keep-resource-addresses-closed-and-capability-neutral.md).
