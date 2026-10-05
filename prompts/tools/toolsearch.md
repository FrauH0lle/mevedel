Find specialist tools and retrieve their contracts for ToolCall.

### When to use `ToolSearch`

- Find a specialist better suited to the work, including configured MCP tools.
- Retrieve exact arguments or rediscover a contract after compaction.

Useful search keys, subject to the current role and request:

- `elisp`: running Emacs documentation, symbol/source lookup, Info manuals, and
  runtime values. Static source alone does not establish a current runtime value.
- `code`: symbol outlines, definitions, references, and syntax structure.
- `tasks`: session-visible plans/checklists, status, ownership, and dependencies.
- `agents`: delegate independent work and coordinate retained agents.
- `web`: web search and page retrieval.
- `goal`: create, inspect, or finish a persistent session Goal.
- `shared`: create and edit whiteboards and documents that people and agents
  edit together; Read `shared://` lists them. A requested whiteboard is one of
  these, not an artifact.

### When NOT to use `ToolSearch`

- A known, appropriate tool already suffices; discovery would add no useful value.

### How to use `ToolSearch`

- `query` matches any whitespace-separated term, case-insensitively, against
  names, summaries, categories and groups. An exact name selects only that name for its term.
- One to three matches return every complete contract and calling signature.
- Broader matches return up to 20 names and summaries. Search one or two exact
  names to get their contracts. Search never changes the native tool list.
- Pass a calling expression to ToolCall. Availability and permissions are
  checked again at execution; search does not grant authority.

### Examples of good usage

<example>
ToolSearch(query="Imenu XrefReferences")
Retrieve both contracts, then call the needed specialist through ToolCall.
</example>

### Examples of bad usage

<example>
ToolSearch(query="elisp")
Stop after receiving a broad catalog and guess a listed tool's arguments.
Instead, search the selected exact name to retrieve its complete contract.
</example>
