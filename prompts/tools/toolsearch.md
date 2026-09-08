Find specialist tools and retrieve their contracts for ToolCall.

### When to use `ToolSearch`

- Find a capability beyond the native core tools, including configured MCP tools.
- Retrieve exact arguments or rediscover a contract after compaction.

### When NOT to use `ToolSearch`

- A native tool or a known current contract already covers the operation.

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
