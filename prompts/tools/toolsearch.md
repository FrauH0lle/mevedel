Retrieve full specialist tool contracts for ToolCall, or find a tool by capability.

Use when argument semantics exceed the listing, an error lacks recovery guidance,
or a needed capability is omitted. Otherwise call the listed shape through ToolCall.

- `query` matches any whitespace-separated term, case-insensitively, against
  names, summaries, categories and groups. An exact name selects only that name for its term.
- One to three matches return every complete contract and calling signature.
- Broader matches return up to 20 call shapes with summaries. Search one or two
  exact names to get their contracts. Search never changes the native tool list.
- Pass a calling expression to ToolCall. Availability and permissions are
  checked again at execution; search does not grant authority.
