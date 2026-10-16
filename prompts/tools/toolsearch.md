Retrieve full specialist tool contracts for ToolCall, or find a tool by capability.

### When to use `ToolSearch`

- A listed tool's argument meaning or usage rules matter beyond its listed shape.
- A call failed and the error does not say how to fix it.
- The work needs a capability the current tool listing omits.

### When NOT to use `ToolSearch`

- The tool listing already gives the call shape you need; call it through ToolCall.

### How to use `ToolSearch`

- `query` matches any whitespace-separated term, case-insensitively, against
  names, summaries, categories and groups. An exact name selects only that name for its term.
- One to three matches return every complete contract and calling signature.
- Broader matches return up to 20 call shapes with summaries. Search one or two
  exact names to get their contracts. Search never changes the native tool list.
- Pass a calling expression to ToolCall. Availability and permissions are
  checked again at execution; search does not grant authority.

### Examples of good usage

<example>
ToolSearch(query="XrefReferences")
Retrieve the contract when the listed shape leaves an argument's meaning unclear.
</example>

### Examples of bad usage

<example>
ToolSearch(query="Imenu")
The tool listing already shows `(Imenu :file_path string)`; call it through
ToolCall instead of searching first.
</example>
