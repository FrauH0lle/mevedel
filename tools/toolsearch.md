Search for and load additional tools that are not currently available.

Some tools are deferred (not loaded by default) to save context. Use
ToolSearch to discover and activate them when needed.

### When to use `ToolSearch`

- When you need a capability that isn't available in your current tool
  set
- When a task requires tools beyond basic reading and searching
- At the start of complex tasks to check what specialized tools exist

### How to use `ToolSearch`

1. Call with a query to search by name or capability
2. Review the results to see what's available
3. Call again with load=true to activate matching tools

### Examples

<example>
ToolSearch(query="xref", load=true)
-> Loads XrefReferences and XrefDefinitions tools
</example>

<example>
ToolSearch(query="edit")
-> Shows available editing tools without loading them
</example>
