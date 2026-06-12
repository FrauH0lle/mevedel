Search for and load deferred tools before using them.

Some tools are deferred (not loaded by default) to save context. Use
ToolSearch to discover and activate them when needed. A deferred tool
name from a reminder or search result is not callable until it has been
loaded.

### When to use `ToolSearch`

- When you need a capability that isn't available in your current tool
  set
- When a task requires tools beyond basic reading and searching
- At the start of complex tasks to check what specialized tools exist

### How to use `ToolSearch`

1. Search by exact tool name when a reminder or prior result names one:
   `XrefReferences`, `XrefDefinitions`, `Imenu`, `Treesitter`,
   `function_source`, etc.
2. Search by capability group when exploring a family of tools:
   `xref`, `imenu`, `treesitter`, `elisp`, `web`, etc.
3. If you know you need the matching tool, call ToolSearch with
   load=true. Use load=false only when you are exploring what tools
   exist.
4. After ToolSearch reports tools loaded, those tools are available now;
   call the newly available tool in your next tool call.

Do not call a deferred tool directly before loading it with ToolSearch.
That can fail as an unknown tool call.

### Examples

<example>
ToolSearch(query="function_source", load=true)
-> Loads function_source. The tool is available now; call
   function_source with its normal arguments in your next tool call.
</example>

<example>
ToolSearch(query="edit", load=false)
-> Shows matching editing tools without loading them.

ToolSearch(query="Edit", load=true)
-> Loads Edit. The tool is available now; call Edit with its normal
   arguments in your next tool call.
</example>
