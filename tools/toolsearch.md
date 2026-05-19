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

1. If you know the exact tool name or capability you need, call
   ToolSearch with load=true.
2. Use load=false only when you are exploring what tools exist.
3. After ToolSearch reports tools loaded, call the newly available tool
   on the next model turn.

Do not call a deferred tool directly before loading it with ToolSearch.
That can fail as an unknown tool call.

### Examples

<example>
ToolSearch(query="function_source", load=true)
-> Loads function_source. On the next model turn, call function_source
   with its normal arguments.
</example>

<example>
ToolSearch(query="edit", load=false)
-> Shows matching editing tools without loading them.

ToolSearch(query="Edit", load=true)
-> Loads Edit. On the next model turn, call Edit with its normal
   arguments.
</example>
