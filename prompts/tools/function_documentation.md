Read a function or macro docstring from the running Emacs.

### When to use `function_documentation`

- Understand documented arguments, return values, or behavior.

### When NOT to use `function_documentation`

- Inspecting implementation code or proving undocumented behavior.

### How to use `function_documentation`

- Pass the exact function or macro name. Returns its docstring, or nil/an error when
  the symbol has no available documentation.
- Large output is persisted with a bounded preview and a retrieval address.

### Examples of good usage

<example>
function_documentation(function="mapcar")
</example>

### Examples of bad usage

<example>
function_documentation(function="(mapcar fn values)")
<reasoning>
Pass the symbol name "mapcar", not a call expression.
</reasoning>
</example>
