Discover bound variable names known to the running Emacs.

### When to use `variable_completions`

- Find bound variables matching a naming fragment.

### When NOT to use `variable_completions`

- Searching source files or treating a completion list as documentation or runtime
  values.

### How to use `variable_completions`

- Matching uses the installed Orderless completion behavior over Emacs symbols, not
  a project-wide source index. Returns newline-separated names. Missing completion
  support is an error; results depend on what this Emacs has loaded.
- Large output is persisted with a bounded preview and a retrieval address.

### Examples of good usage

<example>
variable_completions(variable_prefix="org agenda")
Find bound variables matching both naming fragments under the installed
Orderless configuration. Use an exact returned name for later introspection;
the result depends on symbols available in this Emacs.
</example>

### Examples of bad usage

<example>
variable_completions(variable_prefix="**/*.el")
<reasoning>
This matches symbol names, not file paths. Use a naming fragment relevant to the desired symbols.
</reasoning>
</example>
