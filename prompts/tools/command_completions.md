Discover interactive command names known to the running Emacs.

### When to use `command_completions`

- Find interactive commands matching a naming fragment.

### When NOT to use `command_completions`

- Searching source files or treating a completion list as documentation or runtime
  values.

### How to use `command_completions`

- Matching uses the installed Orderless completion behavior over Emacs symbols, not
  a project-wide source index. Returns newline-separated names. Missing completion
  support is an error; results depend on what this Emacs has loaded.
- Large output is persisted with a bounded preview and a retrieval address.

### Examples of good usage

<example>
command_completions(command_prefix="org-")
</example>

### Examples of bad usage

<example>
command_completions(command_prefix="**/*.el")
<reasoning>
This matches symbol names, not file paths. Use a naming fragment relevant to the desired symbols.
</reasoning>
</example>
