Read source located for a function or macro known to Emacs.

### When to use `function_source`

- Understand an implementation when a docstring is insufficient.

### When NOT to use `function_source`

- Searching arbitrary project files or assuming source text proves the currently
  executing definition.

### How to use `function_source`

- Pass an exact function or macro name. Returns the source form located by Emacs;
  unavailable definitions/source may return nil or an error. Source files can differ
  from an already loaded or advised definition.
- Large output is persisted with a bounded preview and a retrieval address.

### Examples of good usage

<example>
function_source(function="find-file-noselect")
</example>

### Examples of bad usage

<example>
function_source(function="find-file*")
<reasoning>
This expects one exact symbol, not a wildcard. Discover the intended function name first.
</reasoning>
</example>
