Read documentation for an Emacs Lisp variable.

### When to use `variable_documentation`

- Understand a configuration option or documented variable meaning.

### When NOT to use `variable_documentation`

- Reading the variable's current value.

### How to use `variable_documentation`

- Pass an exact variable name. Returns available variable documentation, or nil/an
  error when no documentation is found. Documentation describes a setting; it does
  not establish the current binding.
- Large output is persisted with a bounded preview and a retrieval address.

### Examples of good usage

<example>
variable_documentation(variable="fill-column")
</example>

### Examples of bad usage

<example>
variable_documentation(variable="fill-column") followed by reporting a live value
<reasoning>
The result is documentation. It does not read the current value.
</reasoning>
</example>
