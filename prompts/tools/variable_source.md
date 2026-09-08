Read the source declaration located for an Emacs Lisp variable.

### When to use `variable_source`

- Inspect a variable's definition, documented default, or customization declaration.

### When NOT to use `variable_source`

- Reading the variable's current value or a buffer-local binding.

### How to use `variable_source`

- Pass an exact variable name. Returns its source declaration when Emacs can locate
  it; missing symbols/source may return nil or an error. A declared default need not
  equal the current value.
- C-defined variables require the matching Emacs C sources to be locatable.
- Large output is persisted with a bounded preview and a retrieval address.

### Examples of good usage

<example>
variable_source(variable="backup-directory-alist")
</example>

### Examples of bad usage

<example>
variable_source(variable="backup-directory-alist") followed by reporting its declared default as the current value
<reasoning>
Source explains the declaration; it does not inspect live bindings.
</reasoning>
</example>
