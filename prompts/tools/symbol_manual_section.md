Read the Info section associated with an Emacs Lisp symbol.

### When to use `symbol_manual_section`

- Find reference documentation for a known function, macro, or variable.

### When NOT to use `symbol_manual_section`

- Searching arbitrary topics or assuming every package provides an Info symbol
  index.

### How to use `symbol_manual_section`

- Pass an exact symbol name. Returns the matching node contents, or nil/a lookup
  error when no section can be found.
- Large output is persisted with a bounded preview and a retrieval address.

### Examples of good usage

<example>
symbol_manual_section(symbol="mapcar")
</example>

### Examples of bad usage

<example>
symbol_manual_section(symbol="how to iterate lists")
<reasoning>
This resolves a symbol, not a natural-language topic. Use an exact documented name.
</reasoning>
</example>
