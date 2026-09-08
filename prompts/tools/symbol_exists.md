Check whether a symbol is interned in the running Emacs.

### When to use `symbol_exists`

- Confirm an exact symbol name is known to this Emacs process.

### When NOT to use `symbol_exists`

- Proving a symbol is a callable function, a bound variable, or an installed
  package.

### How to use `symbol_exists`

- Returns the interned symbol name or nil. Existence alone says nothing about its
  definition or value.

### Examples of good usage

<example>
symbol_exists(symbol="mapcar")
</example>

### Examples of bad usage

<example>
symbol_exists(symbol="map*")
<reasoning>
This is an exact name check, not wildcard completion. Use a known symbol name.
</reasoning>
</example>
