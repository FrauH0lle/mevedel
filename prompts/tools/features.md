Check a feature in the running Emacs.

### When to use `features`

- Check a named feature when the task depends on its availability.

### When NOT to use `features`

- Listing all installed packages or treating a non-nil result as proof that every
  API is usable.

### How to use `features`

- A loaded feature returns its name. For a name not yet interned, lookup may return
  a library path. Nil or a lookup error does not exhaustively establish package
  absence; a path alone does not establish that the feature is loaded.
- Large output is persisted with a bounded preview and a retrieval address.

### Examples of good usage

<example>
features(feature="org")
</example>

### Examples of bad usage

<example>
features(feature="*")
<reasoning>
This checks one feature name, not a package-listing pattern.
</reasoning>
</example>
