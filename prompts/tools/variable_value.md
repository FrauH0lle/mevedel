Read a variable's global default value in the running Emacs.

### When to use `variable_value`

- Inspect a specific non-sensitive value needed to explain runtime behavior.

### When NOT to use `variable_value`

- Inspecting credentials/authentication data or a buffer-local value.

### How to use `variable_value`

- Pass an exact variable name. Each call requests permission because values can
  contain private state. Returns the global default value, not a particular buffer's
  binding; unknown or unbound variables return nil or an error.
- Large output is persisted with a bounded preview and a retrieval address.

### Examples of good usage

<example>
variable_value(variable="fill-column")
</example>

### Examples of bad usage

<example>
variable_value(variable="auth-token")
<reasoning>
Do not retrieve authentication data. Diagnose the relevant non-sensitive configuration instead.
</reasoning>
</example>
