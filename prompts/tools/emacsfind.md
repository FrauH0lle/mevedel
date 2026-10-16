Find function, interactive command, variable, or library names in the running
Emacs that match an Orderless pattern.

### When to use `EmacsFind`

- Discover the exact name of an Emacs Lisp function, command, variable, or
  library before reading it with EmacsDescribe.

### When NOT to use `EmacsFind`

- Searching project source text: use Grep.
- Checking whether a known library is loaded: use EmacsDescribe with
  `aspect="location"`.

### How to use `EmacsFind`

- `kind` is `function`, `command`, `variable`, or `library`. `pattern` uses
  Orderless matching; space-separated components match in any order.
- Function, command, and variable results cover symbols this Emacs has loaded.
  Library results cover library files in local `load-path` directories, loaded
  or not. Results are newline-separated names.
- Large output is persisted with a bounded preview and a retrieval address.

### Examples of good usage

<example>
EmacsFind(kind="command", pattern="gptel send")
</example>

### Examples of bad usage

<example>
EmacsFind(kind="function", pattern="a")
<reasoning>
A one-letter pattern matches thousands of names. Use distinctive components.
</reasoning>
</example>
