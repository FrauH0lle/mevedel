Read one function's, variable's, or library's documentation, source, location,
Info manual section, or global value from the running Emacs.

### When to use `EmacsDescribe`

- Understand an Emacs Lisp symbol or library that is loaded or on `load-path`,
  including one outside the workspace.
- Check a variable's actual global value when runtime behavior depends on it.
- Find where a definition lives, or whether a library is available and loaded.

### When NOT to use `EmacsDescribe`

- Searching project files: use Grep, Glob, or Read.
- Reading credentials or other private values.

### How to use `EmacsDescribe`

- `aspect` selects `documentation` (docstring), `source` (definition located by
  Emacs), `location` (the defining file; for a library, its path and whether its
  feature is loaded), `manual` (the Info section documenting the symbol), or
  `value` (the global default value).
- `kind` is inferred when omitted: a defined function, else a variable, else a
  library for `source` or `location`. Pass `variable` for a name that is both,
  and `library` for a library such as `calc` whose name is also a command.
- An unknown name returns a not-defined message; it never interns the symbol.
- Source text can differ from the loaded or advised definition. A docstring or
  source does not establish a current runtime value; read `value` for that.
- `value` asks for permission on every call because values can hold private
  state. It returns the global default, not a buffer-local binding.
- Library source must resolve inside a local `load-path` directory. Location
  searches only local `load-path` directories, and a miss does not prove the
  package is absent. C primitive and variable source needs
  `find-function-C-source-directory`; without it the call returns an error
  instead of prompting.
- Large output is persisted with a bounded preview and a retrieval address.

### Examples of good usage

<example>
EmacsDescribe(symbol="fill-column", aspect="value")
Establishes the global default; a buffer can still have its own binding.
</example>

<example>
EmacsDescribe(symbol="calc", aspect="location", kind="library")
Reports the library's path and whether it is loaded.
</example>

### Examples of bad usage

<example>
EmacsDescribe(symbol="my-project-helper", aspect="source")
<reasoning>
For code in the workspace, Grep and Read show the file itself, including
unloaded edits.
</reasoning>
</example>
