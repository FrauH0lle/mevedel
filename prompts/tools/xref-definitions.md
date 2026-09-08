Find symbol definitions by name pattern using the file's xref backend.

### When to use `XrefDefinitions`

- Discover definitions when you know a name or part of one.

### When NOT to use `XrefDefinitions`

- Searching source text or finding uses of a symbol.
- Remote workspaces: definition lookup is currently unsupported.

### How to use `XrefDefinitions`

- `file_path` must name an existing file that selects the language/project
  context. It is not a directory or a search-result filter.
- The pattern is passed to the backend's symbol search. Pattern syntax,
  indexing, and search scope depend on that backend; this is not a universal
  project-wide text search.
- Returns `file:line: summary` locations, or a no-results/missing-backend
  diagnostic. An unavailable index is not evidence that no definition exists.
  Large output is persisted with a bounded preview and retrieval address.

### Examples of good usage

<example>
XrefDefinitions(pattern="auth", file_path="src/app.ts")
-> Ask that file's backend for matching symbol definitions.
</example>

### Examples of bad usage

<example>
XrefDefinitions(pattern="config", file_path="src/")
<reasoning>
A directory cannot select the file's language backend. Use an existing source
file, for example file_path="src/settings.py".
</reasoning>
</example>
