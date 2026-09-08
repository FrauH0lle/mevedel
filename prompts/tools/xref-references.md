Find references to an identifier using the context file's xref support.

### When to use `XrefReferences`

- Investigate symbol usage or the likely impact of a change.

### When NOT to use `XrefReferences`

- Searching arbitrary text or filename patterns.
- Treating an empty result as proof that code is unused; backend coverage varies.

### How to use `XrefReferences`

- Pass the exact identifier and an existing source file for language/project
  context. Results are `file:line: summary` locations, or a diagnostic.
- The backend determines scope and precision. Emacs Lisp uses symbol-boundary
  text matches in project files, which can include comments or strings;
  results are not a guaranteed semantic call graph.
- Remote support currently covers that Emacs Lisp project-file search only.
  Other remote backends, missing indexes, and failed searches report limitations.
- Large output is persisted with a bounded preview and retrieval address.

### Examples of good usage

<example>
XrefReferences(identifier="authenticateUser", file_path="src/auth.ts")
-> Inspect the returned locations and the backend's coverage when assessing impact.
</example>

### Examples of bad usage

<example>
XrefReferences(identifier="auth.*", file_path="src/auth.ts")
<reasoning>
This expects an identifier, not a text-search regular expression. Use the
actual symbol name for a reference lookup.
</reasoning>
</example>
