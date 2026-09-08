List a file's symbol outline using its Emacs major mode's Imenu index.

### When to use `Imenu`

- Find named definitions or sections and their locations in one file.

### When NOT to use `Imenu`

- Searching multiple files or finding references to a symbol.
- Reading implementation text; an outline contains names and locations only.

### How to use `Imenu`

- Pass one existing file path, not a directory or glob. An existing visiting
  buffer is used, including its unsaved contents.
- Results are location lines with hierarchical symbol names. Coverage depends
  on the file's major mode and index; an empty index does not prove the file
  contains no definitions. Missing support or a failed lookup is reported.
- Large results are persisted with a bounded preview and a retrieval address.

### Examples of good usage

<example>
Imenu(file_path="src/auth.js")
-> Inspect the outline and locations provided by this file's major mode.
</example>

### Examples of bad usage

<example>
Imenu(file_path="**/*.py")
<reasoning>
The tool accepts one file. Discover the paths first, then inspect the relevant
file's outline.
</reasoning>
</example>
