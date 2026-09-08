Search file contents with ripgrep regular expressions.

### When to use `Grep`

- Locate text, investigate code, or check occurrences across files.
- Search a persisted oversized result or packaged documentation.

### When NOT to use `Grep`

- Finding filenames rather than contents: use Glob.
- Reading a known contiguous range: use Read.

### How to use `Grep`

- Searches from the session working directory unless `path` selects another
  file or directory. Supported resources: `local://`, `artifact://`, `skill://`,
  `memory://root` or a memory descendant, and `mevedel://` or its descendants.
  Agent, history, and MCP addresses do not support Grep.
- Patterns use ripgrep syntax. For literal parentheses or braces, character
  classes such as `[(]` and `[{]` avoid escaping ambiguity. `multiline=true`
  permits matches across lines; ordinary patterns match within one line.
- The default result is file paths. `output_mode="content"` returns matching
  lines and supports context; `"count"` returns per-file match counts.
  `head_limit` and `offset` bound the returned entries, not the search scope.
- Scope with `path`, `glob`, or `type`. Glob filters are relative to `path`;
  absolute filters and parent traversal are rejected. Ordinary traversal
  includes hidden files and respects ignores; an explicit path or positive
  glob can select ignored files. Version-control metadata is excluded.
- Ordering is unspecified. Timeouts (20 seconds by default) and output limits
  label partial results. Narrow the scope when results are partial; absence in
  a partial result is not proof of absence. Follow-up searches are valid.
- For resource aliases, search caps, and partial-output details, read
  `mevedel://tools/files.md`. If unavailable, stay within the known contract.

### Examples of good usage

<example>
- Locate an error with surrounding code:
Grep(pattern="authentication failed", path="src", output_mode="content", context=3)
The result contains matching lines and three lines of surrounding context.
</example>

### Examples of bad usage

<example>
Grep(pattern="*.test.js")
<reasoning>
This is a filename glob, not a content regex. Use Glob(pattern="**/*.test.js").
</reasoning>
</example>
