Find files by name patterns, including hidden and ignored files except
version-control metadata.

### When to use `Glob`

- Locate files by extension or name, or inspect directory structure.
- Discover packaged manuals under `mevedel://`.

### When NOT to use `Glob`

- Searching file contents: use Grep.
- Reading a known file: use Read.

### How to use `Glob`

- Patterns such as `**/*.ts`, `*.{js,jsx}`, and `src/**/*.py` are relative to
  `path`, which defaults to the session working directory. Absolute patterns
  and parent traversal are rejected; set `path` to the intended search root.
- Supported resources: `work://`, `artifact://`, `skill://`, `memory://journal/`,
  `memory://root` or a memory descendant, `history://saved` or its descendants,
  and `mevedel://` or its descendants.
  Live history, agent, and MCP addresses do not support Glob.
- Results are newline-separated paths or resource addresses, with unspecified
  ordering and a default cap of 100 entries. Timeouts (20 seconds by default)
  and output limits label partial results; narrow the path or pattern to continue.
- For resource aliases and detailed search limits, read
  `mevedel://tools/files.md`. If unavailable, stay within the known contract.

### Examples of good usage

<example>
- Find JavaScript test files:
Glob(pattern="**/*.test.js", path="src")
The result lists matching paths beneath src.
</example>

### Examples of bad usage

<example>
Glob(pattern="password")
<reasoning>
This finds files named password. To find that text inside files, use Grep(pattern="password").
</reasoning>
</example>
