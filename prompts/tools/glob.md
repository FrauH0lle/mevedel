Find files by name patterns, including hidden and ignored files except
version-control metadata.

Use Grep for file contents and Read for a known file.

- Patterns such as `**/*.ts`, `*.{js,jsx}`, and `src/**/*.py` are relative to
  `path`, which defaults to the session working directory. Absolute patterns
  and parent traversal are rejected; set `path` to the intended search root.
- Supported resources: `work://`, `artifact://`, a selected `skill://` package, `memory://journal/`,
  `memory://root` or a memory descendant, `history://saved` or its descendants,
  and `mevedel://` or its descendants.
  Live history, agent, and MCP addresses do not support Glob.
  Bare `skill://` is a Read-only discovery root.
- Results are newline-separated paths or resource addresses, with unspecified
  ordering and a default cap of 100 entries. Timeouts (20 seconds by default)
  and output limits label partial results; narrow the path or pattern to continue.
- For resource aliases and detailed search limits, read
  `mevedel://tools/files.md`. If unavailable, stay within the known contract.

Glob(pattern="**/*.test.js", path="src") finds test files beneath src.
