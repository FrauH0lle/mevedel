Search file contents with ripgrep regular expressions.

Use Glob for filenames and Read for a known contiguous range.

- Searches from the session working directory unless `path` selects another
  file or directory. Supported resources: `work://`, `artifact://`, a selected `skill://` package,
  `memory://journal/`, `memory://root` or a memory descendant, `history://saved`
  or its descendants, `history://root` or a retained-agent history descendant,
  `shared://` text views, and `mevedel://` or its descendants.
  Bare `history://` and `skill://` are Read-only discovery roots; agent and MCP
  addresses do not support Grep.
- Patterns use ripgrep syntax. For literal parentheses or braces, character
  classes such as `[(]` and `[{]` avoid escaping ambiguity. `multiline=true`
  permits matches across lines; ordinary patterns match within one line.
- The default result is file paths. `output_mode="content"` returns matching
  lines and supports context; `"count"` returns per-file match counts.
  `head_limit` and `offset` bound the returned entries, not the search scope.
- Long content lines are previews. Read the returned source and line with
  `limit=1`, then follow Read's `char_offset` continuations to reach the tail.
- Scope with `path`, `glob`, or `type`. Glob filters are relative to `path`;
  absolute filters and parent traversal are rejected. Ordinary traversal
  includes hidden files and respects ignores; an explicit path or positive
  glob can select ignored files. Version-control metadata is excluded.
- Ordering is unspecified. Timeouts (20 seconds by default) and output limits
  label partial results. Narrow the scope when results are partial; absence in
  a partial result is not proof of absence. Follow-up searches are valid.
- For resource aliases, search caps, and partial-output details, read
  `mevedel://tools/files.md`. If unavailable, stay within the known contract.

Grep(pattern="authentication failed", path="src", output_mode="content", context=3)
returns matches with three lines of surrounding context.
