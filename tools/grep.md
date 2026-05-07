A powerful search tool built on ripgrep.

  Usage:
  - Supports full regex syntax (e.g., "log.*Error", "defun\s+\w+")
  - Filter files with glob parameter (e.g., "*.ts", "**/*.tsx") or type
    parameter (e.g., "py", "rust")
  - Output modes: "content" shows matching lines, "files_with_matches" shows
    only file paths (default), "count" shows match counts
  - Pattern syntax: Uses ripgrep (not grep) -- literal braces need escaping (use
    `interface\{\}` to find `interface{}` in Go code)
  - Multiline matching: By default patterns match within single lines only. For
    cross-line patterns like `struct \{[\s\S]*?field`, use `multiline: true`
  - Searches from the session working directory by default. Relative `path`
    values are resolved from the session working directory.

### When to use Grep

- Finding a specific string or pattern in the codebase
- You know what you are looking for and where it likely is
- Verifying presence or absence of specific text
- Quick, focused searches with expected results under 20 matches

### When NOT to use Grep

- Building code understanding or exploring unfamiliar code -> delegate
- Expected to get many results (20+ matches) -> delegate
- Will need follow-up searches based on results -> delegate
- Searching for files by name -> use Glob
- Reading known file contents -> use Read

### How to use Grep

- Supports full regex syntax
- Can specify glob pattern to narrow scope
- Use context, -A, -B for context lines around matches
- Use output_mode to control result format
- You can call multiple tools in a single response. It is always better to
  speculatively perform multiple focused grep searches in parallel.
- If you find yourself doing a second grep based on first results, you should
  have delegated

### Examples of good usage

<example>
- Find all TODO comments in Python files
Grep(pattern="TODO|FIXME", path=".", glob="**/*.py")
</example>

<example>
- Find function definition with context
Grep(pattern="def authenticate", path=".", output_mode="content", context=3)
</example>

<example>
- List files containing a symbol
Grep(pattern="authenticateUser", path=".", glob="**/*.ts")
</example>

### Examples of bad usage

<example>
Grep(pattern="import")
<reasoning>
Too generic, will return many results.
Should delegate to a specialist for broader exploration.
</reasoning>
</example>

<example>
Grep(pattern="user", glob="**/*")
- Followed by more searches based on results
<reasoning>
Should delegate instead of doing multiple sequential searches.
</reasoning>
</example>
