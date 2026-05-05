Fast file pattern matching tool that works with any codebase size.

- Supports glob patterns like "**/*.el" or "src/**/*.ts"
- Returns matching file paths sorted by modification time
- Searches from the project root by default. Relative `path` values are
  resolved from the project root.
- Use this tool when you need to find files by name patterns
- When you are doing an open-ended search that may require multiple rounds of
  globbing and grepping, delegate to a specialist agent

### When to use Glob

- Searching for files by name patterns or extensions
- You know the file pattern but not exact location
- Finding all files of a certain type
- Exploring project or directory structure

### When NOT to use Glob

- Searching file contents -> use Grep
- You know the exact file path -> use Read
- Doing open-ended multi-round searches -> delegate

### How to use Glob

- Supports standard glob patterns: `**/*.el`, `*.{el,txt}`, `lisp/**/*.el`.
- Returns files sorted by modification time (most recent first)
- You can call multiple tools in a single response. It is always better to
  speculatively perform multiple searches in parallel if they are potentially
  useful.

### Examples of good usage

<example>
- Find all test files
Glob(pattern="**/*.test.js")
</example>

<example>
- Find all config files
Glob(pattern="config/*.{yml,yaml,json}")
</example>

### Examples of bad usage

<example>
- Searching for content
Glob(pattern="password")
<reasoning>
Should use Grep to search file contents instead.
</reasoning>
</example>

<example>
Glob(pattern="/usr/local/bin/python")
<reasoning>
Should use Read if you want to read a specific known file.
</reasoning>
</example>
