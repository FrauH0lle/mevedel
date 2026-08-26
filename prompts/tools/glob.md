Fast file pattern matching tool that works with any codebase size.

Resource addresses
- `path` accepts a raw filesystem path or canonical `local://`,
  `artifact://`, `skill://NAME@SOURCE-KEY[/RELATIVE-PATH]`, or
  `memory://root`/`memory://ROOT-KEY/RELATIVE-PATH` address, or packaged
  documentation at `mevedel://`/`mevedel://RELATIVE-PATH`. These are the only
  resource families supported by Glob. Examples include `local://notes`,
  `artifact://tool-result.txt`, `memory://root`, and `mevedel://`.
- Bare `local://` and `artifact://` list current entries;
  `memory://root` searches the configured memory roots, and `mevedel://`
  searches packaged Markdown documentation. Skill searches accept exact
  `skill://NAME@SOURCE-KEY` locators or the readable origin aliases
  `skill://local-mevedel/SKILL`, `skill://local-agents/SKILL`,
  `skill://global-mevedel/SKILL`, `skill://global-agents/SKILL`,
  `skill://bundled/SKILL`, `skill://managed/SKILL`, and
  `skill://plugin/PLUGIN/SKILL`, each with optional descendants. Aliases
  resolve to exact full-hash locators while output preserves the authored
  address. Agent, history, MCP, and other unsupported scheme/operation pairs
  are rejected explicitly.
- Use canonical `scheme://` text. Resource addresses name a tool target only:
  they do not attach content, invoke skills, or delegate agents. `@file` and
  `@mcp` attach content, `$skill` invokes instructions, and `@agent` delegates
  work. Do not use Markdown links or web URLs as filesystem paths.

- Supports glob patterns like "**/*.ts" or "src/**/*.py"
- Results are capped at 100 entries by default; narrow with `path` / a more
  specific pattern if results are truncated.
- Searches from the session working directory by default. Relative `path`
  values are resolved from the session working directory.
- Includes hidden and ignored files, except version-control metadata
  directories.
- Result ordering is unspecified.
- Stops after `mevedel-tool-fs-search-timeout` seconds (20 by default) and
  labels any captured output as partial; narrow the path or pattern after a
  timeout.
- Use this tool when you need to find files by name patterns
- When you are doing an open-ended search that may require multiple rounds of
  globbing and grepping, delegate to a specialist agent

### When to use `Glob`

- Searching for files by name patterns or extensions
- You know the file pattern but not exact location
- Finding all files of a certain type
- Exploring project or directory structure

### When NOT to use `Glob`

- Searching file contents -> use Grep
- You know the exact file path -> use Read
- Doing open-ended multi-round searches -> delegate

### How to use `Glob`

- Supports standard glob patterns: `**/*.ts`, `*.{js,jsx}`, `src/**/*.py`.
- Directory-qualified patterns narrow the search below `path`; absolute
  patterns and parent traversal are rejected.
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
