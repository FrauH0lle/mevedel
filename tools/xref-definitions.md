Search for functions, variables, or classes by name pattern across your project.
Helps you discover code elements when you know part of the name.

### When to use XrefDefinitions

- Discovering functions or variables with names matching a pattern
- Finding related symbols when you know part of the name
- Exploring API surface area by naming convention
- Locating symbol definitions by partial name

### When NOT to use XrefDefinitions

- Searching for specific text in files -> use Grep
- Finding exact symbol references/usage -> use XrefReferences
- Searching across many files without symbol focus -> delegate
- Pattern is too vague and will return many results -> delegate

### How to use XrefDefinitions

- Provide a pattern (substring or regex) to match symbol names
- Works with indexed symbols (LSP, TAGS, elisp definitions)
- Returns symbol definitions (not all references)
- Useful for discovering what is available in a codebase

### Examples of good usage

<example>
- Find all authentication-related symbols
XrefDefinitions(pattern="auth", file_path=".")
</example>

<example>
- Find symbols with 'config' in name
XrefDefinitions(pattern="*config*", file_path=".")
</example>

### Examples of bad usage

<example>
XrefDefinitions(pattern="error_message", file_path=".")
<reasoning>
Looking for text occurrences.
Use Grep to search for text strings, not symbol definitions.
</reasoning>
</example>
