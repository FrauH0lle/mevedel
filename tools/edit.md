Performs exact string replacements in files.

Usage:
- You MUST use the `Read` tool at least once in the conversation before editing. This tool will error if you attempt an edit without reading the file.
- The file_path parameter can be absolute or relative. Relative paths are
  resolved from the session working directory.
- When editing text from Read tool output, ensure you preserve the exact indentation (tabs/spaces) as it appears AFTER the line number prefix. The line number prefix format is: line number + tab. Everything after that is the actual file content to match. Never include any part of the line number prefix in the old_string or new_string.
- ALWAYS prefer editing existing files in the codebase. NEVER write new files unless explicitly required.
- The edit will FAIL if `old_string` is not unique in the file. Either provide a larger string with more surrounding context to make it unique or use `replace_all` to change every instance of `old_string`.
- Use `replace_all` for replacing and renaming strings across the file. This parameter is useful if you want to rename a variable for instance.

### When to use `Edit`

- Modifying existing files with surgical precision
- Making targeted changes to code or configuration
- Replacing specific strings, functions, or sections
- Renaming variables or identifiers across a file (with `replace_all`)

### When NOT to use `Edit`

- Creating brand new files -> use `Write`
- You haven't read the file yet -> must read first

### How to use `Edit`

- MUST read the file first before editing
- Provide exact `old_string` to match (including proper indentation from file content)
- Provide `new_string` as replacement (must be different from `old_string`)
- The edit will FAIL if `old_string` is not unique (unless `replace_all` is true)
- Preserve exact indentation from the file content
- Always prefer editing existing files over creating new ones

### Examples of good usage

<example>
- Updating a function signature:
Edit(file_path="src/auth.ts", old_string="function validateUser(username: string, password: string)", new_string="function validateUser(username: string, password: string, timeoutMs?: number)")
</example>

<example>
- Renaming a variable across the file:
Edit(file_path="src/utils.ts", old_string="oldName", new_string="newName", replace_all=true)
</example>

### Examples of bad usage

<example>
- Trying to edit without reading the file first:
Edit(file_path="src/config.ts", old_string="old", new_string="new")
<reasoning>
Must read the file first to know the exact content to match.
</reasoning>
</example>
