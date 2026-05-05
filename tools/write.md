Writes a file to the local filesystem.

Usage:
- This tool will overwrite the existing file if there is one at the provided path.
- The file_path parameter can be absolute or relative. Relative paths are
  resolved from the project root.
- If this is an existing file, you MUST use the Read tool first to read the file's contents. This tool will fail if you did not read the file first.
- When editing text from Read tool output, ensure you preserve the exact indentation (tabs/spaces) as it appears AFTER the line number prefix. The line number prefix format is: line number + tab. Everything after that is the actual file content to match. Never include any part of the line number prefix in the content.
- ALWAYS prefer editing existing files in the codebase. NEVER write new files unless explicitly required.
- Prefer the Edit tool for modifying existing files -- it only sends the diff. Only use this tool to create new files or for complete rewrites.
- NEVER create documentation files (*.md) or README files unless explicitly requested by the User.

### When to use `Write`

- Creating new files that don't exist yet
- Completely replacing the contents of an existing file
- Generating new code or configuration files

### When NOT to use `Write`

- Modifying existing files -> use `Edit` instead (more precise and safer)
- The file already exists and you only need to change part of it -> use `Edit`
- You haven't read the file first (if it exists) -> read first, then decide

### How to use `Write`

- Will overwrite existing files completely -- use with caution
- Parent directories are created automatically if they don't exist
- Provide the complete file path and the full file content
- The content should be the entire file, not a partial update

### Examples of good usage

<example>
- Creating a new test file:
Write(file_path="tests/test-user-auth.el", content=";;; test-user-auth.el --- Tests\n\n(ert-deftest test-auth ()\n  (should (user-auth-valid-p \"user\" \"pass\")))\n")
</example>

<example>
- Generating a complete configuration file:
Write(file_path="config/database.yml", content="development:\n  adapter: postgresql\n  database: myapp_dev\n")
</example>

### Examples of bad usage

<example>
- Trying to modify just one function in an existing file:
Write(file_path="src/utils.el", content="(defun helper-func () ...)")
<reasoning>
Should use Edit instead to preserve other functions.
</reasoning>
</example>
