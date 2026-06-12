Navigate and explore a file's structure by listing all its functions, classes,
and variables with their locations.

### When to use Imenu

- Getting a structural overview of a single file's organization
- Listing all functions, classes, methods in a file
- Understanding file structure before making changes
- Quickly finding what symbols are defined in a file

### When NOT to use Imenu

- Searching across multiple files -> use Grep or delegate
- Finding where a symbol is used (references) -> use XrefReferences
- Reading actual code implementation -> use Read
- The file is very large and you only need specific content -> use Read with
  line ranges

### How to use Imenu

- Provide the file path to analyze
- Returns a hierarchical list of symbols (functions, classes, methods, etc.)
- Language-aware (uses major mode's imenu support)
- Useful as a first step before diving into specific functions
- Shows structure without full file content (more efficient than reading
  entire file)

### Examples of good usage

<example>
- Get overview of authentication module structure
Imenu(file_path="src/auth.js")
</example>

### Examples of bad usage

<example>
Imenu(file_path="**/*.py")
<reasoning>
Cannot analyze multiple files.
Use Glob to find files, then Imenu on individual files.
</reasoning>
</example>

<example>
Imenu(file_path="README.md")
<reasoning>
Looking for content in documentation.
Use Read to actually see the content of documentation files.
</reasoning>
</example>
