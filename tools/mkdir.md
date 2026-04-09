Create a new directory at the given path.

### When to use `MkDir`

- Creating new directories for organizing files
- Setting up directory structure for a project
- Preparing directories before writing files

### How to use `MkDir`

- Provide the full path of the directory to create
- Creates parent directories automatically if they don't exist (like mkdir -p)
- Safe to call multiple times -- existing directories are a no-op

### Examples of good usage

<example>
- Create a tests directory:
MkDir(path="tests")
</example>

<example>
- Create nested directory structure:
MkDir(path="src/components/forms")
</example>

### Examples of bad usage

<example>
- Using for file creation:
MkDir(path="src/app.js")
<reasoning>
Use Write tool to create files, not MkDir.
</reasoning>
</example>
