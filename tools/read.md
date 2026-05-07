Reads a file from the local filesystem. You can access any file directly by
using this tool. Assume this tool is able to read all files on the machine. If
the user provides a path to a file assume that path is valid. It is okay to read
a file that does not exist; an error will be returned.

Usage:
- The file_path parameter can be absolute or relative. Relative paths are
  resolved from the session working directory.
- By default, it reads up to 2000 lines starting from the beginning of the file
- When you already know which part of the file you need, only read that part.
  This can be important for larger files.
- Results are returned using cat -n format, with line numbers starting at 1
- This tool can only read files, not directories. To read a directory, use an ls
  command via the Bash tool.
- Any lines longer than 2000 characters will be truncated.
- Files over 512 KB in size can only be read by specifying offset and limit.

### When to use Read

- You need to examine file contents
- Before editing any file (required)
- You know the exact file path
- Understanding code structure and implementation

### When NOT to use Read

- Searching for files by name -> use Glob
- Searching file contents across multiple files -> use Grep

### How to use Read
- Default behavior reads from beginning to end
- For large files, use offset and limit parameters to read specific sections
- Recommended to read the whole file when possible
- Always read before editing - edit tools will error otherwise
- You can call multiple tools in a single response. It is always better to
  speculatively read multiple potentially useful files in parallel.

### Examples of good usage

<example>
- Reading a specific function:
Read(file_path="src/utils.ts", offset=45, limit=18)
</example>

<example>
- Examining configuration before changes:
Read(file_path="config/database.yml")
</example>

### Examples of bad usage

<example>
- Trying to find all files with 'test' in the name:
Read(file_path="*test*")
<reasoning>
Should use Glob(pattern="*test*") instead.
</reasoning>
</example>
