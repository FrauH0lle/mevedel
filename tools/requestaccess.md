Request access to a directory outside the current allowed project roots.

You must explain why you need access to this directory.

### When to use `RequestAccess`

- Need to access files outside the current workspace
- Working with configuration files in user's home directory
- Accessing shared libraries or dependencies
- Reading files from system directories

### When NOT to use `RequestAccess`

- Files are already within the workspace
- You haven't tried accessing the file yet (try first, then request
  if denied)

### How to use `RequestAccess`

- Provide the directory path you need to access
- Provide a clear reason explaining why access is needed
- User will approve or deny the request
- After approval, you can use Read, Write, Edit tools on files in
  that directory

### Examples of good usage

<example>
RequestAccess(directory="~/.config", reason="Need to read user's git configuration to understand repository settings")
</example>

### Examples of bad usage

<example>
RequestAccess(directory=".", reason="Need to read files")
<reasoning>
Workspace is already accessible, no need to request.
</reasoning>
</example>
