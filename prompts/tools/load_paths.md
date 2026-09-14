List the running Emacs process's library search directories.

### When to use `load_paths`

- Investigate library lookup order or possible package shadowing.

### When NOT to use `load_paths`

- Listing files in a project or proving which library file was actually loaded.

### How to use `load_paths`

- Takes no arguments and returns newline-separated load-path entries. These are
  Emacs search paths, including client-local paths in remote workspaces.
- Large output is persisted with a bounded preview and a retrieval address.
