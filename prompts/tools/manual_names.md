List Info manuals available to this Emacs.

### When to use `manual_names`

- Discover the installed manual name needed for a documentation question.

### When NOT to use `manual_names`

- Checking package runtime state or searching the web.

### How to use `manual_names`

- Takes no arguments and returns a JSON array of manual names. Availability depends
  on local Info configuration, including for remote workspaces.
- Large output is persisted with a bounded preview and a retrieval address.
