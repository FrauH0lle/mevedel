# @-mention system

Supports `@ref`, `@file`, `@agent`, `@mcp`.

Expansion runs as a gptel prompt transform (priority -90) via
`mevedel--transform-expand-mentions`, dispatching through
`mevedel-mention-handlers`. Each mention becomes a compact
`[kind:KEY -- STATUS]` placeholder with full content injected as a
`<system-reminder>` block above the user prompt.

## Mention kinds

- **@ref:N** / **@ref:{tag query}** — refs by ID or tag
- **@file:path** — hierarchical file completion; optional `#L<start>[-<end>]`
  pins a line range (not recorded in touched-files, since LLM may still
  need other parts). Directories return a gitignore-filtered recursive
  listing (`rg --files --hidden --follow --sort path`) capped at
  `mevedel-file-mention-directory-max-entries` (default 1000). Contents
  read via `mevedel-tool-fs--slurp-file-contents` (512 KB cap, line
  numbers). Runs `mevedel-check-permission "Read"` first — any non-allow
  yields "permission denied". Directories, unreadable files, and binary
  extensions rejected.
- **@agent:name** — asks main agent to delegate via
  `Agent(subagent_type="NAME", ...)` (looked up in `mevedel-agent--registry`)
- **@mcp:server:uri** — attaches an MCP resource via mcp.el
  (`mcp-hub-get-servers`, `mcp-server-connections`, `mcp-read-resource`).
  URI capture is greedy past internal colons so `file:///...` works.
  mcp.el is optional (declared via `declare-function`).

Every rejection branch emits a follow-up `<system-reminder>` telling the
LLM the bracketed placeholder is a system annotation, not user text.

## Dedup

- Per-session: `mevedel-session-mentions-shown` keyed on `(KIND . KEY)`
  stores `(turn . content-hash)`; unchanged hashes skip re-injection
- Read dedup: `@file` records reads on `mevedel-session-touched-files`
  so later Read calls short-circuit

## Completion

`mevedel-ref-capf`, `mevedel-file-capf`, `mevedel-agent-capf`,
`mevedel-mcp-capf` (two-stage: server names at `@mcp:`, resource URIs at
`@mcp:server:`). Font-lock uses `success`/`shadow`/`link` box faces.
Registered in `mevedel-install`/`-uninstall`.
