# Architecture

## Key data structures

Defined in `mevedel-structs.el` / `mevedel-tool-registry.el`:

- **`mevedel-workspace`**: type, id, root, name,
  additional-roots, file-cache, hints. `.mevedel/` is derived by
  `mevedel-workspace-state-dir`, not stored as a slot.
- **`mevedel-session`**: per-chat state: workspace, working
  directory, tasks, touched-files, permission rules/mode, reminders,
  deferred tool state, mailbox messages, background agents, mention
  dedup, skills, session persistence metadata, agent transcript index,
  invoked skills, permission queue, plan queue.
- **`mevedel-request`**: per-turn state: session, file-snapshots,
  directive UUID, pending plan, cancellers, skill-scoped permission
  rules, model override, effort override.
- **`mevedel-tool`**: name, handler, description, summary, prompt,
  args, category, read-only/destructive/async flags, sync/async
  permission hooks, specifier extractors (`get-path`, `get-pattern`,
  `get-domain`, `get-name`), groups, max-result-size, renderer.
- `mevedel--instructions`: buffer -> overlay alist
- `mevedel--id-counter` / `mevedel--id-usage-map`: instruction IDs
- Instruction types: **References** (context) and **Directives** (prompts)

## Workspace context chain

```
Data buffer (authoritative gptel/org buffer; holds mevedel--workspace,
mevedel--session, and the model-visible transcript)
  |
View buffer (mevedel-view-mode; holds mevedel--data-buffer and the
editable input region)
  |
Derived buffers / previews / transcript inspection views point back to
their data or parent view buffers as needed
```

Tools execute in the data-buffer context with `default-directory` set to
the session working directory. File modifications are tracked per request
via `mevedel-request-file-snapshots`, while cross-turn file metadata
lives on the workspace file cache and session touched-files map.

## gptel integration

Direct via `gptel-request` and `gptel-fsm`. Tools registered in
`gptel--known-tools`. Four presets: `mevedel-discuss` → `implement` →
`revise`; `tutor` inherits from `discuss`. System prompt assembled
dynamically from Markdown-backed parts. Static content is emitted first
for provider prefix-cache reuse: base prompt, workspace config
(AGENTS.md/CLAUDE.md), persistent memory, then environment.

`mevedel-system-build-prompt` checks each directory from workspace root
to the session working directory, preferring `AGENTS.md` over
`CLAUDE.md` in the same directory. Matching files are included from
broadest to closest scope as `## Workspace Configuration` so deeper
instructions override earlier ones.

## Persistent memory

`.mevedel/memory/MEMORY.md` under workspace root is the always-loaded
memory index; first 200 lines are included in every system prompt via
`mevedel-system--memory-prompt`, with a last-updated age annotation.
Durable memory bodies live in linked topic files under
`.mevedel/memory/`, using `user`, `feedback`, `project`, or
`reference` frontmatter. `MEMORY.md` should contain one-line links
only. LLM-writable.

## Chat buffer formatting

The data buffer is normally org-mode so gptel can persist
`GPTEL_BOUNDS` and related state. Tool results containing
`:PROPERTIES:` are escaped with `,` in the data buffer to prevent
nested-drawer confusion; the rendered view strips those storage
artifacts where appropriate.
