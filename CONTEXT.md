# Context

This glossary captures the domain language for mevedel. Keep it focused on user-facing concepts and durable workflow terms, not implementation details.

## Product domain

- **mevedel** — An Emacs Lisp package for visual AI-assisted programming workflows inside Emacs.
- **workspace** — The project context mevedel operates in, including repository roots, workspace configuration, persistent memory, and session state.
- **session** — A chat/workflow instance attached to a workspace. Sessions hold transcript state, permissions, reminders, tasks, background agents, and persistence metadata.
- **worktree session** — A session whose working directory is a Git linked worktree while still belonging to the same mevedel workspace.
- **instruction** — A user-authored overlay in source buffers that gives mevedel context or work to perform.
- **reference** — An instruction that contributes context without asking the agent to act.
- **directive** — An instruction that asks the agent to discuss, implement, revise, tutor, or process work.
- **view buffer** — The compact, user-facing mevedel buffer with status, interaction, and input zones.
- **data buffer** — The authoritative gptel/org transcript buffer backing a view buffer.
- **tool** — A model-callable operation routed through mevedel's validation, permission, execution, rendering, and persistence pipeline.
- **permission rule** — A rule deciding whether a tool call is allowed, denied, or requires user approval.
- **skill** — A reusable prompt package discovered from configured skill directories and invoked by slash command or model-side `Skill` tool.
- **agent** — A specialized sub-agent such as explorer, coordinator, verifier, or reviewer.
- **task** — A tracked work item in the session task list, optionally with dependency links.
- **persistent memory** — Durable workspace-local memory stored under `.mevedel/memory/` and included in future sessions.
- **ADR** — An architecture decision record under `docs/adr/` for durable decisions that should constrain future design work.

## Consumer rules

- Prefer these terms in issue titles, plans, tests, and architecture discussions.
- If a term is missing or ambiguous, ask or use `/grill-with-docs` to resolve it before adding competing vocabulary.
- Keep implementation module maps and detailed architecture in `AGENTS.md` and `docs/`; keep this file as a glossary.
