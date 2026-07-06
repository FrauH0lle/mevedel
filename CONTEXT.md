# Context

This glossary captures the domain language for mevedel. Keep it focused on user-facing concepts and durable workflow terms, not implementation details.

## Product domain

- **mevedel** — An Emacs Lisp package for visual AI-assisted programming workflows inside Emacs.
- **workspace** — The project context mevedel operates in, including repository roots, workspace configuration, persistent memory, and session state.
- **agent resource** — A portable user- or project-authored asset intended to be shared with agent tools, such as a skill, plugin, or durable memory.
- **mevedel state** — Runtime data owned by mevedel, such as session persistence, tool artifacts, input history, and generated metadata.
- **session** — A chat/workflow instance attached to a workspace. Sessions hold transcript state, permissions, reminders, tasks, background agents, and persistence metadata.
- **session cockpit** — The central user-facing control surface for one session, presenting live session state and routing common workflow, configuration, and inspection actions.
- **tabulated cockpit surface** — A session-owned cockpit buffer for inspecting and acting on selectable resources in a table.
- **worktree cockpit surface** — The session cockpit surface for Git worktree status and actions, split between a transient status menu and an optional tabulated worktree list.
- **worktree session** — A session whose working directory is a Git linked worktree while still belonging to the same mevedel workspace.
- **instruction** — A user-authored overlay in source buffers that gives mevedel context or work to perform.
- **reference** — An instruction that contributes context without asking the agent to act.
- **directive** — An instruction that asks the agent to discuss, implement, revise, tutor, or process work.
- **view buffer** — The compact, user-facing mevedel buffer with status, interaction, and input zones.
- **status strip** — The mevedel-owned clickable header-line chrome in a view buffer that routes status items to session cockpit surfaces.
- **data buffer** — The authoritative gptel/org transcript buffer backing a view buffer.
- **tool** — A model-callable operation routed through mevedel's validation, permission, execution, rendering, and persistence pipeline.
- **permission rule** — A rule deciding whether a tool call is allowed, denied, or requires user approval.
- **skill** — A reusable prompt package discovered from configured skill directories and invoked by `$skill` syntax or model-side `Skill` tool.
- **skill roster** — The model-facing list of active skill names and descriptions, including only enough metadata for the model to decide whether to invoke a skill.
- **skill lookup** — A model-facing search over enabled model-invocable skills, including dormant path-scoped skills that are not present in the active skill roster.
- **plugin** — A reusable extension bundle discovered from a plugin manifest. A plugin may contribute skills, hooks, and other implemented extension components.
- **plugin activation** — A workspace-scoped decision that makes a plugin's implemented components active for sessions in that workspace until disabled.
- **pending plugin hook consent** — A plugin activation state where plugin skills may remain active, but executable plugin hooks are withheld until the user reviews and approves the changed hook surface.
- **plugin cockpit** — A session cockpit surface for inspecting installed plugins and managing one selected plugin at a time.
- **agent** — A specialized sub-agent such as explorer, coordinator, verifier, or reviewer.
- **task** — A tracked work item in the session task list, optionally with dependency links.
- **persistent memory** — Durable user- or workspace-local memory included in future sessions.
- **ADR** — An architecture decision record under `docs/adr/` for durable decisions that should constrain future design work.

## Consumer rules

- Prefer these terms in issue titles, plans, tests, and architecture discussions.
- If a term is missing or ambiguous, ask or use `$grill-with-docs` to resolve it before adding competing vocabulary.
- Keep implementation module maps and detailed architecture in `AGENTS.md` and `docs/`; keep this file as a glossary.
