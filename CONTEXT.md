# Context

This glossary captures the domain language for mevedel. Keep it focused on user-facing concepts and durable workflow terms, not implementation details.

## Product domain

- **mevedel** — An Emacs Lisp package for visual AI-assisted programming workflows inside Emacs.
- **workspace** — The project context mevedel operates in, including repository roots, workspace configuration, persistent memory, and session state.
- **agent resource** — A portable user- or project-authored asset intended to be shared with agent tools, such as a skill, plugin, or durable memory.
- **mevedel state** — Runtime data owned by mevedel, such as session persistence, tool artifacts, input history, and generated metadata.
- **session** — A chat/workflow instance attached to a workspace. Sessions hold transcript state, permissions, reminders, tasks, background agents, and persistence metadata.
- **session setting** — Configuration owned by one session, initially derived from global defaults, preserved across resume, and copied when the session is forked without affecting other sessions.
- **session cockpit** — The central user-facing control surface for one session, presenting live session state and routing common workflow, configuration, and inspection actions.
- **tabulated cockpit surface** — A session-owned cockpit buffer for inspecting and acting on selectable resources in a table.
- **worktree cockpit surface** — The session cockpit surface for Git worktree status and actions, split between a transient status menu and an optional tabulated worktree list.
- **worktree session** — A session whose working directory is a Git linked worktree while still belonging to the same mevedel workspace.
- **instruction** — A user-authored overlay in source buffers that gives mevedel context or work to perform.
- **reference** — An instruction that contributes context without asking the agent to act.
- **reference mention binding** — The association between a direct `@ref:N` mention and the exact selected reference. It follows that reference's latest state, but never a later reference that reuses its displayed number.
- **reference query mention** — An `@ref:{query}` mention that selects all currently matching references when dispatched rather than preserving an earlier result set.
- **file mention binding** — The association between an `@file` mention and the absolute pathname selected when the mention is bound. It follows the latest contents at that pathname rather than a file snapshot or filesystem object.
- **MCP mention binding** — The association between an `@mcp` mention and one server-name/resource-URI pair. It reads the resource's latest contents and treats a replacement server with the same name as the same locator.
- **agent mention** — An `@agent:name` mention that selects the currently registered agent definition by its unique name when dispatched.
- **directive** — An instruction that asks the agent to discuss, implement, revise, tutor, or process work.
- **goal** — A durable user objective that may require multiple planning and execution cycles before it is complete or blocked.
- **goal cycle** — One planning, approval, implementation, and review iteration toward a goal.
- **plan** — The proposed implementation approach for one goal cycle, subject to user approval.
- **goal phase** — The current stage of active work within a goal: planning, awaiting approval, implementing, or reviewing.
- **goal status** — The lifecycle state controlling whether a goal may continue: active, paused, blocked, or complete. Status is independent of the goal's current phase.
- **goal token budget** — An optional limit on model tokens consumed across a goal's workloads. Exhausting it pauses the goal without implying completion.
- **goal approval policy** — A goal-level choice between requiring user approval for every plan and sending plans through the goal guardian for automatic approval. It is independent of tool permission mode, and changes take effect at the next unresolved plan-approval boundary.
- **goal guardian** — A model-based reviewer that either approves an automatic goal's proposed plan or escalates it for user approval. It judges plans before implementation; it does not review completed work.
- **goal review** — A post-implementation evidence audit that decides whether the whole goal is complete, another goal cycle is required, or progress is blocked.
- **execution home** — The session and worktree in which a goal performs implementation; it is selected once for the goal and remains stable across cycles.
- **goal owner** — The single session allowed to continue a goal. Moving a goal to a worktree transfers ownership rather than copying it.
- **implementation context** — The context supplied to a goal's implementer: either the full goal transcript or a focused handoff containing the goal objective and accepted plan.
- **model workload** — A named kind of model request, such as planning, implementation, review, exploration, or compaction, whose model and reasoning effort can be configured by a preset.
- **conversation compaction** — Replacement of older model-visible transcript history with an anchored summary and a recent verbatim tail while retaining the original transcript on disk.
- **compaction target** — A model-visible transcript whose context pressure is managed independently through conversation compaction; currently either a main session segment or a persisted sub-agent transcript.
- **view buffer** — The compact, user-facing mevedel buffer with status, interaction, and input zones.
- **status strip** — The mevedel-owned clickable header-line chrome in a view buffer that routes status items to session cockpit surfaces.
- **data buffer** — The authoritative gptel/org transcript buffer backing a view buffer.
- **agent transcript view** — A read-only projection of a sub-agent transcript,
  backed by the live agent while it is running and by its saved transcript after
  it reaches a terminal state. It is an observation-only view-buffer variant,
  not a separate transcript presentation or interaction surface.
- **tool** — A model-callable operation routed through mevedel's validation, permission, execution, rendering, and persistence pipeline.
- **tool input repair** — A deterministic correction to a tool call that either responds to a specific contract violation or is explicitly requested by the tool. Ordinary default values are tool semantics, not repairs.
- **permission rule** — A rule deciding whether a tool call is allowed, denied, or requires user approval.
- **mention binding** — The association created when a mention's exact target first becomes known. A bound mention never falls back to a same-named target, editing its visible token removes the association, and an unavailable target becomes an explicit annotation in the temporary request without blocking the turn or changing the stored user text.
- **malformed mention binding** — A binding whose stored shape or visible token is inconsistent, so its intended target cannot be trusted. It blocks live submission or causes persisted history to be quarantined rather than being reinterpreted as an unbound mention.
- **unbound mention** — A mention whose exact target is not yet known. It remains eligible for live resolution rather than preserving a missing-target result.
- **skill** — A reusable prompt package discovered from configured skill directories and invoked by `$skill` syntax or model-side `Skill` tool.
- **skill invocation role** — The interpretation of one skill use as either an instruction or a command. The invocation surface determines the role; the skill does not declare it.
- **skill instruction invocation** — An embedded `$skill` mention that contributes a prepared skill body to the current consumer without invocation arguments or changes to the consumer's request policy.
- **skill command invocation** — A leading `$skill` invocation or model-side `Skill` call that supplies arguments and runs according to the skill's execution context.
- **skill execution context** — The location in which a skill command runs: `inline` in the current session context or `fork` in a dedicated sub-agent.
- **skill mention binding** — The association between one user-written `$skill` token and the exact discovered skill source it names, independent of later same-name lookup.
- **skill policy override** — Session-preset model and reasoning-effort policy applied to an existing skill invocation without changing or duplicating the skill.
- **skill roster** — The model-facing list of active skill names and descriptions, including only enough metadata for the model to decide whether to invoke a skill.
- **skill lookup** — A model-facing search over enabled model-invocable skills, including dormant path-scoped skills that are not present in the active skill roster.
- **plugin** — A reusable extension bundle discovered from a plugin manifest. A plugin may contribute skills, hooks, and other implemented extension components.
- **plugin activation** — A workspace-scoped decision that makes a plugin's implemented components active for sessions in that workspace until disabled.
- **pending plugin hook consent** — A plugin activation state where plugin skills may remain active, but executable plugin hooks are withheld until the user reviews and approves the changed hook surface.
- **hook audit record** — Persisted structured metadata or transcript markup describing how a hook changed model-visible context, submitted content, control flow, or permissions.
- **hook audit surface** — The user-facing record that a hook changed model-visible context, control flow, permissions, or submitted content.
- **plugin cockpit** — A session cockpit surface for inspecting installed plugins and managing one selected plugin at a time.
- **agent** — A specialized sub-agent such as explorer, coordinator, verifier, or reviewer.
- **task** — A tracked work item within a goal cycle or ordinary session work, optionally with dependency links.
- **persistent memory** — Durable user- or workspace-local memory included in future sessions.
- **ADR** — An architecture decision record under `docs/adr/` for durable decisions that should constrain future design work.

## Consumer rules

- Prefer these terms in issue titles, plans, tests, and architecture discussions.
- If a term is missing or ambiguous, ask or use `$grill-with-docs` to resolve it before adding competing vocabulary.
- Keep implementation module maps and detailed architecture in `AGENTS.md` and `docs/`; keep this file as a glossary.
