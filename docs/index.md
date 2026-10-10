# mevedel documentation

mevedel is an Emacs Lisp package that provides a visual, overlay-based
workflow for interacting with LLMs while programming, with direct gptel
integration.

This manual describes the current system for users and agents working in the
codebase. The sidebar groups workflows, architecture, tool reference, and
development guidance. [Decision records](adr/README.md) explain architectural
choices and their history; [the backlog](backlog.md) is the sole future-work
section. The repository root
[`AGENTS.md`](https://github.com/FrauH0lle/mevedel/blob/master/AGENTS.md) is
the agent entry point. The documentation map below locates area contracts;
[module-map.md](module-map.md) locates implementation modules.

Good starting points:

- [Claude subscription setup](https://github.com/FrauH0lle/mevedel/blob/master/README.md#claude-promax-subscriptions) — use an installed Claude Pro/Max login through the guided setup.
- [Architecture](architecture.md) — data structures, workspace context chain,
  gptel integration, persistence layout
- [Tools](tools.md) and [Permissions](permissions.md) — the tool pipeline and
  the permission decision chain
- [Agents](agents.md) — worker, explorer, verifier, reviewer
- [Architecture Decision Records](adr/README.md) — why the design is what it is
- [Instruction context delivery](architecture.md#retained-instruction-context) — stable policy and retained changing context.
- [Development guide](development.md) — required preparation, implementation and verification procedures.

## Documentation map

Read the relevant contracts before planning or changing an unfamiliar area.

- [`architecture.md`](architecture.md) — key data structures
  (`mevedel-workspace`, `-session`, `-request`, `-tool`), workspace
  context chain, gptel integration, persistent memory layout, chat
  buffer formatting
- [`address-to-resource.md`](address-to-resource.md) — closed
  resource-address families, canonical locators, operation matrix, permission
  seam, lifecycle, freshness, and capability boundaries
- [`memory.md`](memory.md) — curated memory, journal capture and retention,
  shared working files, consolidation, and proposal decisions
- [`view.md`](view.md) — dual-buffer view model, status /
  interaction / input zones, rendered agent transcript views, input
  history, and the [workspace artifact store](view.md#artifact-store)
- [`collaboration.md`](collaboration.md) — browser sharing, bearer-link
  authority, typed guest input, notifications, and relay reconnection
- [`shared-editing.md`](shared-editing.md) — concurrent whiteboards and documents,
  native model tools, editor isolation, item leases, host saves, versions, and
  recovery
- [`tools.md`](tools.md) — tool pipeline, hook ordering, permission checks,
  mutation snapshots, result projection and persistence, `:wrap` /
  `:groups`, renderers and render-data side channel, oversized result
  persistence
- [`ptc-dialect.md`](ptc-dialect.md) — closed ToolCall language,
  pure primitives, nested tools, parallel calls, limits, and security boundary
- [`permissions.md`](permissions.md) — permission decision chain,
  bucket precedence, Bash/Eval specifics,
  sub-agent permission propagation, example config
- [`guardian-prompts.md`](guardian-prompts.md) — trusted guardian
  prompts, untrusted evidence boundaries, response contracts, examples
- [`agents.md`](agents.md) — worker/explorer/verifier/reviewer,
  retained asynchronous spawning, canonical paths, mailboxes, waits,
  tree-wide capacity, and task status
- [`preview.md`](preview.md) — inline diff overlay,
  keybindings, mode dispatch, handler return shape
- [`plan-mode.md`](plan-mode.md) — sticky Plan conversations,
  proposal approval axes, tool boundary, implementation handoff, recovery
- [`mentions.md`](mentions.md) — `@ref`/`@file`/`@agent`/`@mcp`
  expansion, dedup, completion CAPFs
- [`skills.md`](skills.md) — SKILL.md discovery, `$skill` invocation and local slash
  commands, model-side Skill, allowed-tools, model / effort
  overrides, forked skill dispatch, review skill
- [`hooks.md`](hooks.md) — hook subsystem:
  lifecycle events, config layers, command/Elisp handlers, pipeline
  integration, trust model, dry-run inspection, logs
- [`reminders.md`](reminders.md) — retained delivery history,
  staging seams, injection contract, hidden injection record, implemented
  reminder surface
- [`goals.md`](goals.md) — Goal context, continuation, accounting,
  failures, commands, recovery, and accepted-plan authority
- [`sessions.md`](sessions.md) — on-disk layout, segment
  persistence contract, resume/rewind/fork, locking, auto-cleanup,
  defcustoms
- [`compaction.md`](compaction.md) — manual and automatic
  conversation compaction, token thresholds, gptel token baseline,
  anchored summaries, tail preservation, segment integration
- [`telemetry.md`](telemetry.md) — append-only lifecycle telemetry,
  data policy, profiler artifacts, prompt guard, Goal reproduction procedure
- [`buddy.md`](buddy.md) — unasked review of recent edits and
  the guidance command, note lifecycle, ephemerality, model selection
- [`commits.md`](commits.md) — commit message format and
  guidelines
- [`backlog.md`](backlog.md) — canonical
  concise future-work requests and unresolved defects
