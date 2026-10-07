# Handoff: Claude subscription sessions through ACP (2026-10-06)

Status: implemented and verified on `feature/claude-code-engine`. See the
[acceptance index](claude-code-engine-acceptance.md) for A01–A21, final tests and
review results, and the [evidence log](claude-code-engine-progress.md) for
chronological run details. Start with `M-x mevedel-claude-code-setup`.

Validation after the thorough review fixes (2026-10-07): 9,446 test cases,
zero unexpected results, 24 conditional skips; 229 production files compile
without warnings. The [review and fix report](claude-code-engine-review-2026-10-07.md)
records all resolved findings, follow-up reviews, performance measurements,
and final evidence. Live subscription checks used
the existing Enterprise login, not a separate Pro/Max account. Session sidecars
now require v0.5.9; older records are rejected without migration, not deleted.
The strict Goal record additionally requires `:tokens-incomplete-p`; earlier
development Goals missing it are dropped on normal restore. The standalone
v0.5.6 converter preserves Goals by adding this field. No features were removed.

## Canonical implementation specification

The [PRD](../../.scratch/claude-code-engine/PRD.md) is the current implementation
contract, retained in the local issue tracker with `Status: implemented`.
It includes user stories, required versus capability-limited behavior,
implementation milestones, acceptance cases and the autonomous Goal command.
The user confirmed its test seams: the existing mevedel session workflow,
with focused ACP/MCP contract tests underneath.

The PRD is local-only planning state. It is intentionally gitignored and will
not travel with a Git commit; retain it in this worktree for implementation.

Worktree: `/home/roland/Projekte/mevedel/.scratch/worktrees/claude-code-engine`
Branch: `feature/claude-code-engine`
Starting commit: `5adfcfb28c4504968bc76b70a8bed18e8038f776`

## Goal and chosen direction

Use a Claude Pro/Max subscription inside mevedel through normal provider/model
selection, while retaining mevedel tools, permissions, patch review, execution
targets, retained agents and saved-session workflows. Authentication stays in
Claude's supported login flow. Mevedel generates integration wiring.

- **ACP** connects mevedel to the external agent for session communication,
  streaming, cancellation and supported lifecycle operations.
- **MCP** exposes mevedel's existing tools to that agent. Built-in execution is
  disabled; effects retain mevedel's authoritative pipeline.
- Claude owns model history, compaction, thinking and cache. Mevedel owns its
  canonical displayed transcript, request settlement and workflow state.
- Shared ACP behavior is reusable; Claude-specific prompt/tool setup remains
  isolated. Support for a second agent is outside the first delivery.

Proceed with option two. Build a focused ACP/MCP feasibility slice, then finish
production integration. Do not build option one or a comparative bake-off.
Direct stream-json is not the default connection or a second required engine.
A prototype alone does not satisfy the goal.

## Corrections to the earlier investigation

- Model history must follow root, directive and retained-agent context scopes;
  one external history per displayed session would leak unrelated context.
- A saved external session ID alone does not prove durable reconciliation or
  portable resume. Same-machine restart/resume is required; unsupported exact
  history operations must be disabled before changing files or conversation.
- Memory consolidation already uses bounded investigation tools. Side
  conversations also use tools. Only genuinely stateless tool-free workloads
  can use a short-lived tool-free conversation.
- Structured pipeline outcomes omit provider projection responsibilities.
  Preserve result persistence, media, reminders and transcript evidence without
  executing tools twice.
- Compaction requires timely context restoration during the current turn,
  not merely at the next user prompt. Boundary stops must retain their
  successful-settlement meaning rather than becoming arbitrary aborts.
- The inspected CLI's `--bare` mode disables subscription login; verify
  isolation and authentication together through the ACP adapter.

## Original implementation Goal

Run in the dedicated worktree:

```text
/goal Implement .scratch/claude-code-engine/PRD.md: use a Claude Pro/Max subscription inside mevedel through ACP and mevedel tools over MCP. Complete the required workflows and acceptance cases, including the initial feasibility slice, tests and implemented documentation. Resolve routine engineering questions autonomously; report demonstrated blockers and allowed capability limitations honestly. A prototype alone does not complete the goal.
```

The implementation is complete. The original Goal followed the PRD's acceptance
contract and repository development rules.

## Supporting material

[Original research](claude-code-subscription-integration.md) retains source
observations and alternatives considered. Its old transport/bake-off
recommendations are historical and superseded by the PRD. Verify installed
versions and external policy before relying on old observations.

Keep implementation evidence and ordinary handoffs under `work://shared/`.
Keep local issues and this PRD in the skill-configured tracker. Amend current
manuals, glossary and ADRs only when their described behavior is implemented.
