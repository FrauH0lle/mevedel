# Compose system prompts from ordered profiles

Status: accepted. Incorporates ADR 0021's guardian boundary.

## Current decision

System prompts use ordered profiles of reusable named components and inline
file/text components. A named component can read a file, supply literal text,
or produce dynamic content. Workspace-aware profiles explicitly include
workspace configuration and environment; validation rejects missing required
components and duplicate names. Main, agent, approval reviewer, and Buddy prompts
use this mechanism. The isolated context-summary generator owns a fixed prompt.

Role, tone, shared task policy, and memory use/save policy are separate
components. Main owns the coding tone; worker, explorer, and verifier share a
reporting tone. Main and worker select memory saving guidance. Buddy selects
memory context and use policy without saving instructions. The save policy
requires retrieving `mevedel://memory.md` before mutations; that manual owns
root selection, frontmatter, and index maintenance.

Retained agents freeze their behavioral system content and selected observation
names at spawn. Named workspace, memory, journal, environment, skill, resource,
and Goal observations are delivered according to
[ADR 0115](0115-retain-delivered-conversation-fragments.md), subject to the
profile's selection. Authored inline components remain in the system prompt.
Changing a retained role contract requires a new agent.

The approval reviewer receives only its trusted approval policy as a system
component. Actual root user turns, Goal objective, operation, target and authority
facts arrive as quoted evidence. It excludes tools, ambient conversation, memory,
skills and workspace instructions from the trusted system prompt. Its profile
is `permission-review`, with `:workspace-aware nil`.

The [architecture manual](../architecture.md) describes composition and retained
instruction delivery. The [guardian manual](../guardian-prompts.md) owns approval
decisions, evidence bounds and examples.

## Rationale and consequences

Explicit ordered composition makes prompt dependencies visible at their owner.
Shared policy has one home; roles retain distinct duties and machine-consumed
report formats. Guardians and summaries do not inherit coding task policy.
A retained role does not need a per-turn reminder to reconstruct policy that is
restored outside the compacted transcript.

Separating stable policy from changing observations avoids unrelated prefix
changes and keeps agent behavior predictable. Absolute modification dates give
memory freshness information without rewriting unchanged data every day.
Retrieving the memory manual reduces always-visible procedure text, but relies
on model compliance with the before-write trigger; compliance has not been
established for every model.

## Decision history

- **ADR 0021 isolated two guardians completely**, including excluding workspace
  instructions. Each owned a full trusted system prompt and received reviewed
  material as untrusted user content. Duplicating their small trust-boundary
  wording was preferred to a shared template because their authority and response
  contracts differed. The Goal guardian was tool-free and trusted explicit PRD
  or ticket references rather than independently investigating them.
- **The Bash guardian gained scoped workspace context in ADR 0070.** Documented
  project workflows can explain the command under review without importing the
  coding assistant or changing the guardian's authority. Ordered components
  replaced the blanket workspace exclusion. The former automatic Goal planning
  guardian was removed with the phase-based workflow; see
  [ADR 0067](0067-separate-planning-from-goal-execution.md#decision-history).
- **Captured prompts exposed repeated workflow and routing policy** across role
  text, descriptions, and unconditional verifier/reviewer reminders. Shared
  `task-policy` and role-local report contracts replaced these duplicates. Custom
  role names no longer acquire an undeclared read-only reminder. Tool authority
  did not change with that prompt cleanup.
- **ToolScript activation rewrote earlier system policy** to promote a tool whose
  schema already explained it. That activation policy was removed. Tool discovery
  and the later ToolCall surface retain their own contracts.
- **Memory inspection found saving procedures in passive Buddy requests and
  relative index ages that changed daily.** Use policy, save policy, and data
  were split. On 2026-09-08 the ordinary save procedure moved from the inline
  policy to the retrieved manual, preserving the visible before-write trigger.
- **Tutor mode was removed.** It required users to summon teaching before knowing
  they needed it and then refused the requested answer; ordinary chat answered
  those questions better. The pedagogical purpose moved to optional Buddy notes
  that can be ignored without interaction. Tutor profiles, presets, components,
  and tools were removed; ordered composition remains.

- **Delegated approval replaced advisory risk annotation in September 2026.**
  The accepted friction reduction requires actual user intent and the complete
  capability request. Because the reviewer can now approve an invocation,
  workspace instructions were removed from trusted system policy; evidence is
  supplied separately. The `guardian` workload is retained.
