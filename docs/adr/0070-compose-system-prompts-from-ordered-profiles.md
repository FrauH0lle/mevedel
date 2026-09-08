# Compose system prompts from ordered profiles

Status: accepted

System prompts are rendered through one ordered profile mechanism. Reusable
components declare a file, literal text, or dynamic producer; profiles choose
those components, may add inline file or text components, and define render
order directly. Workspace-aware profiles must explicitly include workspace
configuration and environment components, so context cannot disappear through
implicit defaults. Main, agent, Bash guardian, and compaction
prompts use this mechanism. Agent definitions declare inline
`:system-components`; behavioral system content is frozen when the retained
agent is spawned. As extended by ADR 0115, selected named observations are
instead delivered as retained current-context updates. The selected component
names are frozen and persisted alongside the role contract; authored inline
components remain in the system prompt.

Role and tone remain separate selectable components. Main owns the coding tone
and worker/explorer/verifier share a reporting tone. Memory saving guidance is
selected only for main and worker; Buddy profiles also select memory context
and use policy, without saving instructions.
Compaction uses the isolated context-summary generator's fixed prompt.

Amendment: captured effective prompts showed repeated routing/workflow policy
in roles and descriptions, while the former ToolScript activation rewrote the earlier
system prompt to promote a tool already described in its own schema. Coding
profiles now share an explicit `task-policy` component for scope, permission,
trust, existing edits, and verification. Roles keep their distinct obligations
and machine-consumed report formats; tone owns communication. ToolScript
activation no longer adds system policy. This retains ordered composition and
frozen agent prompts while reducing competing homes and avoidable prefix churn.
Guardians and summaries do not select the coding task policy.

The same inspection found verifier/reviewer contracts duplicated in unconditional
per-turn reminders. Frozen role policy is restored before every request and does
not live in the compacted transcript, so these name-triggered reminders are
removed. Role-local judgment and report formats remain in the role prompt; direct
tool rosters and permission enforcement are unchanged. Custom role names no longer
implicitly acquire an undeclared read-only reminder.

Memory inspection also found save procedures in passive Buddy requests and
relative index ages that rewrote unchanged context daily. Memory use policy,
save policy, and root/index data are now separate explicit components. Static
policies precede changing data; absolute modification dates retain freshness
metadata without daily text churn. The initial split kept the ordinary save procedure inline. The 2026-09-08
follow-up moves root selection, frontmatter and index-maintenance procedure to
`mevedel://memory.md`; the always-visible save policy requires retrieving that
manual before explicit or model-initiated mutations. This preserves a visible
before-write trigger while removing procedures unrelated to most turns.
Behavioral compliance with that trigger is not established for every model.

This supersedes ADR 0021's exclusion of workspace instructions from guardian
system messages. The Bash guardian now receives scoped `AGENTS.md` /
`AGENTS.local.md` content and environment data after its dedicated role policy.
That project context can explain documented workflows, but cannot override the
guardian's risk criteria, advisory authority boundary, or response contract.
The guardian still excludes the coding-assistant prompt, transcript, tools,
memory, and skills; the Bash command and deterministic classifier facts remain
separate user-message evidence.

Amendment: Tutor mode was removed. It required the user to summon it before
knowing they needed teaching, then refused to answer what was asked, so the
chat buffer answered the same questions better without it. Its pedagogical
angle now reaches the user through Buddy notes, which arrive unasked and cost
nothing to ignore. Every tutor profile, component, preset, and tool named above
is gone; the surrounding mechanism is unchanged.
