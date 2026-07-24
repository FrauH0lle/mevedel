# Compose system prompts from ordered profiles

Status: accepted

System prompts are rendered through one ordered profile mechanism. Reusable
components declare a file, literal text, or dynamic producer; profiles choose
those components, may add inline file or text components, and define render
order directly. Workspace-aware profiles must explicitly include workspace
configuration and environment components, so context cannot disappear through
implicit defaults. Main, revise, tutor, agent, Bash guardian, and compaction
prompts use this mechanism. Agent definitions declare inline
`:system-components`; the rendered string is still frozen when the retained
agent is spawned.

Role and tone remain separate selectable components. Main and revise share a
tone, worker/explorer/verifier share a reporting tone, and tutor owns its
tutoring tone. Memory is selected only for main, revise, tutor, and worker.
Compaction uses an isolated single-component profile.

This supersedes ADR 0021's exclusion of workspace instructions from guardian
system messages. The Bash guardian now receives scoped `AGENTS.md` /
`AGENTS.local.md` content and environment data after its dedicated role policy.
That project context can explain documented workflows, but cannot override the
guardian's risk criteria, advisory authority boundary, or response contract.
The guardian still excludes the coding-assistant prompt, transcript, tools,
memory, and skills; the Bash command and deterministic classifier facts remain
separate user-message evidence.
