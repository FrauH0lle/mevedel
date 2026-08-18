# Compose system prompts from ordered profiles

Status: accepted

System prompts are rendered through one ordered profile mechanism. Reusable
components declare a file, literal text, or dynamic producer; profiles choose
those components, may add inline file or text components, and define render
order directly. Workspace-aware profiles must explicitly include workspace
configuration and environment components, so context cannot disappear through
implicit defaults. Main, agent, Bash guardian, and compaction
prompts use this mechanism. Agent definitions declare inline
`:system-components`; the rendered string is still frozen when the retained
agent is spawned.

Role and tone remain separate selectable components. Main owns the coding tone
and worker/explorer/verifier share a reporting tone. Memory is selected only for
main and worker.
Compaction uses an isolated single-component profile.

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
