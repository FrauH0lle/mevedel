# Agent Resource Roots

mevedel treats `.agents/` as the preferred shared agent-resource family and
`.mevedel/` as the mevedel-specific family. Agent resources such as skills
and persistent memory are read from both families. New portable resources
prefer `.agents/`, mevedel-specific resources use `.mevedel/`, and existing
resources are updated in place to avoid duplicating transitional state.

Hooks are excluded from this first resource-root merge because they execute
code and carry separate trust and command-resolution semantics.

When same-named skills are found in multiple roots or bundled skills, mevedel
keeps them all and uses the shortest unique visible prefix from scope
(`global`/`local`) and family (`agents`/`mevedel`), preferring family-only
prefixes when either dimension is sufficient. Bundled and managed skills keep
their existing `bundled:` and `managed:` conflict prefixes.

Persistent memory is merged from local `.mevedel/`, local `.agents/`, global
`.mevedel/`, then global `.agents/`, with earlier layers treated as more
specific when memories conflict. New broadly portable memory writes prefer
`.agents/` in the appropriate scope; existing memories are updated where they
already live.

Skill discovery uses the same local-before-global and `.mevedel/`-before-
`.agents/` order for ordinary resource roots, followed by bundled skills and
plugin skills.

Workspace instructions remain in ordinary `AGENTS.md` and `AGENTS.local.md`
files along the workspace directory chain. mevedel does not read `CLAUDE.md`
or introduce `.agents/AGENTS.md` as a second instruction location.
