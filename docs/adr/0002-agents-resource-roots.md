# Agent Resource Roots

mevedel treats `.agents/` as the interoperable agent-resource family shared
with other AI harnesses and `.mevedel/` as its mevedel-native counterpart.
Every supported `.agents/XYZ` resource has a corresponding `.mevedel/XYZ`
override unless an ADR records a deliberate exception. New portable resources
prefer `.agents/`, mevedel-specific resources and overrides use `.mevedel/`,
and existing resources are updated in place.

Unless a subsystem documents a deliberate exception, resource precedence from
highest to lowest is workspace `.mevedel/`, workspace `.agents/`, global
`~/.mevedel/`, global `~/.agents/`, then explicit defcustom roots. This
precedence decides conflict winners; ordered systems such as hooks may execute
lower-precedence layers first so higher-precedence layers can refine them.
Systems that retain colliding resources under prefixes, such as skills, keep
both families instead of discarding the lower-precedence entry.

Standalone hooks are included in the resource-root model. Global
`.agents/hooks.*` files are trusted user files like `~/.mevedel/hooks.*`;
project `.agents/hooks.*` files use the same workspace trust-by-hash model
as project `.mevedel/hooks.*`. Plugin hooks may also come from plugins
discovered in either plugin family, but those plugins remain inactive until
explicitly enabled in a workspace.

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
plugin skills. Hook execution visits lower-precedence hook roots before
higher-precedence hook roots because later hook rewrites win. Plugin
discovery includes local `.mevedel/plugins/` and `.agents/plugins/` roots,
followed by global `~/.mevedel/plugins/` and `~/.agents/plugins/` roots, but
discovery never implies activation. `/plugin install` writes new installs to
the interoperable global `~/.agents/plugins/` root. Existing managed global
plugins in either family are updated and removed in place.

When the same plugin manifest name appears in multiple roots, mevedel uses
root precedence rather than exposing multiple prefixed plugin names:
workspace `.mevedel/plugins/`, workspace `.agents/plugins/`, global
`~/.mevedel/plugins/`, global `~/.agents/plugins/`, then extra roots.
Shadowed duplicates should be visible in plugin listing output.

Workspace instructions remain in ordinary `AGENTS.md` and `AGENTS.local.md`
files along the workspace directory chain. mevedel does not read `CLAUDE.md`
or introduce `.agents/AGENTS.md` as a second instruction location.
