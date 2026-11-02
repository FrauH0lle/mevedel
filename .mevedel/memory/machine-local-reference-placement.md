---
name: "Keep machine-specific reference maps out of general documentation"
description: "General mevedel documentation must not depend on local checkouts or machine-specific source paths"
type: feedback
---

# Keep machine-specific reference maps out of general documentation

On 2026-10-24, the user requested removing “# Local reference sources” from general mevedel documentation because it “makes only sense on this machine but not in general for mevedel” (removed in `f06238ed`).

Do not put local checkouts or machine-specific comparison sources into general project documentation; it must not depend on another contributor having the same local paths or source installations.

The map was first moved to the local memory `local-reference-sources.md`, which the user then deleted as well. Do not recreate such a map in memory unprompted; `.mevedel/memory/` is tracked in Git, so it is not machine-local either.
