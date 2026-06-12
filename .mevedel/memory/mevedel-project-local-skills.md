---
name: mevedel-project-local-skills
description: mevedel development skills are project-local workflows, not bundled end-user skills
type: project
---

Mevedel development skills such as refactor, review/verify loops, diagnosis, TDD, and architecture-improvement workflows are intended for this repository's project-local development workflow, not for shipment as bundled mevedel skills for users.

**Why:** They encode how this project should be developed and maintained, rather than product functionality for downstream mevedel users.

**How to apply:** Recommend placing them under project-local skill configuration such as `.mevedel/skills/`, and avoid treating them as bundled `skills/` package content unless the user explicitly asks to productize one.
