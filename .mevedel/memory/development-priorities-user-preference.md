---
name: "Correctness and simplicity without losing features or performance"
description: "Prioritize correctness and maintainability, avoid overengineering, and confirm feature removal with the user"
type: user
---

# Mevedel development priorities

User statement, 2026-10-14:

- Prioritize correctness, maintainability, and reducing complexity or overengineering.
- Follow “make it work, make it correct, make it fast.”
- Performance remains important: substantial effort has already gone into improving it.
- Do not break or remove features without confirmation.

Apply these priorities when weighing implementations or proposing simplifications. They do not establish a backwards-compatibility requirement for old APIs or persisted formats; preserve the distinction between simplifying an implementation and removing a user-facing capability.
