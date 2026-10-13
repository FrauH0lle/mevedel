---
name: "Artifact comments retain chat context; Git tracking stays user-controlled"
description: "Use the viewing chat's context for attached-artifact comments, and leave artifact-store Git tracking to the user or project"
type: user
---

# Artifact collaboration preferences

User feedback recorded 2026-10-09:

- When an artifact is attached to a chat and the user comments while viewing it there, use **that chat's context**. Routing every comment to the artifact's dedicated session would lose the context the user explicitly attached.
- Whether `.mevedel/artifacts/` is committed to Git belongs to **the user or project**, not to mevedel.

Apply these constraints when designing artifact comment routing and storage policy. Keeping leases outside the artifact store was the assistant's implementation inference, not an explicitly confirmed user preference.
