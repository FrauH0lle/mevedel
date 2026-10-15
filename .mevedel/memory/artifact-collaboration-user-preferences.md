---
name: "Artifact context, visible titles, and user-controlled Git tracking"
description: "Use the viewing chat's context for artifact comments, display titles rather than hashes, and leave Git tracking user-controlled"
type: user
---

# Artifact collaboration preferences

User feedback recorded 2026-10-09:

- When an artifact is attached to a chat and the user comments while viewing it there, use **that chat's context**. Routing every comment to the artifact's dedicated session would lose the context the user explicitly attached.
- Whether `.mevedel/artifacts/` is committed to Git belongs to **the user or project**, not to mevedel.

User statement recorded 2026-10-14:

- Prefer artifact titles, not hashes, as visible labels in the artifact list: “can we have the artifact titles and not their hashes?”

Apply these constraints when designing artifact comment routing, list presentation, and storage policy. Keeping leases outside the artifact store was the assistant's implementation inference, not an explicitly confirmed user preference.
