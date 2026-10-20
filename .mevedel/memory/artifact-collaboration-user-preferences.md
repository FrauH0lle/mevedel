---
name: "Unified artifact discussions, contextual routing, and visible titles"
description: "Prefer one Discuss mechanism across artifact types, preserve the viewing chat's context, show titles, and leave Git tracking user-controlled"
type: user
---

# Artifact collaboration preferences

User feedback recorded 2026-10-09:

- When an artifact is attached to a chat and the user comments while viewing it there, use **that chat's context**. Routing every comment to the artifact's dedicated session would lose the context the user explicitly attached.
- Whether `.mevedel/artifacts/` is committed to Git belongs to **the user or project**, not to mevedel.

User statement in the 2026-10-11 session, recorded 2026-10-12:

- Prefer one **Discuss** mechanism for HTML artifacts, whiteboards, and documents rather than separate comment and ask pathways: “It is the right direction.”

User statement recorded 2026-10-14:

- Prefer artifact titles, not hashes, as visible labels in the artifact list: “can we have the artifact titles and not their hashes?”

Apply these preferences when designing artifact discussion interfaces, contextual routing, list presentation, and storage policy. Detailed routing and discussion-lifecycle contracts belong in maintained documentation. Keeping leases outside the artifact store was the assistant's implementation inference, not an explicitly confirmed user preference.
