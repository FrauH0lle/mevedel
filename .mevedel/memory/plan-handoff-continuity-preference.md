---
name: "Preserve conversational continuity at plan handoff"
description: "Plan acceptance should preserve the conversation and window, without leaving a confusing obsolete planning view"
type: user
---

# Preserve conversational continuity at plan handoff

User feedback, 2026-10-16:

- Switching to a new buffer after accepting a plan breaks the flow. Implementation should retain the planning conversation.
- When Worktree is chosen, the implementation view should replace the planning view in the **same window**.
- Close the no-longer-needed planning buffer to “reduce potential confusion.”
- Context should default to **Current** when Worktree is selected.

Apply this preference when designing plan-to-implementation transitions. It describes conversational and navigational continuity, not permission to eliminate worktree isolation or ignore the historical reasons for session boundaries.
