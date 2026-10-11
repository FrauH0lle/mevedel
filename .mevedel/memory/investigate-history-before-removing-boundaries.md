---
name: "Investigate history before recommending removal of a boundary"
description: "Read the commits and ADRs that introduced a boundary before proposing to remove it"
type: feedback
---

# Investigate history before recommending removal of a boundary

User correction (2026-09-28): after the assistant recommended that planning and implementation share one conversation, the user asked it to check commit history — "I think there was a reason." Reading `61b5e7ca` and `docs/adr/0010-worktree-session-owns-plan-execution.md` led the assistant to retract the recommendation before it was acted on.

**Rule.** Before recommending that an existing boundary be removed, collapsed, or bypassed (session/worktree separation, module split, retained state), read the commits and ADRs that introduced it and state what they establish. Apparent user-experience friction is not evidence that the boundary was accidental.

**When to apply.** Whenever a boundary looks like overhead and the proposal is to delete it, or when a past decision is being treated as obsolete without its recorded reason.

**Related observations from the same session.**
- Explicit links between the planning and implementation views were called "better" by the user, but that did not explain why the second session exists or what it was for. Navigational continuity and the understandable purpose of retained sessions are distinct concerns (model inference).
- This does not weaken the workspace rule that ADRs are revisable: the point is to know the recorded reason so it can be argued with in the ADR instead of being overlooked.
