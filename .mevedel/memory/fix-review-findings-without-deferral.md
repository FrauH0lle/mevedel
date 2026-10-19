---
name: "Fix review findings without deferral"
description: "Browser collaboration UI reviews mean review and fix; resolve findings in the same pass while asking about consequential UX choices"
type: feedback
---

# Fix review findings without deferral

User correction, 2026-10-15: “Please do not defer any findings. Fix them immediately.”

For review-and-fix requests, address every review finding in the same pass. Do not treat recording a finding in `docs/backlog.md` as resolving it, or postpone it to a later change.

On 2026-10-19, the user clarified that their request for a browser collaboration UI/UX review prompt “meant review and fix,” not review-only. For that work, every finding should be fixed in the same pass, with nothing moved to the backlog.

In the same browser collaboration review context, the user asked to be consulted before removing a user-visible feature or choosing between two reasonable UX designs. Fixing every finding does not mean silently making those choices.

**Why:** The user explicitly rejected leaving discovered issues for future work, including findings already placed in the backlog, and corrected a review-only interpretation of their browser UI request.

This is an expectation about completing the requested review-and-fix work, not authorization to bypass permission boundaries or an explicit read-only, mock-only, or “no code changes yet” constraint. If a finding cannot be addressed within the available authority or evidence, report the blocker explicitly rather than silently deferring it or claiming completion.
