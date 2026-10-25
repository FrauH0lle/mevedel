---
name: "Fix encountered issues without deferral"
description: "Resolve encountered issues in authorized implementation and review-and-fix work, including flakes and coverage gaps; preserve consequential-choice and permission boundaries"
type: feedback
---

# Fix encountered issues without deferral

User correction, 2026-10-15: “Please do not defer any findings. Fix them immediately.”

For review-and-fix requests, address every review finding in the same pass. Do not treat recording a finding in `docs/backlog.md` as resolving it, or postpone it to a later change.

On 2026-10-19, the user clarified that their request for a browser collaboration UI/UX review prompt “meant review and fix,” not review-only. For that work, every finding should be fixed in the same pass, with nothing moved to the backlog.

On 2026-10-20, during performance and async-publication implementation work, the user said “Please fix all issues encountered” and “please consider both options and do not defer any more.” Apply the no-deferral expectation to encountered implementation issues too, including test flakes and coverage gaps, rather than merely listing them as residuals.

In the browser collaboration review context, the user also asked to be consulted before removing a user-visible feature or choosing between two reasonable UX designs. Fixing every finding does not mean silently making those choices.

**Why:** The user explicitly rejected leaving discovered issues for future work and corrected both a review-only interpretation and further implementation deferrals.

This expectation applies within the authorized task. It is not authorization to bypass permission boundaries or an explicit read-only, mock-only, or “no code changes yet” constraint. If a finding cannot be addressed within the available authority or evidence, report the blocker explicitly rather than silently deferring it or claiming completion.
