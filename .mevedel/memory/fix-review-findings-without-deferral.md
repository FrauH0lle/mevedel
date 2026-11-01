---
name: "Fix encountered issues without deferral"
description: "Revisit the backlog during work and resolve authorized, relevant findings without silently deferring them."
type: feedback
---

# Fix encountered issues without deferral

User correction, 2026-10-15: “Please do not defer any findings. Fix them immediately.”

For review-and-fix requests, address every review finding in the same pass. Do not treat recording a finding in `docs/backlog.md` as resolving it, or postpone it to a later change.

On 2026-10-19, the user clarified that their request for a browser collaboration UI/UX review prompt “meant review and fix,” not review-only. For that work, every finding should be fixed in the same pass, with nothing moved to the backlog.

On 2026-10-20, during performance and async-publication implementation work, the user said “Please fix all issues encountered” and “please consider both options and do not defer any more.” Apply the no-deferral expectation to encountered implementation issues too, including test flakes and coverage gaps, rather than merely listing them as residuals.

On 2026-10-31, the user corrected a one-time interpretation of checking `docs/backlog.md`: revisit it as work proceeds, including its Inbox, and pick up bugs that fit the area currently being touched. A planning-time scan alone does not satisfy this expectation. This is not a direction to sweep unrelated backlog items into every task.

The user also favors implementing fixes incrementally while processing review notes when the environment supports hot-loading, rather than waiting for the entire review to finish. Apply this timing preference to suitable review workflows; it does not override authorization, ownership, or coordination boundaries.

In the browser collaboration review context, the user asked to be consulted before removing a user-visible feature or choosing between two reasonable UX designs. Fixing every finding does not mean silently making those choices.

**Why:** The user explicitly rejected leaving discovered issues for future work and corrected review-only interpretations, implementation deferrals, and treating backlog consultation as a single planning step. Incremental implementation is a preferred way to conduct suitable reviews, not permission to bypass necessary coordination or checks.

**Scope:** These expectations apply within the authorized task. They do not authorize bypassing permission boundaries or an explicit read-only, mock-only, or “no code changes yet” constraint. If a finding cannot be addressed within the available authority or evidence, report the blocker explicitly rather than silently deferring it or claiming completion.
