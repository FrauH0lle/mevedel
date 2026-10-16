---
name: "Fix review findings without deferral"
description: "In review-and-fix work, address every finding in the same pass rather than parking it in the backlog"
type: feedback
---

# Fix review findings without deferral

User correction, 2026-10-15: “Please do not defer any findings. Fix them immediately.”

For review-and-fix requests, address every review finding in the same pass. Do not treat recording a finding in `docs/backlog.md` as resolving it, or postpone it to a later change.

**Why:** The user explicitly rejected leaving discovered issues for future work, including findings already placed in the backlog.

This is an expectation about completing the requested review-and-fix work, not authorization to bypass permission boundaries. If a finding cannot be addressed within the available authority or evidence, report the blocker explicitly rather than silently deferring it or claiming completion.
