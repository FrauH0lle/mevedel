---
name: "Fix encountered issues without deferral"
description: "Fix authorized findings in the same pass; a full backlog sweep includes defects, debt, flaky tests, and the Inbox."
type: feedback
modified: 2026-11-18
---

# Fix encountered issues without deferral

User correction, 2026-10-15: “Please do not defer any findings. Fix them immediately.”

For review-and-fix requests, address every authorized finding in the same pass. Recording a finding in `docs/backlog.md` or moving it to a later change does not resolve it.

On 2026-10-19, the user clarified that their browser collaboration UI/UX review request “meant review and fix,” not review-only. On 2026-10-20, during performance and async-publication work, the user said “Please fix all issues encountered” and “please consider both options and do not defer any more.” This applies to encountered implementation issues, including test flakes and coverage gaps.

## Distinguish area work from a full backlog sweep

- **Area work:** On 2026-10-31, the user corrected a one-time interpretation of checking `docs/backlog.md`. Revisit it as work proceeds, including the Inbox, and pick up bugs that fit the area being touched. This is not a direction to sweep unrelated work into every task.
- **Full backlog sweep:** In the 2026-11-17 session (`segment-0001.chat.org`, turn 13), the user corrected a `$pair` handoff that silently narrowed “solve the backlog” to selected sections. The agreed sweep included defects, technical debt, flaky tests, and the Inbox. Carry that scope into delegated instructions rather than treating the sections named in the draft as the limits of the request. Fix issues found along the way without creating new deferrals.

## Backlog curation must not become another deferral

**User statements, 2026-11-17:** The backlog should ideally contain only new features and items such as “Hook extensions”; defects and resolved entries should not remain there. The user later defined a “feature” as something that does not exist yet, not merely an extension of something existing. These examples do not establish a complete taxonomy.

The user also said, “No new deferrals. We either solve it now, delete or keep it.” Distinguish solving an item and removing its resolved entry, deliberately removing an obsolete or unwanted request, and explicitly retaining unresolved work. Relocating a finding is not a fix.

The user had not selected a replacement destination for defects or technical debt in the admitted evidence. A separate `docs/defects.md` was the assistant's recommendation, not an approved workflow. Keep actual work items and current dispositions in the appropriate tracker or maintained documentation, not memory.

## Review timing and authorization

The user favors implementing fixes incrementally while processing review notes when the environment supports hot-loading, rather than waiting for the entire review to finish. This preference does not override authorization, ownership, or coordination boundaries and does not authorize loading or evaluating modules in the user's running Emacs.

In the browser collaboration review, the user asked to be consulted before removing a user-visible feature or choosing between two reasonable UX designs. Fixing every finding does not mean silently making those choices. The backlog's solve/delete/keep wording does not relax separate prohibitions on proposing or removing user features.

These expectations apply within the authorized task. They do not override permission boundaries or explicit read-only, mock-only, or “no code changes yet” constraints. If a finding cannot be addressed within the available authority or evidence, report the blocker explicitly rather than silently deferring it or claiming completion.
