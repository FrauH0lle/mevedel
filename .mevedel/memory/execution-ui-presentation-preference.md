---
name: "Background-work UI: minimal, unified, and composer-stable"
description: "Unify running work, avoid duplicate status, keep controls out of busy headers, and prevent status growth from displacing the composer"
type: user
---

# Execution and background-work UI preferences

## Minimal presentation and coherent grouping

User statement, 2026-09-27 preview comparison: favors a **work-oriented default**. Liked A's minimal command row much more than B's result card with output preview. Saw potential usefulness in C's completion breadcrumb but did not choose it over A.

User statement, 2026-10-16: the executions/Monitor UI feels awkward. Clicking the row above the composer is not expected to open the cockpit; executions and monitors belong to the same category.

User statements, 2026-10-17:

- Unify background work into one area, **including agents**.
- The live panel should show what is actually running. Finished items should disappear because completion is represented by the event row in the conversation.
- Avoid repeating the panel header's counts in the footer: the user called that redundant.
- Keep headers uncluttered. The user found `Manage…`'s trailing dots strange and wanted the control out of the busy header. They explicitly accepted `[Manage]` in its own last panel row, with `[Show all N]` / `[Show fewer]` for overflow.

These are presentation preferences, not a specification of exact timing or every interaction.

## Composer stability and interactive rows

User statement, 2026-10-17: the main layout discomfort is status content growing and pushing the composer “away.” Prefer status content **below the composer**, while allowing the composer to grow during typing.

In the same discussion, the user rejected `after-string` designs for interactive rows: rows must remain real buffer text so point can move to them and invoke their actions. Preserve that interaction requirement when exploring layouts.

## Scope and unconfirmed proposals

- For the October 17 redesign, the user explicitly requested planning and mockups first, with “Not code changes yet.” This was a constraint on that design phase, not a standing prohibition on UI implementation.
- “One completion breadcrumb for every command that continues in the background, regardless of delivery mechanism” was an assistant recommendation, not user confirmation. The user said “I don't get that difference” when asked about polling versus independent notification.
- The October 16 inline-expansion proposal was not confirmed.
- Exact generic footer wording, removing the `All…` link, and a repeated-Monitor-event collapse policy were not confirmed in the supplied evidence.
- Fixed reserved panel height and the proposed marker-based composer layout were assistant proposals, not an accepted design.

Do not cite these unconfirmed details as user intent.
