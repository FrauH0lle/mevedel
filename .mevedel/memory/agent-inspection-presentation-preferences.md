---
name: "Agent model attribution and read-only inspection"
description: "Show accurate model/effort metadata and preserve the parent workspace during agent inspection"
type: user
---

# Agent model attribution and inspection

On 2026-10-14, the user explicitly approved the proposed agent presentation mockups: “Yes, that looks really good.”

## Model and effort presentation

- Show available model and effort in the agent transcript's top status line, the parent launch row, and agent status above the composer.
- Use full provider/model/effort in the transcript header and compact `Model:effort` in parent rows.
- Do not add a model inventory to the collapsed aggregate status.
- Omit unknown effort rather than inventing a default.
- Distinguish the execution model from the model used to prepare summary context.
- Keep launch metadata historical; live headers and active-agent rows reflect current resolved configuration.

These distinctions prevent misleading attribution of which model did the work.

## Agent inspection

Same-window inspection should provide a visible Parent return affordance and preserve the parent's draft, point, and scroll position. Inspection remains read-only; actionable interactions stay in the parent.

The user did not explicitly choose same-window opening as the default instead of an alternate action. The assistant's plan to retain a right-side default was not a user-confirmed preference.
