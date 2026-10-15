---
name: "Review observational Eval by its visible effects"
description: "Unconfined execution alone is not a reason to defer task-relevant observational Eval; preserve safety boundaries and bounded explanations"
type: feedback
---

# Review observational Eval by its visible effects

On 2026-10-14, the assistant proposed judging task-relevant observational live Eval by the visible expression's effects, rather than asking solely because execution in Emacs is unconfined. Examples were inspection of function metadata and view settings. The user explicitly confirmed: “please do that.”

For permission-review design:
- Assess the visible expression's effects and task relevance.
- Do not defer an otherwise observational expression merely because Emacs execution lacks confinement.
- Retain bounded reviewer explanations so unnecessary deferrals can be diagnosed after transient approval cards disappear.

This confirmation does not authorize mutations, unknown effects, or access to unrelated private data, and does not override applicable permission rules.

Validation caveat: offline prompt assertions established policy serialization, not live-provider classification accuracy or reduced interruptions; no live-provider replay tested those outcomes in the captured evidence.
