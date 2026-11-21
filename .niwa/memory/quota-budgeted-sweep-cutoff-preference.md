---
name: "Finish quota-budgeted sweeps before the reset"
description: "Weekly sweeps finish before the quota reset by default; post-reset continuation requires opt-in at intake."
type: user
modified: 2026-11-12
---

# Finish quota-budgeted sweeps before the reset

**User correction, 2026-11-06:** a quota-budgeted weekly sweep should finish before the weekly reset by default, so no next-week tokens are used. Continuing past the reset requires the user's opt-in at sweep intake. This supersedes the earlier flexible-cutoff preference.

**Why:** the retrospective on the 2026-10-31 sweep found that its dispatch cutoff held, but a late-started multi-round security review chain continued after the reset with Roland's approval. A dispatch cutoff alone therefore did not ensure completion before the reset. The approved continuation in that event is not standing permission for future sweeps.

**How to apply:**

- Plan review chains and their final rounds to finish before the reset, not merely to stop dispatching new implementation work before it.
- Use the weekly-sweep skill's maintained scheduling guidance for dispatch cutoffs, review timing, and checkpoints rather than treating historical clock times as a reusable schedule.
- Use available quota efficiently within the permitted window, but do not let utilization goals override the default finish boundary.
- If continuation was not opted into at intake, do not assume that unfinished work authorizes using next-week tokens.

This preference applies to quota-budgeted weekly sweeps, not to deadlines for unrelated work. Track the current sweep's schedule, remaining work, and approvals in its workflow material rather than memory.
