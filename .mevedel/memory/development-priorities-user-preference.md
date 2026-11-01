---
name: "Correctness and simplicity without losing features or performance"
description: "Avoid overengineering, record delegated decisions, preserve user features, and measure promising performance alternatives."
type: user
---

# Mevedel development priorities

User statement, 2026-10-14:

- Prioritize correctness, maintainability, and reducing complexity or overengineering.
- Follow “make it work, make it correct, make it fast.”
- Performance remains important: substantial effort has already gone into improving it.
- Do not break or remove features without confirmation.

User corrections, 2026-10-29: a redesign intended to simplify the system should not become an excuse for expanding it with more machinery. Complex problems can require complex solutions, but not every problem does. The motivating example was ephemeral working notes: the user preferred removing undo rather than engineering more safeguards around deleting an old text file. Apply the maintained proportionate-solutions rules rather than treating all data as equally consequential.

User correction, 2026-10-30: do not dismiss promising performance alternatives as too complex without estimating their benefit. Use a limited benchmark or throwaway prototype to answer whether the likely gains justify the added complexity. The motivating case was archive batching and unchanged-result reuse in a history scan.

User statement, 2026-10-31: during `$improve-codebase-architecture`, and generally when making decisions on the user's behalf, favor “better performance, less complexity, less over engineering.” Record those decisions. Never remove user features without explicit user approval.

**How to apply:** prefer the simplest design that meets the actual need. When a plausible performance alternative could materially change the tradeoff, make a bounded measurement before rejecting it; a prototype is evidence for a decision, not a commitment to ship more machinery. This is not a requirement to benchmark every speculative optimization.

Record delegated decisions in the appropriate tracker or maintained documentation, following the applicable workflow—not in memory as changing project state. Recording a decision does not substitute for obtaining required approval.

These preferences do not establish a backwards-compatibility requirement for old APIs or persisted formats. Preserve the distinction between simplifying an implementation and removing a user-facing capability.
