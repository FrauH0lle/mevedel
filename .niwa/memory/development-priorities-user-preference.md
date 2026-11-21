---
name: "Correctness and simplicity without losing features or performance"
description: "Reduce implementation and UX complexity without optimizing concept counts or imposing unevidenced restrictions."
type: user
modified: 2026-11-12
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

**Audit-specific user prohibition, 2026-11-07** (turn 17): audits such as ponytail-audit must not remove features, and “lean/delete” findings may not propose removing a user feature. Do not turn this into an audit recommendation to delete a feature after seeking approval. Simplify implementation without proposing loss of user-facing capability.

**UX-simplicity correction, recorded 2026-11-09** (`segment-0001.chat.org`, turn 11): the redesign goal is a simpler interface/UX, not fewer conversation kinds as an end in itself. “We should have as many types as we need and preferably we do not need too many.” Apply this when evaluating feature redesigns: reducing the number of concepts can help, but retaining a necessary distinction is compatible with simplicity. Do not equate merging types with improving the user experience.

**Evidence-before-restriction correction, recorded 2026-11-11** (`segment-0001.chat.org`, turns 39–40): in the Buddy discussion, the user challenged the assumption that there would be too many notes and rejected unsupported caps and mute rules. When the assistant proposed simplifying the activity dial to on/off, the user said to “stick with the three level dial and see how it goes. We can adjust from there.”

Apply this correction when proposing simplifications or restrictions based on predicted usage problems: distinguish an observed problem from an assumption, and do not override the user's selected configurability merely to reduce the number of options. The Buddy example supports learning from use rather than preemptively constraining that design; it does not establish a universal requirement for three levels, configurable controls in every feature, or rejection of evidence-backed limits.

**How to apply:** prefer the simplest design that meets the actual need. Judge both implementation complexity and the user's mental load rather than optimizing a raw count of types or mechanisms. When a plausible performance alternative could materially change the tradeoff, make a bounded measurement before rejecting it; a prototype is evidence for a decision, not a commitment to ship more machinery. This is not a requirement to benchmark every speculative optimization.

Record delegated decisions in the appropriate tracker or maintained documentation, following the applicable workflow—not in memory as changing project state. Recording a decision does not substitute for obtaining required approval.

These preferences do not establish a backwards-compatibility requirement for old APIs or persisted formats. Preserve the distinction between simplifying an implementation and removing a user-facing capability.
