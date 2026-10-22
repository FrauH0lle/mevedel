---
name: "Preserve controls and conditions when attributing test outcomes"
description: "Equivalent context, delivered test inputs, assertion conditions, and negative controls establish different parts of a coverage claim"
type: feedback
---

# Preserve controls and conditions when attributing test outcomes

**Rule:** attribute failures and coverage claims using equivalent context, identified loaded sources, delivered test inputs, and the actual assertion or skip condition—not test names, aggregate status, or later green runs alone.

**Why — controlled observations recorded 2026-10-13:**

- A whole-prompt absence assertion rejected `artifact://` text from retained journal evidence, not necessarily current feature advertising. The same assertion failed on unchanged baseline sources with source-identity guards. Before blaming a prompt change, locate the matching text's origin and compare an equivalent workspace context.
- An intermittent false-clean Git assertion reproduced in source-guarded immutable archives both before and after a resource change, despite earlier baseline checks passing 32/32 and 25/25. Frozen sources, matching dependency identities, environment controls, and captured failed assertions strengthened attribution; a finite green repetition count did not establish absence of the failure.
- Injecting a 0.6-second delay before a real Bubblewrap probe reproduced four capability-gated skips; without the delay, the tests passed and real probes completed successfully. A probe deadline can yield an inconclusive unavailable result that tests report as a capability skip.

**Additional browser observations recorded 2026-10-19:**

- Before editor logic ran, Firefox 155 exposed zero clipboard files from a supplied one-file `ClipboardEvent` initializer and rejected fabricated touch capture with `Invalid pointer id`; Chromium 153 retained the file and accepted the sequence. Those failures established synthetic-input incompatibility, not broken genuine clipboard or touchscreen behavior.
- With Playwright 1.63, native touch taps in Firefox and Chromium released before the first animation frame. Presence DOM created during pointer-down therefore did not establish a visibly rendered tip. Separating native lifecycle/capture checks from visible movement checks avoided a vacuous rendering assertion.
- Isolated `Page.route` negative controls showed that repaired tests rejected ignored PNG input, blocked HTML paste, and disabled reduced-motion handling. These observations support checking both delivered inputs and consumer-visible effects.

**When and how to apply:**

- For suspected regressions, compare baseline and changed sources under equivalent relevant context and identify the code and dependencies actually loaded.
- For intermittent failures, retain counterexamples even after a green suite. Report what reproduced separately from any proposed mechanism.
- For capability skips, retain the condition and probe output, distinguishing deadline expiry from demonstrated lack of support.
- For cross-browser event tests, first establish what input reached the handler. A constructor accepting an initializer does not establish that its payload survived.
- Distinguish input delivery, event lifecycle, capture, rendered visibility, and physical integration. Use discriminating negative controls to show that the relevant behavior—not merely fixture setup—determines the assertion.

**Evidence limits:**

- The Git captures did not establish identical nanosecond mtimes or prove a racy-index cause. The historical Bubblewrap skips' approximately 0.501-second durations supported a deadline explanation, but their conditions and output were not captured; worker contention remained unproven. Later passing runs did not resolve either causal question.
- The browser observations were version-specific automated probes, not physical touchscreen-dragging or OS-clipboard integration tests. Genuine touch capture can be implicit, so `hasPointerCapture` alone did not prove that the explicit production capture call caused capture.
