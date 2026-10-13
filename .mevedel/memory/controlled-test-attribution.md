---
name: "Preserve controls and conditions when attributing test outcomes"
description: "Green repetitions, whole-prompt matches, and capability skips do not by themselves establish cause or coverage"
type: feedback
---

# Preserve controls and conditions when attributing test outcomes

**Rule:** attribute failures and coverage claims using equivalent context, identified loaded sources, and the actual assertion or skip condition—not test names, aggregate status, or later green runs alone.

**Why — controlled observations recorded 2026-10-13:**

- A whole-prompt absence assertion rejected `artifact://` text from retained journal evidence, not necessarily current feature advertising. The same assertion failed on unchanged baseline sources with source-identity guards. Before blaming a prompt change, locate the matching text's origin and compare an equivalent workspace context.
- An intermittent false-clean Git assertion reproduced in source-guarded immutable archives both before and after a resource change, despite earlier baseline checks passing 32/32 and 25/25. Frozen sources, matching dependency identities, environment controls, and captured failed assertions strengthened attribution; a finite green repetition count did not establish absence of the failure.
- Injecting a 0.6-second delay before a real Bubblewrap probe reproduced four capability-gated skips; without the delay, the tests passed and real probes completed successfully. A probe deadline can yield an inconclusive unavailable result that tests report as a capability skip.

**When and how to apply:**

- For suspected regressions, compare baseline and changed sources under equivalent relevant context and identify the code and dependencies actually loaded.
- For intermittent failures, retain counterexamples even after a green suite. Report what reproduced separately from any proposed mechanism.
- For capability skips, retain the condition and probe output, distinguishing deadline expiry from demonstrated lack of support.

**Evidence limits:** the Git captures did not establish identical nanosecond mtimes or prove a racy-index cause. The historical Bubblewrap skips' approximately 0.501-second durations supported a deadline explanation, but their conditions and output were not captured; worker contention remained unproven. Later passing runs did not resolve either causal question.
