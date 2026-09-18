---
name: "Debugging process feedback"
description: "Bisect recurring test/log emitters before implementation changes"
type: feedback
---

Bisect recurring test/log emitters before changing implementation.

**Why:** Jumping to implementation changes can introduce unnecessary code when the real source is a narrower test or cleanup path. Observed examples: a suite reported 16 named landing failures that did not reproduce at all, and 28 failures traced to two harness environment defects rather than product code.

**How to apply:** For repeated suite/log errors, first isolate the responsible command, test file, test group, or buffer lifecycle path; only then make the smallest change justified by that isolation.

- Suspect the harness before the product when failures do not reproduce. Observed causes include stale `.elc` artifacts shadowing edited source, a missing `EMACSLOADPATH` for nested `call-process emacs` children, and an isolated temp root placed inside the checkout.
- Do not trust a filtered output path. A shared helper filter that matches a warning string can swallow a whole ERT assertion diagnostic; add a separately named assertion for the exact message so the evidence stays visible instead of widening or editing the shared filter.
