---
name: "Prove injected failures and stubs exercise the intended path"
description: "Check injection entry, exact diagnostics, isolated-file behavior, and a discriminating negative control"
type: feedback
---

# Prove injected failures and stubs exercise the intended path

**Rule:** for a stubbed or injected-failure test, establish that the injection was reached and that the observed failure is the intended one. A matching error substring or a green combined run is insufficient.

**Why — observed cases, 2026-10-13:**

- Independent review reproduced a publication-failure false positive: a one-argument stub was called with two arguments. The resulting `wrong-number-of-arguments` diagnostic printed the lambda, including `"simulated publication failure"`, so a substring assertion passed without exercising the intended error. Re-review closed the false positive after the test checked stub entry and the exact warning.
- A permission-queue Git test passed alongside reviewer tests but failed alone: loading the reviewer library overwrote its stub. Making the fixture's dependency explicit allowed the file to pass independently; removing the behavior under test then made the new regression fail.

**When and how to apply:**

- Check an explicit injection-entry marker or equivalent evidence.
- Assert the intended diagnostic, distinguishing it from incidental errors that print stub source.
- Run the affected test file independently to expose load-order and autoload contamination.
- Use a discriminating negative control: removing or bypassing the behavior being tested should make the regression fail.

These are model-inferred lessons supported by reproduced test defects, not a claim that every passing test requires the same instrumentation.
