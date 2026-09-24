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
- Compare serialized payloads, not parsed structures, when asserting that a generated schema or form is unchanged. Observed 2026-09-21: the first post-change regression failed on Lisp `equal` of parsed gptel tool schemas because gptel creates fresh uninterned property keys with `make-symbol`; asserting the `gptel--json-encode` output instead tested actual provider-schema stability. Treat a structural-equality failure there as a test artifact until the serialized form is compared.
- Confirm that a fixture establishes the condition it names before trusting its green result. Observed 2026-09-18: a directory-write fixture's dynamic binding never established the `Edits` permission mode, so the effective policy remained `Ask`/best-effort and the expected denial was never exercised; the missing denial reappeared only when confinement was forced unavailable. Establishing buffer-local `Edits` and covering both availability branches repaired the fixture evidence without weakening production guards.
- A fully green suite is not independent certification when the change touches permissions or request-lifecycle behavior. An independent verifier returned `VERDICT: FAIL` on a snapshot with 941/941 suite passes, because held hook approvals in unchanged Ask/Edits bypassed new denies and cancellation at `permission-wait` still permitted late-approved live Eval. Treat corrections as unverified until something independent re-checks the changed boundary.
