---
name: "Establish test conditions before claiming coverage or causation"
description: "Check starting state, delivered inputs, injection entry, exact assertions, and discriminating controls before attributing outcomes."
type: feedback
---

# Establish test conditions before claiming coverage or causation

**Rule:** attribute failures, side effects, and coverage claims using recorded starting state, equivalent context, identified loaded sources, delivered test inputs, and actual assertion or skip conditions—not test names, aggregate status, visible end state, matching error substrings, or later green runs alone.

These are reusable lessons inferred from the observations and explicitly attributed user correction below. They do not require identical instrumentation or exhaustive end-to-end testing for every change.

## Establish starting state before attributing a side effect

**User correction, 2026-11-02:** the assistant interpreted the user's “Attached” response as evidence that an edit had attached a whiteboard to a session. The user corrected: “The board was attached from the very beginning.”

A subsequent live test used a different, initially unattached board. Its host-side attachment flag remained false after a plain read and became true after an acknowledged guest update. This supported the side effect in that controlled run; it did not make the original inference valid.

**Apply:** record relevant pre-action and post-action state, preferably with a signal tied to the claimed transition. Confirmation of a visible state does not establish that the preceding action caused it. A host-side signal can establish attachment without establishing browser presentation, and one tested board does not establish a universal guarantee about remote edits.

## Preserve equivalent context and intermittent counterexamples

Controlled observations recorded 2026-10-13:

- A whole-prompt absence assertion rejected `artifact://` text from retained journal evidence, not necessarily current feature advertising. The same assertion failed on unchanged baseline sources with source-identity guards. Locate the matching text's origin and compare equivalent workspace context before blaming a prompt change.
- An intermittent false-clean Git assertion reproduced in source-guarded immutable archives both before and after a resource change, despite earlier baseline checks passing 32/32 and 25/25. Frozen sources, matching dependency identities, environment controls, and captured failed assertions strengthened attribution; a finite green repetition count did not establish absence of the failure.
- Injecting a 0.6-second delay before a real Bubblewrap probe reproduced four capability-gated skips; without the delay, the tests passed and real probes completed successfully. A probe deadline can yield an inconclusive unavailable result that tests report as a capability skip.

**Apply:**

- Compare baseline and changed sources under equivalent relevant context; identify the code and dependencies actually loaded.
- Retain intermittent counterexamples after a green suite. Separate what reproduced from a proposed mechanism.
- For capability skips, retain the condition and probe output. Deadline expiry is not demonstrated lack of support.

**Limits:** the Git captures did not establish identical nanosecond mtimes or prove a racy-index cause. Historical Bubblewrap skips lasting approximately 0.501 seconds supported a deadline explanation, but their conditions and output were not captured; worker contention remained unproven. Later passing runs did not resolve either causal question.

## Prove injected failures and stubs reach the intended path

Observed cases, 2026-10-13:

- Independent review reproduced a publication-failure false positive: a one-argument stub was called with two arguments. The resulting `wrong-number-of-arguments` diagnostic printed the lambda, including `"simulated publication failure"`, so a substring assertion passed without exercising the intended error. Re-review closed the false positive after the test checked stub entry and the exact warning.
- A permission-queue Git test passed alongside reviewer tests but failed alone: loading the reviewer library overwrote its stub. Making the fixture's dependency explicit allowed the file to pass independently; removing the behavior under test then made the new regression fail.

**Apply to stubbed and injected-failure tests:**

- Check an explicit injection-entry marker or equivalent evidence.
- Assert the intended diagnostic, distinguishing it from incidental errors that print stub source.
- Run the affected test file independently to expose load-order and autoload contamination.
- Use a discriminating negative control: removing or bypassing the behavior being tested should make the regression fail.

A matching error substring or a green combined run is insufficient proof that the intended failure path was exercised.

## Establish delivered browser input and consumer-visible effects

Version-specific automated observations recorded 2026-10-19:

- Before editor logic ran, Firefox 155 exposed zero clipboard files from a supplied one-file `ClipboardEvent` initializer and rejected fabricated touch capture with `Invalid pointer id`; Chromium 153 retained the file and accepted the sequence. Those failures established synthetic-input incompatibility, not broken genuine clipboard or touchscreen behavior.
- With Playwright 1.63, native touch taps in Firefox and Chromium released before the first animation frame. Presence DOM created during pointer-down therefore did not establish a visibly rendered tip. Separating native lifecycle/capture checks from visible movement checks avoided a vacuous rendering assertion.
- Isolated `Page.route` negative controls showed that repaired tests rejected ignored PNG input, blocked HTML paste, and disabled reduced-motion handling.

**Apply:** first establish what input reached the handler. A constructor accepting an initializer does not establish that its payload survived. Distinguish input delivery, event lifecycle, capture, rendered visibility, and physical integration. Use negative controls to show that the relevant behavior—not merely fixture setup—determines the assertion.

These probes did not establish physical touchscreen-dragging or OS-clipboard integration. Genuine touch capture can be implicit, so `hasPointerCapture` alone did not prove that the explicit production capture call caused it.

## Reproduce selection-dependent context, not just current-buffer bindings

Observed 2026-10-29 with real polymode on Emacs 31.1: `find-buffer-visiting` returned a file-named indirect child selected through `switch-to-buffer`, even when lookup ran inside `with-current-buffer` on the base buffer. Selecting the base reversed the result. A no-advice fixture and independent rerun established this without a disk change or second child, despite the function's documented base-buffer-only guarantee.

**Apply:** when behavior may depend on selected buffers, windows, or recent-use ordering, reproduce the relevant selection transitions. Changing the current buffer alone is not equivalent.

This established an isolated mechanism, not the precise historical sequence after the reported `git switch`.
