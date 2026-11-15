---
name: "Establish test conditions before attributing failures or coverage"
description: "Isolate failures without defaulting to harness blame; establish real fixture conditions, delivered inputs, negative controls, and equivalent baselines."
type: feedback
---

# Establish test conditions before attributing failures or coverage

**Rule:** isolate recurring failures before changing implementation, and attribute failures, side effects, and coverage using recorded starting state, equivalent context, identified loaded sources, delivered inputs, and actual assertions—not test names, aggregate status, matching error substrings, or later green runs alone.

These are reusable lessons inferred from observed investigations and the explicitly attributed user correction below. They do not require exhaustive end-to-end testing or identical instrumentation for every change. Current verification requirements remain in maintained project documentation.

## Isolate without defaulting to harness blame

For repeated suite/log errors, isolate the responsible command, test file, group, or buffer lifecycle path before making the smallest justified change.

Earlier investigations found apparent product failures caused by stale compiled artifacts, missing child-process load paths, or an isolated temporary root placed inside the checkout. Treat the harness as a hypothesis, not the default verdict.

November 12 parallel-suite triage illustrates both sides:

- Several timing-sensitive failures passed unchanged in isolation after concurrent full suites had raised system load. Staggering expensive checks can reduce interference.
- The initial “load only” diagnosis was later corrected: a cold-compilation dependency defect and a fixture that removed global transport/hooks were real problems despite isolated green reruns.

Compare unchanged tests in isolation and on an equivalent clean integration baseline. A failure reproducing only in a shared process can reveal contamination; a cold-start-only failure can reveal a real dependency gap. An isolated pass narrows conditions but does not exonerate the branch or prove load was the cause.

For loaded-artifact and fresh compiled first-use investigation, use the separate runtime-provenance memory and maintained development guidance.

## Establish fixture truth before trusting green results or rewriting failures

Observed examples:

- A permission fixture's dynamic binding never established buffer-local `Edits`; effective policy remained `Ask`/best-effort. Its green result did not exercise the intended denial. Covering confinement available and unavailable exposed the gap without weakening production guards.
- A November 12 command-loop characterization omitted `pre-command-hook`, so replacement tracking was never armed. Running both pre-command and post-command hooks reversed its “disproved” conclusion.
- A cold-restart fixture with a partial tool catalog exposed the same missing registration in production.
- A session-opening regression was concealed by discarding pending journal captures in the fixture instead of questioning why routine sessions had acquired a recovery path.

**Apply:** establish the real setup and lifecycle that arm the behavior, including the full command-hook cycle when relevant. Before expanding a catalog, discarding state, or otherwise “fixing” a failing fixture, check whether ordinary production use has the same failure. A failure caused by a new UX path can be design evidence, not fixture inconvenience.

A passing suite is not independent certification of sensitive boundaries. In an earlier permissions investigation, independent review found held approvals bypassing new denies and late approval permitting Eval after cancellation despite 941/941 suite passes. Re-check the changed boundary independently when claiming those corrections are verified.

## Establish starting state before attributing a side effect

**User correction, 2026-11-02:** the assistant interpreted “Attached” as evidence that an edit had attached a whiteboard. The user corrected: “The board was attached from the very beginning.”

A later controlled test began with a different, unattached board. Its host-side flag stayed false after a plain read and became true after an acknowledged guest update. That supported a transition in that run, not the original inference.

Record relevant pre-action and post-action state using a signal tied to the transition. A visible end state alone does not show what caused it. Host-side attachment does not establish browser presentation, and one tested board does not establish a universal remote-edit guarantee.

## Preserve equivalent context and intermittent counterexamples

Observed 2026-10-13:

- A prompt absence assertion matched retained journal evidence containing `artifact://`, not necessarily current feature advertising; it also failed on unchanged baseline sources.
- An intermittent false-clean Git assertion reproduced in source-guarded immutable archives before and after a resource change despite earlier green baseline runs.
- Injecting delay before a real Bubblewrap capability probe reproduced deadline-driven skips; without delay, real probes succeeded.

Compare equivalent workspace context, sources, and dependencies. Retain intermittent counterexamples after green reruns and separate reproduction from mechanism. For capability skips, retain the actual condition and probe output: timeout is not demonstrated lack of support.

The Git evidence did not establish identical nanosecond mtimes or prove a racy-index cause. Historical roughly half-second capability skips supported a deadline hypothesis but lacked captured conditions/output; worker contention remained unproven.

## Prove injected failures and stubs reach the intended path

Observed 2026-10-13:

- A stub with the wrong arity produced `wrong-number-of-arguments`; the diagnostic printed its lambda containing the expected “simulated publication failure” substring. The assertion passed without exercising the intended error.
- A Git test passed with reviewer tests but failed alone because loading the reviewer library overwrote its stub. An explicit fixture dependency and a negative control exposed the distinction.

For injected or stubbed failures:

- Check injection entry explicitly.
- Assert the intended diagnostic, not merely a substring that incidental errors can print.
- Run affected files independently to expose load-order/autoload contamination, and together where shared-state contamination is relevant.
- Use a discriminating negative control: removing or bypassing the tested behavior should make the regression fail.

Do not hide evidence in output filtering. A shared warning filter once swallowed an entire ERT assertion diagnostic; a separately named exact-message assertion kept the evidence visible without widening the filter.

## Compare the representation the consumer uses

Observed 2026-09-21: parsed gptel tool schemas failed Lisp `equal` because property keys were fresh uninterned symbols, although serialized provider JSON was unchanged.

For a claim of serialized schema/form stability, compare serialized payloads rather than assuming parsed object identity is meaningful. This does not make structural comparisons inappropriate when structure or identity is the actual contract.

## Establish delivered browser input and visible effects

Version-specific observations recorded 2026-10-19:

- Firefox 155 exposed zero clipboard files from a supplied one-file `ClipboardEvent` initializer and rejected fabricated touch capture with `Invalid pointer id`; Chromium 153 retained the file and accepted the sequence. Those results established synthetic-input incompatibility, not broken genuine clipboard/touchscreen behavior.
- With Playwright 1.63, native taps released before the first animation frame. Presence DOM created on pointer-down did not establish a visibly rendered tip.
- Isolated `Page.route` negative controls rejected ignored PNG input, blocked HTML paste, and disabled reduced-motion behavior.

First establish what input reached the handler. Constructor acceptance does not establish payload delivery. Separate event lifecycle, capture, rendered visibility, and physical integration. A genuine touch capture may be implicit, so `hasPointerCapture` alone does not prove the explicit production call caused it.

These probes did not establish physical touchscreen-dragging or OS-clipboard integration.

## Reproduce selection-dependent context

Observed 2026-10-29 with real polymode on Emacs 31.1: `find-buffer-visiting` returned a file-named indirect child selected through `switch-to-buffer`, even inside `with-current-buffer` on the base. Selecting the base reversed the result. A no-advice fixture and independent rerun established this without a disk change or second child, despite the documented base-buffer-only guarantee.

When behavior can depend on selected buffers, windows, or recent-use ordering, reproduce those selection transitions. Changing the current buffer alone is not equivalent.

This established an isolated mechanism, not the precise historical sequence after the reported `git switch`.
