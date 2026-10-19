---
name: "Do not attribute guesses or unchecked behavior to reference systems"
description: "Check authoritative documentation, source, or observed behavior before using another product to justify a design"
type: feedback
---

# Do not attribute guesses or unchecked behavior to reference systems

## Corrections

**2026-10-09:** after the assistant proposed retaining 10 artifact versions as though that number came from Claude, the user asked, “Did you check how many versions Claude keeps?” The assistant admitted the number was its own guess.

**2026-10-18:** the assistant claimed both Claude Code's AskUserQuestion and Codex's request_user_input block. The user corrected that Codex has at least a non-blocking variant. The journal reports that local source inspection found `request_user_input_async`, contradicting the blanket Codex claim.

## Rule

Before presenting a value or behavior as following a reference product, check authoritative documentation or the relevant implementation. Where the question concerns visible UI behavior, distinguish actual observations from descriptions or guesses.

Label local design proposals and estimates as such. A statement about one API or mode does not establish the behavior of every variant.

**Why:** unsupported attribution makes a local choice appear externally established and can hide alternatives relevant to the user's decision.

**When to apply:** retention counts, size caps, defaults, blocking/asynchronous behavior, and other comparisons used to justify a design. Preserve qualifications about version, mode, availability, and scope.

## Codex source lead

For future claims about Codex Ask behavior, the October 18 journal identifies the local checkout `~/codex/codex-rs`, particularly:

- `core/src/tools/handlers/request_user_input_async.rs`
- `tui/src/bottom_pane/async_questions/`

These are source leads reported by that investigation, not a current verification stamp. Reinspect the relevant source before asserting current availability or semantics.
