# 10 — Preserve parent-owned subdirective semantics

**What to build:** Keep nested directives as detailed instructions owned by their topmost parent across implementation, change requests, failure, abort, and Rewind. They enrich the parent without creating independent activity histories.

**Blocked by:** 06 — Replace Revise with Request changes and Retry; 09 — Rewind directive implementation attempts

**Status:** resolved

- [x] Acting on a nested directive resolves and submits its topmost parent.
- [x] Parent prompt construction includes every nested directive as a detail, hint, or correction in stable source order.
- [x] Nested directives do not receive independent execution sessions, attempts, activity surfaces, or Rewind actions.
- [x] A successful parent implementation consumes the submitted subdirectives.
- [x] A failed or aborted parent implementation leaves submitted subdirectives in source.
- [x] Newly authored subdirectives after a successful attempt can supply the instructions for Request changes.
- [x] Request changes feedback is optional when at least one unconsumed subdirective supplies new instruction and remains required otherwise.
- [x] The new attempt records which subdirectives it consumed without making their overlays historical truth.
- [x] Rewinding a successful attempt restores the subdirectives that attempt consumed.
- [x] User-authored subdirectives added after an attempt survive Rewind according to the same authored-workspace-state rule as parent directives.
- [x] Parent state, activity, source presentation, and prompt preview remain coherent after success, failure, abort, request changes, and Rewind.
- [x] Tests cover nested selection, prompt inclusion, success consumption, failure/abort retention, post-success corrections, feedback omission, and Rewind restoration.

## Answer

Implemented in `d623590`.
