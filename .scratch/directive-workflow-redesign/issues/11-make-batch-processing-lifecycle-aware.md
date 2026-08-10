# 11 — Make batch processing lifecycle-aware

**What to build:** Make batch processing honor the redesigned directive lifecycle without guessing revision intent. It performs only initial implementation work, uses the same eligibility rules as individual actions, and stops as soon as repository state becomes uncertain.

**Blocked by:** 06 — Replace Revise with Request changes and Retry; 08 — Preserve directives when their source disappears; 10 — Preserve parent-owned subdirective semantics

**Status:** resolved

- [x] Batch processing submits Ready directives with Implement in deterministic source order.
- [x] A Discussed directive without an implementation attempt is submitted with Implement this and its complete local discussion.
- [x] Any directive with a prior implementation attempt is skipped regardless of whether its latest outcome is success, failure, or abort.
- [x] Batch processing never infers Request changes, Retry, or a clean restart.
- [x] Detached and Source missing directives use the same action-eligibility and prompt-context validation as individual submission.
- [x] An ineligible directive is left untouched and reported clearly rather than submitted with guessed context.
- [x] Nested directives remain part of their parent and are never queued as independent batch items.
- [x] The batch stops after the first failed or aborted attempt and leaves all remaining directives untouched.
- [x] Terminal request cleanup finishes before the next directive starts, preserving the existing sequential safety property.
- [x] Batch progress and completion leave directive activity, compact session events, source labels, and available actions consistent with individual submission.
- [x] Tests cover mixed Ready, Discussed, attempted, nested, Detached, Source missing, ineligible, successful, failed, and aborted directives.
- [x] Maintained user and architecture documentation describes the final action model, activity location, patch semantics, anchor states, session binding, and Rewind behavior without retaining obsolete Revise guidance.

## Answer

Implemented in `d59709b`.
