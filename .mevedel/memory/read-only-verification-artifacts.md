---
name: "Read-only verification can rest on independently checked artifacts"
description: "Inability to rerun a writing check is not absence of verification evidence"
type: feedback
---

# Read-only verification can rest on independently checked artifacts

Inability to rerun a check is not absence of verification evidence. Observed 2026-09-28: two separate completion verifiers moved from PARTIAL to PASS without rerunning write-producing checks, after independently inspecting artifacts for correspondence to the final source and to actual execution.

- `/root/verify_7` inspected native logs, structured per-worker results, fixture assertions, and source chronology, corroborating 8,778 unique results rather than the parent's summary (`.scratch/test-suite-performance/20260928-115358/summary.json` and worker result files).
- `/root/verify_9` counted 8,780 unique results as 8,750 passed and 30 skipped — not 8,780 passes — traced `exec-000985` to the terminal Emacs invocation, and confirmed that `.scratch/plan-tty-acceptance.el:19–33` activated `[Show result]` with RET and wrote its success artifact only after the output/history assertions completed (`.scratch/test-suite-performance/20260928-130231/`, eight `*-results.json` files).

**How to apply.** As a read-only verifier, do not downgrade a verdict merely because a check cannot be rerun. Require instead that the artifacts be tied to the final source revision, to real execution records, to the assertion paths they claim to exercise, and to the relevant skips — and report skips and unexercised paths as such rather than counting unique results as passes.

**Scope limit.** This licenses acceptance only of what the artifacts actually cover; it does not convert absence of a rerun into positive evidence for paths no artifact reaches.
