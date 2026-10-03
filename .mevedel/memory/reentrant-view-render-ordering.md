---
name: "Reentrant view render corruption is ordering-dependent"
description: "Settlement nested inside an older incremental render duplicates projection output; regress both nesting orders"
type: project
---

# Reentrant view render corruption is ordering-dependent

Observed 2026-09-16 (session `segment-0002.chat.org`, turn 3; `work://shared/view-render-investigation-20260916.md`, `artifact://executions/execution-95iYzf.log`, `.scratch/view-render-investigation-20260916/replay.el:63`): the deterministic scratch case `diagnosis-settlement-during-live-render` failed with expected `(:tail nil :copies 1)` and actual `(:tail t :copies 2)`. Ordinary chunks `(1 7 23 51)` and the reverse nesting order passed; the loaded native functions reproduced the failure, so it is not an artifact of the scratch harness.

- **The ordering is decisive.** An older incremental render can resume after nested settlement and restore obsolete text and tail state. Regression tests should cover both orders: an incremental update nested inside settlement, and settlement nested inside an older incremental render.
- **The corruption is in the projection, not the transcript.** Authoritative transcript lines 2747–2778 and the live data contain one fix heading/example, the affected view contained four headings and seven examples, and fresh rendering with the loaded native functions was correct. Do not distrust the transcript from a bad screenshot alone.
- **Diagnosis kept the live state intact.** The corrupted live view was preserved and the reproduction carved out under `.scratch/`, without changing product source.
- **Static findings, not reproduced:** shared fontification-buffer reuse, deferred request cleanup acting on replacement state, and same-key transport retries removing newer ownership (`mevedel-view-render.el:3322–3577`, `mevedel-view-fontify.el:106–178`, `mevedel-turn.el:613–632`, `mevedel-presets.el:593–662`, `mevedel-transport.el:248–276`, inherited summary). These paths were not runtime reproductions.
- **Status at capture:** the "Fix Reentrant View Rendering and Stale Callback Ownership" plan (per-view mutation coordination, deferred disclosure/agent-refresh intent, nested Markdown fontification isolation, transport/request callback ownership) was emitted for approval and nothing more — no implementation, suite run, compilation, or deployed-runtime verification is established. Its listed workflow (`npx @emacs-eask/cli clean elc`, focused ERT for `test/test-mevedel-view*.el` plus turn/transport/chat suites, `python3 test/run_tests.py`, warning-free compilation) is proposed validation, not completed work. The original production interrupt sequence and the inherited `/root/async_audit` result remain unknown.
- **Draft preservation still applies.** Any change here must keep the `view-composer-preservation-feedback` regression; the plan includes asserting that a multiline composer draft beginning with a literal `>` retains its text and point.
