---
name: "Verify consumer-visible state, not intermediate signals"
description: "Assert rendered/consumed state, not internal flags, audit rows, or same-segment tests"
type: feedback
---

# Verify consumer-visible state, not intermediate signals

When a change alters what a user or another consumer observes — a rendered view or status zone, tool presentation, results read after compaction, retained evidence — assert the state at the boundary that consumer uses. Internal success flags, durable audit records, and same-segment tests are intermediate signals and do not establish the visible outcome.

**Why:** Observed 2026-09-28 in session 2026-09-27T19-53. Four verification probes each passed an intermediate check while the consumer-visible state differed:

- Hiding observation rows: the completion verifier's probe retained `"INITIAL\nFINAL"` with exit 7, but guest current records were empty and guest archived history showed only `"INITIAL"` with a running header. Model inference from that observed result: such a change needs cross-segment result-access checks for every consuming projection; same-segment tests do not establish that output stays accessible after compaction (`segment-0010`, `UpdateGoal` completion-verification result).
- Async presentation: the `mevedel-view-stream-handle-execution-event` probe observed `row-finished=t visible-breadcrumbs=0 durable-breadcrumbs=1`; only `mevedel-view--full-rerender` made the breadcrumb visible, so a durable audit record plus an updated original row did not prove the notification appeared. Model inference: assert the rendered view immediately after terminal handling, without forcing a redraw (`segment-0011`, final `UpdateGoal` verification).
- Control operations: accepted `WriteStdin` input `"answer\n"` followed by process exit 2 produced `:control-succeeded-p t` while rendering showed `× WriteStdin: sent input` and guest status `"failed"`. Model inference: testing only `:control-succeeded-p` misses status conflation downstream; verify through the full renderer pipeline (`segment-0011`).
- Retained-evidence fallback: with the original execution row and output artifact unavailable, a serialized successful completion carrying `:execution-output ""` made `mevedel-view-audit--evidence` report "Original execution row and retained output are unavailable"; the corresponding nonempty-output probe passed. Model inference: fallback coverage needs both present-but-empty and absent evidence — nonempty fixtures alone miss this boundary (`segment-0015`; `mevedel-view-audit.el:709–712`, `test/test-mevedel-view-render-audits.el:638–674`).

**How to apply:**

- For view, status, tool-presentation, compaction-boundary, or evidence-retention changes, assert the consumed result (rendered buffer text, the post-compaction read, the evidence-presence check) through the same entry point the consumer uses, not an internal predicate, serialized record, or audit row.
- Treat a green same-segment test as unproven for behavior a different segment, projection, or renderer stage consumes; add a case that crosses that boundary.
- Include a present-but-empty fixture alongside the absent and nonempty ones whenever fallback or retained-evidence behavior is under test.
- These were verification probes, not user corrections: keep the rule general, and re-check the specific symbol and line references against the current source before relying on them.
