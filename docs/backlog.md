# Project backlog

Canonical home for project notes, todos, feature ideas, fixes, and
explicitly deferred work. Read this before planning work in any listed
area.

Use the inbox for ideas that have not been investigated yet. Promote an
item to a detailed entry when its scope and current status are understood.
Remove items when they are implemented, obsolete, or no longer valuable.

/goal Resolve every ticket in `.scratch/plan-and-goal-redesign/issues/` on the
current branch. Use $implement once for the complete ticket set, treating
individual tickets as milestones rather than separate skill invocations. Process
tickets in dependency order. Follow `AGENTS.md`, including the no-backwards-
compatibility policy.

For each ticket, satisfy its acceptance criteria, make the smallest root-cause
change, update affected callers/tests/docs, run focused verification, and commit
the completed ticket. Do not run the full review suite between ordinary tickets.

After all tickets are implemented, perform one cumulative review cycle over the
complete starting-commit-to-HEAD diff:

1. Complete $implement ’s required code review and resolve its actionable
   findings.
2. Run $ponytail:ponytail-review , apply justified simplifications, then run
   $thermo-nuclear-code-quality-review on the resulting leaner implementation.
3. Consolidate duplicate findings and fix all correctness/spec failures and
   high-confidence maintainability blockers in one pass.
4. Rerun only a reviewer whose unresolved findings remain or whose review domain
was materially affected by the fixes. Recheck those findings specifically; do
not restart all four reviews automatically.
5. Do not iterate on subjective or non-blocking preferences indefinitely. Record
   any rejected finding with a concise rationale.

After review fixes, clean stale bytecode, compile without warnings, and run the
full test suite once.

Complete the Goal only when every ticket’s acceptance criteria are satisfied,
all required commits exist, final compilation and tests pass, and no actionable
review blocker remains. If progress is genuinely blocked, stop with the affected
ticket, attempted approaches, concrete evidence, and the exact input or external
change needed.

This changes reviews from an inner loop to an outer quality gate: roughly four
reviews total instead of four multiplied by ticket count. Running Ponytail
before the thermo-nuclear audit also prevents the expensive structural reviewer
from analyzing code that will be deleted.

The important wording is “actionable blocker,” not “all reviewers green.” The
thermo-nuclear review is intentionally aspirational and can keep proposing
improvements forever; literal greenness is not a stable completion condition.

For especially risky tickets—new architectural boundaries, cross-module state
changes, or a file crossing 1,000 lines—add an exceptional mid-stream thermo
review. Otherwise, defer it. This preserves the official Goal pattern:
measurable outcome, verification surface, constraints, boundaries, iteration
policy, and blocked stop condition. OpenAI’s Goal-mode guide
(https://developers.openai.com/cookbook/examples/codex/using_goals_in_codex#how-to-write-a-goal)
recommends those elements while leaving Codex room to choose the next action.

## Inbox

- ApplyPatch follow-ups (deferred while landing the tracer bullet):
  - Teach the repair pipeline numeric ranges: `:minimum`/`:maximum` in the
    tool arg DSL plus a clamp-range repair rule, so range clamping (Bash
    `yield_time_ms`, WaitAgent `timeout_ms`) becomes model-visible and
    telemetry'd instead of silent handler policy.
  - Live streaming patch preview: render the review incrementally while the
    model is still generating, like Codex's `StreamingPatchParser` +
    `PatchApplyUpdated` events.
  - Cumulative turn/session diff view aggregating all applied patches,
    like Codex's `SharedTurnDiffTracker`; could reuse the ediff glue.

- Add a memory-verification slash command or skill that consolidates project
  memories and checks whether they are still accurate; explore whether a
  weekly automated check is useful.

- Warnings in Emacs are quite intrusive. Consider making warnings in mevedel target
  the messages buffer ([mevedel] Something happened, can be colored, see corfu)
  and display the warning also in the view buffer (but not permanent via the data
  buffer).

- Consider making mevedel's data buffers hidden

- Find a better folder for the tool description markdown files
- Ensure all tools have the examples and their descriptions in markdown files

## Entry format

Each entry records its source, owed change, reason for deferral, current
status, and blast radius. Keep entries terse and remove them when they
become implemented, obsolete, or unjustified.

## Sessions

### Surface incompatible persisted sessions

- **Source:** `mevedel-session-persistence.el`; session schema changes during
  `v0.5.0`.
- **What's owed:** Detect session directories that the current picker cannot
  restore and tell the user how many were omitted, why, their age and disk
  usage, and where they live. Decide whether to offer inspection, archival, or
  deletion; keep incompatible sessions unselectable until recovery semantics
  are deliberately defined.
- **Why deferred:** Resume remains safe by accepting only the current sidecar
  shape, and ordinary age-based cleanup can reclaim stale data independently.
- **Status check:** `mevedel-resume` currently drops missing, unreadable,
  unsupported, and obsolete sidecars without reporting them.
- **Blast radius:** Silent omission looks like data loss and can leave
  unbounded workspace state without giving users enough information to decide
  whether it is disposable.

## Agents

### Add task-focused summary context for agent forks

- **Source:** `mevedel-agent-control.el`, `mevedel-plan-handoff.el`; agent
  context-fork discussion on 2026-07-24.
- **What's owed:** If real usage shows self-contained Agent messages are
  insufficient, add an explicit `fork_turns="summary"` mode. It should cost one
  disclosed additional model request, use the child task to produce focused
  background, never treat prior requests as child assignments, inject the
  result as labelled context rather than replayed user turns, and leave the
  parent transcript unchanged.
- **Why deferred:** Ordinary delegation should first rely on a complete
  `message`. Generated context adds latency, cost, failure handling, and
  asynchronous preparation to Agent spawning.
- **Status check:** Plan handoff already creates compact implementation
  summaries, but its continuation-oriented prompt preserves unresolved next
  steps and its preparation runs after a completed Plan turn.
- **Blast radius:** Reusing Plan handoff compaction unchanged could revive
  parent obligations in the child and cannot safely mutate or compact a parent
  request that is still active. Reuse lower-level summarization machinery only
  behind a non-mutating, task-focused agent handoff.

## Request lifecycle

### Prevent system sleep during active requests

- **Source:** `mevedel-structs.el` (`mevedel-request-begin`,
  `mevedel-request-push-canceller`); `mevedel-agent-runtime.el`
- **What's owed:** While a top-level or sub-agent request is active, hold an OS
  sleep inhibitor and release it on every completion, failure, abort, and stale
  request replacement path.  On Linux, start `systemd-inhibit --what=sleep` as
  an asynchronous child process and register its teardown as a request
  canceller.  Keep screen blanking and locking unaffected; add other platform
  mechanisms only when they are needed.
- **Why deferred:** Emacs 30 has no portable system-sleep inhibitor, and a
  leaked platform inhibitor could prevent intended suspend indefinitely.
- **Status check:** Request ownership and teardown are already centralized, so
  each request can own its inhibitor without a new session-level reference
  counter.  No inhibitor is currently acquired.
- **Blast radius:** Without this, automatic suspend interrupts long-running
  model, tool, and agent work.  Incorrect cleanup can drain a laptop battery or
  block explicit suspend after mevedel becomes idle.

## Tools

### Bedrock backend support for deferred tool loading

- **Source:** `mevedel-tools.el` (`mevedel-tools--handle-deferred-inject`)
- **What's owed:** Read and replace Bedrock tools under
  `(:toolConfig :tools)` when deferred tools are injected or expired.
- **Why deferred:** Bedrock uses a different payload nesting from the other
  supported gptel backends and has not been exercised here.
- **Status check:** The handler explicitly supports only the common `:tools`
  path.
- **Blast radius:** Bedrock sessions cannot use deferred-tool loading
  correctly.

### Split the side-conversation test suite into per-function deftests

- **Source:** `test/test-mevedel-side-conversation.el`
- **What's owed:** The whole module is covered by one ~1,800-line
  `mevedel-deftest mevedel-view-send/btw` named after a
  `mevedel-view-composer` function.  Split into per-entry-point deftests
  (`mevedel-side-conversation-open`, `-send`, `--snapshot`,
  `--copy-context-sources`) with the module's own names.
- **Why deferred:** Pure test-file reorganization; every `:doc` case maps
  to an ADR 0093 clause and all 26 cases pass, so the churn carries risk
  without behavior gain right now.
- **Status check:** Still one deftest covering the module.
- **Blast radius:** Failures report under a misleading test name; pure
  helpers are only exercised through the full view integration path.
