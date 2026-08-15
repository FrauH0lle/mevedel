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
  weekly automated check is useful. See also "/learn" command

- Warnings in Emacs are quite intrusive. Consider making warnings in mevedel target
  the messages buffer ([mevedel] Something happened, can be colored, see corfu)
  and display the warning also in the view buffer (but not permanent via the data
  buffer).

- Consider making mevedel's data buffers hidden

- Find a better folder for the tool description markdown files
- Ensure all tools have the examples and their descriptions in markdown files

- Report the batch file-notify defect upstream to Emacs: in a
  noninteractive Emacs (30.2), the first filesystem notification on any
  watched directory permanently stops process sentinel delivery for the
  whole session. Twenty-line repro: `emacs -Q --batch`, add a
  `file-notify-add-watch` on a temp directory, `make-directory` under it,
  then spawn any process with a sentinel -- the sentinel never runs (the
  notify callback never runs either; batch has no command loop to deliver
  the event). mevedel works around it by refusing to install watchers when
  `noninteractive` (`mevedel-skills--ensure-watcher`,
  `mevedel-skills--filenotify-supported-p`); revisit if upstream fixes it.

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

## Remote workspaces and collaboration

### Complete the accepted remote-workspace roadmap

- **Source:** Remote-workspace review and use-case discussion on 2026-08-12;
  the accepted product roadmap is
  `.scratch/tramp-support/FUTURE-SCOPE-PACKAGES.md`, and the focused first
  browser-viewer spec is `.scratch/live-collaboration/PRD.md`.
- **What's owed:** Make these user journeys work through focused feature PRDs:
  ordinary SSH and already-running container workspaces with minimal setup;
  target-side session handoff from Desktop to Laptop, including when Desktop
  itself later becomes the SSH target; discovery, read-only inspection,
  cooperative control transfer, rejection, timeout, and expired takeover;
  identity across equivalent connection routes and persistent container
  replacement; recovery after client or network loss; target-native worktrees
  and forks; and link-based live collaboration for guests without project
  access. Keep mounted remote storage as documented local operation, add
  bootstrap only when target provisioning demonstrates a need, and treat
  Remote Mevedel and Managed Workspace as separate future products.
- **Why deferred:** The execution-target and Portable Workspace implementation
  is still landing. Each remaining capability has a distinct authority,
  security, dependency, and acceptance surface and should not expand that PRD.
- **Status check:** The execution-target PRD covers the core remote workspace
  and target-side portability foundation. The first live-collaboration PRD
  specifies a native view-only browser guest; relay, client-side encryption,
  steering, Remote Mevedel, and managed provisioning remain later slices.
- **Blast radius:** Execution-target identity, readiness and bootstrap,
  durability and leases, session discovery and control transfer, worktree
  workflows, live session projection, browser rendering, transport, and
  bearer-capability security.

## Skills

### Add required skill attachments inside skill bodies

- **Source:** `mevedel-skills-core.el`, `mevedel-skills-invoke.el`,
  `mevedel-skills-plan.el`; skill-composition discussion on 2026-08-10.
- **What's owed:** Add an authored `!$skill` marker that guarantees the named
  skill is prepared and attached as instruction context rather than relying on
  the model to call `Skill`. Reuse the existing `[skill:NAME -- attached]`
  placeholder and hidden attachment reminder. A user-origin parent may attach
  a user-invocable child even when the child has
  `disable-model-invocation: true`; a model-origin parent may not launder that
  authority, and a required model-disabled dependency therefore makes the
  parent effectively model-disabled. Unsatisfied, disabled, unauthorized, or
  cyclic dependencies fail atomically before dispatch. Resolve dependencies
  eagerly when the session skill set refreshes, then reload bound bodies at
  invocation. Resolve qualified names exactly; resolve unqualified names from
  the parent's skill root or plugin first, then only from a unique global raw
  name. Dormant path-scoped dependencies attach one-shot without becoming
  active. Keep inline `!$skill` argument-free; allow the full-line form
  `!$skill -- RAW ARGUMENTS`, where everything after `--` through end of line
  is the child argument string. Deduplicate identical source/argument pairs
  and reject conflicting arguments for one source.
- **Why deferred:** The feature was intentionally paused before settling the
  trust grammar: escaping and Markdown/quote handling, whether only literal
  author-written markers may activate recursively, and how generated text,
  hooks, permissions, audit data, and UI inspection interact with attachment
  dependencies still need design.
- **Status check:** Skill bodies and generated output are currently never
  rescanned as user skill mentions. A wrapper can ask the model to invoke
  model-visible children, as `grill-with-docs` does, but cannot reach a child
  protected by `disable-model-invocation: true`.
- **Blast radius:** Skill discovery and hot reload, effective roster
  visibility, invocation preparation, attachment staging, source binding,
  invocation records and replay, skill inspection UI, permissions/hooks, and
  parser/security tests. A loose origin or generated-text rule could let a
  model or untrusted input bypass a user-only skill restriction.

## Execution

### Run remote Bash over tramp-direct-async-process

- **Source:** `mevedel-execution.el` (`mevedel-execution--start-process`
  spawns with `make-process` `:file-handler t`); test-suite performance
  round, 2026-08-15.
- **What's owed:** Evaluate `tramp-direct-async-process` for the Bash
  tool. The spawn goes through TRAMP's handler `make-process`, which is
  exactly the path direct-async rewrites: the execution would run over
  its own `ssh` invocation instead of the shared control connection, so
  a live Bash stops competing with durable work for the channel and
  stops being a reentrancy window at all -- a bigger structural win than
  any single round trip removed so far.
- **Why deferred:** A real change to the execution layer, not a variable
  flip. Remote process-group tracking is already special-cased
  (`record-group-id` is captured locally for local processes; remote
  identity arrives through the filter group marker), the wrapping in
  `mevedel-execution--remote-command` has to survive `ssh host <cmd>`
  delivery, and it needs coverage in the real-SSH acceptance file before
  it can be trusted.
- **Status check:** Not started; needs proper scoping first.
- **Blast radius:** Today every live remote execution shares the control
  connection, so a long-running Bash serializes against saves, leases,
  and publication, and every in-flight command is a window in which a
  foreign package's remote operation is refused as reentrant.

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
