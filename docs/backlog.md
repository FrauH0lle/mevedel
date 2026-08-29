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

- Add a memory-verification slash command or skill that consolidates project
  memories and checks whether they are still accurate; explore whether a
  weekly automated check is useful. See also "/learn" command

- Consider making mevedel's data buffers hidden

- Markdown-ts-mode is now part of Emacs core

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
- **Status check:** the `mevedel` session chooser currently drops missing,
  unreadable, unsupported, and obsolete sidecars without reporting them.
- **Blast radius:** Silent omission looks like data loss and can leave
  unbounded workspace state without giving users enough information to decide
  whether it is disposable.

## Remote workspaces and collaboration

### Revisit the browser viewer

- **Source:** Post-review discussion on 2026-08-18; revisited 2026-08-29
  once daily phone usage made the deferrals observable rather than
  hypothetical. Directive-scoped guest prompts, guest-visible queue
  state, and general file attachment landed then. The overhaul later the
  same day landed skills-as-buttons (the allowlist design this entry
  prescribed), notifications, the directive tab strip, own-queue
  entries with retract, questionnaire dismiss, the QR share frame, and
  one room per session; see
  `docs/adr/0099-project-live-collaboration-from-host-authoritative-state.md`.
- **Original filenames for guest attachments.** Saved names are
  host-generated (`guest-<stamp>-<n>.<ext>`), so a model reading a guest
  log sees no clue what it was called. Carrying the guest's name would
  need sanitizing a guest-supplied string that becomes a write path;
  deferred because the guest can say what the file is in the prompt.
- **Chunked attachment upload.** The whole attachment set caps at
  1.25 MiB decoded so the sealed prompt frame clears the relay's 2 MiB
  read limit alongside a maximum-length prompt. Logs and patches fit;
  if that ever bites, chunking across frames is the upgrade path.
- **Guest /btw.** Deferred with an explicit acceptance bar: build it
  only when a need is observed that is simultaneously private (not
  visible to host and other guests), ephemeral (not part of the durable
  session), immediate (cannot wait behind the running turn), and
  unscoped (not about any directive). Directive-scoped discuss prompts
  cover everything short of that conjunction. If built, it is full-link
  only, read-only tools, per-peer delivery -- host `/btw` carries Bash
  and ApplyPatch and must never be handed to a bearer link as-is.
- **Skill arguments for guest buttons.** Guest skill invocation is
  argument-less; an argument UI is a later iteration if missed in
  practice.

### Collaboration limitations accepted at landing

- **Source:** Post-landing gap review of the browser-relay feature on
  2026-08-18. Recorded so real usage can promote any of them; none blocks
  current use. Two were promoted and closed on 2026-08-29 (relay host
  authentication and directive-scoped guest prompts); the same day's
  overhaul closed two more (one room per Emacs process fell to
  per-session rooms, and remote questionnaire cancel became a plain
  dismiss once the Ask overhaul made cancellation settle only the
  questionnaire rather than abort the request).
- **Compaction drops historical guest badges.** Attribution audit records
  inside compacted spans vanish with the spans; tail-preserved turns keep
  theirs. Cosmetic and historical only.

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

## Review

### Automatic turn advisor

- **Source:** Design discussion on 2026-08-20, prompted by oh-my-pi's
  watchdog/advisor feature (`WATCHDOG.yml`, `advisor.immuneTurns`).
- **What's owed:** After a successful top-level turn, quietly review it with a
  second model and, when something is wrong, inject one hidden note into the
  next request. Trigger from the existing `Stop` hook event only -- once per
  landed turn, not per tool call and not mid-stream. Hand the reviewer the
  transcript delta since the last check and reuse `mevedel-review.el`'s
  spawn/wait/parse-findings machinery over the `reviewer` role in
  `mevedel-agents.el`. Deliver a flagged finding through
  `mevedel-reminders.el` as a `<system-reminder>` alongside the next user
  message. Build both emission guards on day one, because a second model that
  talks constantly is worse than none: hard dedupe (lowercase, collapse
  punctuation and whitespace, drop anything already said this session, drop
  content-free notes such as "looks good"/"lgtm"/"nothing to add", cap at one
  note per pass) and a 3-turn cooldown after a successful injection, during
  which anything further is demoted to a non-interrupting aside.
- **Deliberate exclusions:** No advisor roster -- one reviewer; several named
  advisors with their own models and prompts answer a prompt problem with
  headcount. No per-advisor tool grants -- the reviewer keeps its read-only
  investigation, never edit or bash. No `/advisor configure` UI -- without a
  roster there is nothing to configure. No advisors on spawned sub-agents --
  root session only, or a tree of five agents becomes ten model streams. Add a
  roster when one reviewer prompt is visibly two unrelated jobs stapled
  together; add tool grants when read/grep/glob demonstrably cannot confirm a
  finding.
- **Why deferred:** The routing is trivial and the guards are the real work;
  the accepted cost is one extra model call per turn on the delta, in money and
  latency, which wants deliberate acceptance rather than a quiet default.
- **Status check:** All three pieces exist -- the `Stop` hook event, the
  reviewer role with `/review`'s spawn-and-parse path, and reminder injection.
  Nothing fires a review automatically and no dedupe or cooldown state exists.
- **Blast radius:** Hook lifecycle, sub-agent spawning and capacity, reminder
  injection, and per-turn cost. Ungated notes train the user to ignore
  `<system-reminder>` blocks, at which point the feature is pure spend.

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
