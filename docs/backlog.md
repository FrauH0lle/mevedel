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

- Find a better folder for the tool description markdown files
- Ensure all tools have the examples and their descriptions in markdown files

- investigate and test address to resources, preferably in mevedel itself
  - are the hashes necessary? Don't they prohibit the model from calling the correct address?
  - The local:// scratchpad should be clearly promoted in the instructions
  -
- Allow activation and deactivation of skills per project
- Allow activation and deactivation of plugins per project
- In Ask mode, ApplyPatch should expand the first one or two hunks
- Residual watch after the 2026-08-23 interaction rebuild fix (cursor jumps
  at tick rate + garbled ApplyPatch feedback both traced to the 5s
  control-transfer poll rebuilding the interaction zone through an
  intermediate empty render, with new overlay objects and stale preview
  body snapshots): non-composer point and window positions are still
  preserved as raw integers in `mevedel-view-zone--restore-view-state` and
  `mevedel-view--call-preserving-window-state`. If cursor drift reappears
  during heavy history-live streaming with point on interaction text,
  convert those captures to markers.

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

- **Source:** Post-review discussion on 2026-08-18; collects the viewer
  capabilities worth a deliberate second pass now that daily phone usage
  is real.
- **Skills and slash commands for guests.** Guest input is deliberately
  skill-inert today (`$skill` tokens stay literal; slash commands are
  never parsed). If guests should invoke skills or a curated slash
  subset, that wants an explicit design: which skills, whose authority,
  how the viewer discovers the roster — not a lifted ride-along.
- **General file upload, not only photos.** The attach button accepts
  `image/*` and re-encodes through canvas. Logs, patches, CSVs, and
  PDFs would ride the same @file-mention pipeline but need a type
  allowlist, a size budget without canvas downscaling, and a decision on
  which types mention as text versus media.
- Candidates observed in use, for the same pass: guest-visible queue
  state (what is pending beyond the flash notice), directive-scoped
  guest prompts (send within the active filter), and a remote
  questionnaire cancel with a confirm step.

### Authenticate room creation on the collaboration relay

- **Source:** Post-landing gap review of the browser-relay feature on
  2026-08-18.
- **What's owed:** Anyone who discovers the relay's `wss://…/r/<id>?role=host`
  endpoint can open rooms and hold idle connections. Content is never at
  risk -- a stranger's room carries only their own ciphertext, and
  max-room-age bounds the lifetime -- but it is an idle-connection/DoS
  surface on the operator's server. A `-host-token` relay flag checked at
  the host upgrade, with a matching defcustom sent as a header or query
  parameter, closes it in a few lines. Guests stay tokenless: their
  authority is the bearer link.
- **Why deferred:** Single-operator deployment behind an unpublicized
  domain; no data exposure either way.

### Collaboration limitations accepted at landing

- **Source:** Post-landing gap review of the browser-relay feature on
  2026-08-18. Recorded so real usage can promote any of them; none blocks
  current use.
- **Guest prompts always land in main chat.** The directive filter is
  view-only; directive-scoped guest input was deferred by the PRD. If
  wanted: carry the active filter's directive id in the prompt frame and
  enqueue with the matching scope.
- **One room per Emacs process.** Two sessions cannot be shared at once.
  The frame grammar tolerates a later `session-list`/`switch-session`
  extension additively.
- **Compaction drops historical guest badges.** Attribution audit records
  inside compacted spans vanish with the spans; tail-preserved turns keep
  theirs. Cosmetic and historical only.
- **No remote questionnaire cancel.** Ask cancellation aborts the whole
  request, too heavy for a phone button; guests can only submit. If
  wanted, a cancel option would need its own confirm step.

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

### Programmatic tool calling

- **Source:** `elij/macher-agent` at commit
  [`f6ed4c3`](https://github.com/elij/macher-agent/tree/f6ed4c35296780f61b49316af95bea0c0f50f8c1),
  especially `macher-agent-sandbox.el`, `macher-agent-tools.el`, and its
  skill-scoped `ptc-primitives` metadata.
- **Problem statement:** Data-dependent tool chains currently require a model
  continuation between stages. A model may issue known independent calls in
  parallel, but a workflow such as Glob -> inspect returned paths -> Read each
  match -> aggregate the results pays for another inference at every decision
  point. That adds latency and tokens and gives the model repeated chances to
  drift from deterministic orchestration.
- **Implemented:** Programmatic tool calling is one model tool, ToolScript,
  whose input is an Emacs Lisp orchestration script evaluated by an isolated,
  yielding interpreter. Ordinary control flow and pure data transformations
  run inside the interpreter. Calling an active primitive yields a structured
  tool request; the driver suspends the script, executes that request through
  `mevedel-pipeline-run-tool-outcome`, and resumes it with the canonical result. Async
  tools therefore look synchronous to the script while every call still uses
  mevedel's validation, hooks, permission, resource, snapshot, cancellation,
  and telemetry path. Provider-only result persistence applies to the
  ToolScript envelope rather than to each nested call.
- **Skill integration:** Implemented `ptc-primitives` SKILL.md frontmatter for
  request-owning command skills. It narrows which canonical tools may become
  Lisp primitives for the owned request; it grants no authority, intersects
  the request's effective tool set, and never bypasses per-call permission.
  Instruction attachments and model-invoked skills cannot expand the parent
  request's execution surface. Agent and coordination tools are not ToolScript
  primitives.

  ```yaml
  ---
  name: orchestrator
  description: Coordinate a data-dependent repository investigation.
  ptc-primitives:
    - Glob
    - Grep
    - Read
  ---
  ```

- **Interface sketch:** The model invokes ToolScript with a `script` string.
  The request-time prompt describes only the selected primitives, their
  argument contracts, the supported Lisp subset, and the returned value
  contract. The result should be the script's pure final value; nested calls
  remain inspectable through one owned audit/render record rather than
  pretending to be provider-origin tool calls.
- **Acceptance follow-up:** Measure turns whose continuations only perform
  deterministic tool orchestration; choose one real data-dependent workflow
  as the acceptance case. Specify the safe Lisp subset, macro expansion,
  limits on steps/results/nesting, async suspension inside non-local exits,
  cancellation, permission-prompt reentrancy, synthetic nested-call identity,
  transcript evidence, render-data ownership, failure propagation, and
  retained-agent behavior. Consult current gptel dispatch before fixing the
  design: gptel already runs multiple tool calls from one response in
  parallel, so known fan-out alone does not justify ToolScript.
- **Rejected shortcut:** Do not implement ToolScript as native `eval` plus a
  claimed function whitelist. `Glob`, `Grep`, and the mevedel pipeline are
  asynchronous, and safely constraining native Elisp requires an interpreter
  or an equivalently complex validator. Do not bypass the pipeline to make a
  read-only prototype appear smaller.
- **Do not port macher's sandbox boundary.** Its interpreter architecture is
  worth adapting; its boundary is not, and the defects are still present
  upstream as of `020830f`. It passes model-authored text through host
  `macroexpand-all`, so any macro expander runs on that text --
  `(eval-when-compile FORM)` executes FORM during expansion, which is
  arbitrary host code from a script. Its primitive table is scraped by
  `mapatoms` over the `pure`/`side-effect-free` properties, an open set that
  includes host-state readers and shifts with whatever packages are loaded.
  Its evaluator also evaluates an unknown operator's arguments before
  rejecting the operator, so a forbidden wrapper around a real tool call runs
  the tool first. Passing an ENVIRONMENT to `macroexpand-all` does not fix
  the first defect: it shadows named macros but does not restrict expansion,
  so an unlisted macro still expands from the global obarray. Guest
  identifiers must not be interned into the host obarray either; plain `read`
  leaks them permanently.
- **Decision:** The durable design is recorded in
  [`ADR 0111`](adr/0111-run-programmatic-tool-calls-in-a-closed-machine.md).
  The explicit machine, structured nested outcome, generated request roster,
  synthetic child identity, live aggregate row, value budgets, partial-work
  settlement, and bounded `parallel`/`parallel-map` joins are implemented.
- **Blast radius:** Tool registry and pipeline reentrancy, permissions and
  prompts, request cancellation, skills frontmatter and invocation ownership,
  prompt assembly, transcripts and audit records, render-data, telemetry,
  agents, compaction, and security. A porous interpreter or incorrectly
  inherited primitive set would turn skill metadata into arbitrary Emacs
  execution or an authority escalation.

### Investigate other macher-agent ideas

- **Source:** The same `macher-agent` review; these are investigation leads,
  not commitments and do not justify porting its VFS or Zero-Mem/PageRank
  implementation.
- **Exact history recall:** Determine whether finalized root segments and
  numbered agent compaction archives should form searchable `history://root`
  and `history://root/PATH` corpora for the existing `Grep` tool. Prefer the
  resource-address seam over a separate SearchHistory tool; use regex search,
  not semantic/PageRank retrieval.
- **Repeated tool-result cost (resolved 2026-08-21: do not build):** Measured
  over 43 retained chat contexts (417 completed tool results): 14 exact
  call+result duplicates totaling roughly 64 tokens of potential saving.
  `Read` had 0 exact duplicates -- the source-level read dedup already removes
  the dominant repeat class -- and 3 repeated `Read` calls whose results had
  changed, confirming that a call-signature key alone (macher-agent's design)
  would serve stale results. Artifacts and scanner under
  `.scratch/macher-agent/` (`tool-result-dedup-measurement.md`,
  `measure-tool-result-dedup.el`, `tool-result-dedup-data.json`). Revisit only
  if a future profile shows material duplicate volume; the correct design is
  then request-time elision keyed on identical call AND result, eliding the
  newest occurrence to keep the provider prompt-cache prefix stable, with
  compaction invalidating any elision whose original body left the retained
  tail.
- **Tool-call ID ambiguity:** The dedup measurement found 5 duplicate results
  whose persisted call IDs were reused because gptel associates parallel
  same-name tool calls with one ID. A tool-call ID is therefore not a unique
  referent. Audit any mevedel feature that assumes ID uniqueness (transcript
  audit records, render-data association, compaction evidence) and consider an
  upstream gptel fix.
- **Verify unapplied patches:** Investigate running checks against a selected
  ApplyPatch proposal in a disposable projected worktree. Reuse session fork,
  patch, execution, and cleanup machinery where it actually fits, but account
  for dirty user state, untracked files, selected hunks, dependencies, remote
  targets, permissions, cancellation, and cleanup before treating this as a
  patch-review action.
- **Concurrent worker conflicts:** Measure stale ApplyPatch proposals and
  overlapping untracked Bash/Eval mutations. The existing submission-time
  baseline check is the preferred fail-fast seam; add isolation or locking
  only if real incidents escape it.
- **Large-workspace Xref:** Profile `XrefReferences` on a genuinely large
  repository. Its output cap applies after the backend has returned a complete
  list, so it cannot prevent a backend from allocating excessive results.
  Prefer `Grep` for broad workspace search; pursue a backend-specific limit
  only if profiling reproduces the failure.
- **Status check:** Each idea has a plausible existing mevedel seam, but none
  has usage or profiling evidence showing that another module should be
  built.

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
