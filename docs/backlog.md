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
- Cursor jumps on permission promtp back to composer, seems to follow a certain tick rate
- ApplyPatch persisted diff not rendered nicely/highlighted
  
  
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
  and target-side portability foundation. The relay-based live collaboration
  slice (`.scratch/browser-relay/PRD.md`) has landed: content-blind Go relay
  in `relay/`, end-to-end sealed frames, two-tier bearer links, guest
  prompting/interrupting, and the directive filter. Remote Mevedel and
  managed provisioning remain later slices.

### Browser-relay ui-request surface: landed

- **Source:** `.scratch/browser-relay/PRD.md`, implementation decision
  "Remote interaction answering". The full surface has landed: generic
  request prompts (approve/deny/feedback), permission prompts (one-shot
  allow-once/deny-once/feedback -- durable authority stays Emacs-only),
  plan approval (accept with host-configured axes and remote feedback
  that queues the templated revision request; Worktree proposals offer
  feedback only, and axis editing stays in Emacs), ApplyPatch review
  (apply the staged
  selection or request a revision with whole-patch feedback), and Ask
  questionnaires (the frame carries the questions, options, and current
  answers structurally; the guest answers atomically and the host adopts
  them through the wizard's own submit path; host navigation re-announces
  so guests stay in sync). Gated by
  `mevedel-collaboration-remote-interactions`. Guest attribution is
  durable via hidden `guest-prompt` transcript audit records and renders
  as the turn heading in both surfaces. Guests can attach photos to
  prompts (downscaled client-side, saved under the session media
  directory, @file-mentioned with read grants), and guest input is
  skill-inert: `$skill` tokens stay literal text at submission.
- **Blast radius:** Execution-target identity, readiness and bootstrap,
  durability and leases, session discovery and control transfer, worktree
  workflows, live session projection, browser rendering, transport, and
  bearer-capability security.

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

## View rendering

### Restore responsive Markdown tables and inline images

- **Source:** B12-3 and `8f83a25` removed mevedel's table padder and declared
  raw fontified pipe tables sufficient. That deletion removed a weak
  implementation, not an unwanted feature. The reference behavior is
  agent-shell's table reflow from
  [`ff688fc`](https://github.com/xenodium/agent-shell/commit/ff688fcf49e465631134e3a01f0120404121708b)
  and its shared table/image realignment in 0.73.
- **What's owed:** Restore beautified tables as a view guarantee by adapting
  the current agent-shell table renderer into a focused mevedel projection:
  render canonical pipe-table Markdown into styled box-drawing rows, retain
  the raw table source and measured window width, proportionally shrink and
  wrap columns to the usable width (including any `line-prefix` inset), and
  re-render only stale tables after window size or displayed-buffer changes.
  Extend `mevedel-view-inline-image-max-width` to accept a window-width ratio
  as well as fixed pixels; retain each responsive image's sizing input and
  measured width so the same realignment job recreates only stale images.
  Use buffer-local hooks and one debounced idle timer, never mutate from a
  redisplay hook, and keep re-layout off the undo list without changing the
  modified flag.
- **Why deferred:** The view currently remains correct and reconstructable:
  raw tables are readable and inline images use a fixed 600-pixel maximum.
  The desired presentation needs a deliberate port of agent-shell's measured
  renderer, not resurrection of mevedel's source-padding parser or a dependency
  on agent-shell/md-mode. md-mode lacks auto-resize and its whole-document
  renderer conflicts with mevedel's source panels, links, mentions, images,
  and source mapping.
- **Status check:** `mevedel-view--decorate-markdown-in-range` decorates code
  blocks, images, links, and paths but leaves tables raw. In-flight streaming
  already rebuilds the disposable view turn from the canonical data buffer,
  so omit agent-shell's rendered-table row-refolding machinery. Preserve only
  mevedel-owned source/read-only/turn properties across table replacement,
  and exclude fenced source panels.
- **Acceptance:** Resizing a frame, splitting a window, or first displaying an
  off-screen view reflows tables and ratio-sized images after a debounce while
  leaving the data buffer, point, window position, and active composer draft
  unchanged. Cover a multiline draft beginning with `>`, tool-body indentation,
  escaped pipes and backticks, links and faces inside cells, narrow hard wraps,
  emoji/CJK width, fenced code exclusion, multiple tables, off-screen render,
  and image resize. A table remains aligned under variable-pitch display.
  Audit every copy path, choose and document whether it yields rendered box
  characters or canonical Markdown, and test that contract before landing.
- **Initial boundary:** Do not add md-mode or agent-shell as dependencies. Skip
  cell-navigation keys, agent-shell streaming row extension, interactive image
  `+`/`-`/`0` controls, image attributes, and glyph-height normalization until
  usage demonstrates they are needed. Accept and document that one mutable
  view buffer can hold only one layout when shown simultaneously in windows of
  different widths.
- **Blast radius:** Tables are frequent model output and currently lose useful
  structure; fixed-width images waste space or overflow narrow views. A careless
  resize implementation can corrupt source mapping, spread keymaps/faces, move
  point, or damage the composer, so these are must-work view cases rather than
  optional cosmetic tests.

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

### Restore must-work real-world diff overlay tests

- **Source:** The real-world Shiny/R overlay fixture removed by `907fc38` from
  `test/test-mevedel-diff-apply.el` while replacing the custom geometry engine
  with `replace-buffer-contents` and canonical edit hooks.
- **What's owed:** Restore the real-world diff examples as public
  `mevedel-diff-apply-buffer` acceptance tests using registered directives and
  references. Add an explicit note beside them that these are must-work user
  cases and must not be deleted when the implementation changes; change their
  expected behavior only when the product requirement itself changes.
- **Why deferred:** The original Shiny/R example still passes when replayed
  against the native replacement path, but the broader behavior should be
  observed before deciding which additional real-world fixtures belong in the
  permanent set.
- **Status check:** The focused suite has final-byte and lifecycle coverage,
  but no retained real-world example; deleting the old fixture made the green
  suite unable to prove this use case.
- **Blast radius:** A later edit can regress semantic overlay tracking during
  realistic multi-location changes while synthetic transaction tests remain
  green.

### Restore bounded Buddy context expansion

- **Source:** B24-3 and `7745fe8` deleted Buddy's unrestricted `read_buffer`
  tool because an unattended review could read an entire changed buffer beyond
  its bounded diff payload. The broad authority was wrong, but removing all
  context expansion also removed the intended inspect-then-annotate workflow.
- **What's owed:** Add a bounded `read_around`-style Buddy tool anchored to a
  live line marker already shown in the current review. Return only a fixed
  local window, register markers for the newly exposed live lines so they may
  receive notes, and retain generation and review-scope checks. Do not permit
  arbitrary ranges, omitted bounds that mean the whole buffer, or reads from
  buffers outside the running review. Restore prompt guidance and test both the
  successful read-then-annotate flow and every authority boundary.
- **Why deferred:** Real usage should determine the useful window and whether
  one expansion per anchor is enough; those choices do not justify restoring
  the old whole-buffer reader meanwhile.
- **Status check:** Automatic Buddy currently receives six context lines on
  either side of each change and must stay silent whenever that payload is
  insufficient. It has no way to inspect a nearby definition or surrounding
  block before deciding whether a note is justified.
- **Blast radius:** Buddy can miss real defects that need slightly more local
  context, while an over-broad replacement would again expand unattended
  provider access beyond what the user was shown.

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

## Two test files still depend on suite load order

- **What's owed:** `test/test-mevedel-overlays-source.el`
  (`mevedel-directive-activity-archive`) and
  `test/test-mevedel-interaction-prompt.el` (`mevedel--prompt--settle` case 3)
  pass in a full run and fail when their file is run alone, which is the
  documented single-file workflow.
- **Why deferred:** the overlays-source failure is an eager macro-expansion
  cycle in product requires -- `mevedel-session-persistence` ->
  `mevedel-agents` -> `mevedel-tool-registry` -> `mevedel-pipeline` ->
  `mevedel-permissions` -> `mevedel-session-persistence` -- that only closes
  when the modules load from source in that order. Breaking it means moving
  requires in product code, which is a wider change than test hygiene.
- **Status check:** every other test file was made to run standalone by
  adding the `require` it was borrowing from a sibling file; these two are
  not missing a require.
- **Blast radius:** a developer running one file sees failures that are not
  theirs, and learns to distrust single-file runs.
