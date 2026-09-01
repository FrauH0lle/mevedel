# AGENTS.md

## Project Overview

**mevedel** is an Emacs Lisp package that provides a visual workflow for
interacting with LLMs during programming. It enables overlay-based
instruction management for AI-assisted development with direct gptel
integration.

## NO BACKWARDS COMPATIBILITY

mevedel is under active development and has no backwards-compatibility
contract. There is currently one known user, so prefer the cleanest current
design even when it breaks existing APIs, commands, configuration, persisted
state, or workflows.

- Do not add compatibility wrappers, aliases, shims, deprecation layers,
  dual-format readers/writers, version gates, or migrations unless the user
  explicitly requests compatibility for that specific change.
- Remove superseded code and update all in-repo callers, tests, fixtures, and
  documentation in the same change. Do not leave the old path alongside the
  new one.
- Existing compatibility code is not precedent. Delete it when a touched
  design no longer needs it.
- Prefer a direct breaking change over complexity introduced solely to support
  older mevedel versions or previously persisted local state.
- Call out destructive effects in the handoff, but do not preserve the old
  behavior merely to avoid a break.

## ADRS ARE REVISABLE

An accepted ADR records why a decision was made at the time. It is not a
boundary on later work. When evidence changes the trade, change the ADR.

- Amend an ADR in the same change that changes the behavior it describes. An
  ADR documenting a design the code no longer has is worse than no ADR.
- Supersede instead of amending when the decision itself is reversed: keep the
  old record, mark it superseded, and name the ADR replacing it.
- State what moved the decision — a measurement, a failure, a constraint that
  turned out not to hold. "We changed our minds" is not a reason; "the profile
  put this at 21% and the check was redundant with the target-side proof" is.
- Do not defer to an ADR you disagree with. Argue with it, in it.

## Documentation map

This file is the entry point. Detail docs live in `docs/` and are loaded
lazily — read them when planning work in the relevant area. The
`docs/` tree is the maintained working documentation.

- [`docs/architecture.md`](docs/architecture.md) — key data structures
  (`mevedel-workspace`, `-session`, `-request`, `-tool`), workspace
  context chain, gptel integration, persistent memory layout, chat
  buffer formatting
- [`docs/address-to-resource.md`](docs/address-to-resource.md) — closed
  resource-address families, canonical locators, operation matrix, permission
  seam, lifecycle, freshness, and capability boundaries
- [`docs/view.md`](docs/view.md) — dual-buffer view model, status /
  interaction / input zones, rendered agent transcript views, input
  history
- [`docs/tools.md`](docs/tools.md) — tool pipeline
  (validate → permission → snapshot → handler → persist), `:wrap` /
  `:groups`, renderers and render-data side channel, oversized result
  persistence
- [`docs/ptc-dialect.md`](docs/ptc-dialect.md) — closed ToolScript language,
  pure primitives, nested tools, parallel calls, limits, and security boundary
- [`docs/permissions.md`](docs/permissions.md) — 8-step decision chain,
  bucket precedence, Bash/Eval specifics,
  sub-agent permission propagation, example config
- [`docs/guardian-prompts.md`](docs/guardian-prompts.md) — trusted guardian
  prompts, untrusted evidence boundaries, response contracts, examples
- [`docs/agents.md`](docs/agents.md) — worker/explorer/verifier/reviewer,
  retained asynchronous spawning, canonical paths, mailboxes, waits,
  tree-wide capacity, and task status
- [`docs/preview.md`](docs/preview.md) — inline diff overlay,
  keybindings, mode dispatch, handler return shape
- [`docs/plan-mode.md`](docs/plan-mode.md) — sticky Plan conversations,
  proposal approval axes, tool boundary, implementation handoff, recovery
- [`docs/mentions.md`](docs/mentions.md) — `@ref`/`@file`/`@agent`/`@mcp`
  expansion, dedup, completion CAPFs
- [`docs/skills.md`](docs/skills.md) — SKILL.md discovery, slash
  invocation, model-side Skill, allowed-tools, model / effort
  overrides, forked skill dispatch, review skill
- [`docs/hooks.md`](docs/hooks.md) — hook subsystem: prior art,
  lifecycle events, config layers, command/Elisp handlers, pipeline
  integration, trust model, dry-run inspection, logs
- [`docs/reminders.md`](docs/reminders.md) — the two delivery
  channels (ephemeral by default, position-bound permanence), staging
  seams, injection contract, hidden injection record, implemented
  reminder surface
- [`docs/goals.md`](docs/goals.md) — Goal context, continuation, accounting,
  failures, commands, recovery, and accepted-plan authority
- [`docs/sessions.md`](docs/sessions.md) — on-disk layout, segment
  persistence contract, resume/rewind/fork, locking, auto-cleanup,
  defcustoms
- [`docs/compaction.md`](docs/compaction.md) — manual and automatic
  conversation compaction, token thresholds, gptel token baseline,
  anchored summaries, tail preservation, segment integration
- [`docs/telemetry.md`](docs/telemetry.md) — append-only lifecycle telemetry,
  data policy, profiler artifacts, prompt guard, Goal reproduction procedure
- [`docs/buddy.md`](docs/buddy.md) — unasked review of recent edits and
  the guidance command, note lifecycle, ephemerality, model selection
- [`docs/commits.md`](docs/commits.md) — commit message format and
  guidelines
- [`docs/backlog.md`](docs/backlog.md) — canonical
  backlog for notes, todos, feature ideas, fixes, and deferred work

Each `.el` file has its own `;;; Commentary:` block describing its
purpose. Open the file for details.

## Module layer map

```
Entry point
  mevedel.el                  top-level loader, install/uninstall, directives

Data model
  mevedel-structs.el          passive workspace/session/request/task data shapes and invariants
  mevedel-directive.el        directive mutation, lifecycle, plan invalidation, rewind
  mevedel-turn.el             request admission/cancellation and terminal settlement
  mevedel-workspace.el        workspace detection, registry, and state lookup
  mevedel-workspace-identity.el project-owned durable workspace identity
  mevedel-models.el           model tier/provider resolution, context budget
  mevedel-hooks.el            project/user/skill/agent hook loading + runner
  mevedel-prompt-submission.el accepted prompt + lifecycle-context transaction
  mevedel-bash-analysis.el    conservative shell parsing and normalized command facts
  mevedel-bash-policy.el      Bash classification, reusable rules, guardian policy
  mevedel-transport.el        remote reentrancy detection and idle-transport deferral
  mevedel-execution-target.el immutable local/TRAMP target, path domains, readiness
  mevedel-execution.el        managed execution registry, admission, and facade
  mevedel-execution-process.el opaque child, process-group, and spool lifecycle
  mevedel-execution-transcript.el durable execution render data and archive reconciliation
  mevedel-execution-scheduler.el fair session-scoped Bash admission
  mevedel-execution-telemetry.el safe execution facts and profiler adaptation
  mevedel-sandbox.el          optional Bubblewrap child-process confinement
  mevedel-sandbox-grants.el   exact FD-backed grants and symlink mount planning
  mevedel-telemetry.el        append-only lifecycle events and profiler capture
  mevedel-plan.el             lifecycle-neutral plan data and artifacts
  mevedel-plan-handoff.el     durable accepted-plan preparation and kickoff
  mevedel-permission-mode.el  mode normalization, session scoping, lifecycle
  mevedel-permission-rules.el rule parsing, matching, buckets, resource grants
  mevedel-permission-persistence.el target-aware authority store codec
  mevedel-permissions.el      permission preflight and 8-step decision facade
  mevedel-tool-permission.el permission-step orchestration, hooks, prompts, logging
  mevedel-pipeline.el         tool context, standard steps, sequencing, ordering
  mevedel-ptc-checkpoint.el  durable ToolScript audit settlement across restart
  mevedel-ptc-driver.el      ToolScript orchestration, nested calls, progress
  mevedel-ptc-interpreter.el closed programmatic-tool-call evaluator and machine
  mevedel-resource.el         resource-address grammar and attempt lifecycle
  mevedel-resource-capf.el    resource-address completion
  mevedel-permission-log.el   durable permission decision log
  mevedel-tool-media.el       tool media storage, scrubbing, provider payloads
  mevedel-tool-render-data.el render-data codec, provider scrubber, transcript mutation
  mevedel-tool-registry.el    mevedel-tool struct, mevedel-define-tool macro
  mevedel-tool-repair.el      structured validation and atomic input repair
  mevedel-tool-repair-gptel.el  lossless gptel argument decoding bridge
  mevedel-tool-repair-diagnostics.el  repair audit and telemetry
  mevedel-queue.el            shared interaction entry metadata
  mevedel-permission-queue.el permission/Bash/Eval/execution-authority queue
  mevedel-reminders.el        system-reminder injection
  mevedel-edit-diagnostics.el post-edit Flymake/Flycheck report state machine
  mevedel-plugin-registry.el  plugin manifests, activation state, hook consent
  mevedel-plugin-lifecycle.el managed Git install, update, and removal
  mevedel-plugin-ui.el        plugin cockpit and /plugin command
  mevedel-plugins.el          narrow session/workspace plugin facade
  mevedel-skills-core.el      skill model, discovery, state, reload
  mevedel-mention-bindings.el shared atomic mention validation and edit lifecycle
  mevedel-skills-preparation.el argument substitution and body injection execution
  mevedel-skills-invoke.el    request context, invocation, fork dispatch, model tools
  mevedel-skills-input.el     user token binding, raw dispatch, inline projection
  mevedel-skills-plan.el      deterministic user invocation planning and preparation
  mevedel-skills-prompt.el    model-visible roster, reminders, activation
  mevedel-skills-ui.el        slash commands, cockpit, completion, font-lock

Chat / view
  mevedel-chat.el             session lifecycle
  mevedel-directive-request.el  directive prompts, dispatch, and settlement
  mevedel-side-conversation.el  ephemeral /btw conversation lifecycle
  mevedel-directive-activity.el  read-only workspace directive inspector
  mevedel-directive-frame.el  directive-anchored child frame and transcript filter
  mevedel-directive-plan.el   directive-owned planning and approval workflow
  mevedel-transcript.el       transcript span classification for view/persistence/compaction
  mevedel-transcript-audit.el hidden audit record encoding and structural parsing
  mevedel-transcript-restore.el  transcript property restoration via the canonical grammar
  mevedel-context-summary.el  stateless validated continuation/handoff summary generation
  mevedel-view.el             view mode, zones, and session coordination
  mevedel-view-agent.el       agent transcript inspection, status rows, refresh
  mevedel-view-composer.el    composer geometry, submission, root dispatch, fork/send flow
  mevedel-view-input-files.el local file drops and clipboard-image input
  mevedel-pending-inputs.el   pending queue, steering, delivery, and cockpit
  mevedel-patch-review.el     staged ApplyPatch review UI
  mevedel-plan-mode.el        Plan conversations and proposal approval UI
  mevedel-view-interaction.el interaction registration, ordering, callback overlays, redraw
  mevedel-view-control-transfer.el cooperative transfer polling, presentation, and commands
  mevedel-view-disclosure.el  source-backed transcript disclosure state and actions
  mevedel-view-render.el      transcript projection, source mapping, live navigation
  mevedel-view-segments.el    historical session segment projection and navigation
  mevedel-view-stream.el      request progress and streaming redraw scheduling
  mevedel-gptel-stream-bridge.el private gptel stream compatibility advice
  mevedel-view-audit.el       audit disclosure rendering
  mevedel-view-zone.el        managed view-zone lifecycle + fragments
  mevedel-view-history.el     view input history ring and persistence
  mevedel-view-fontify.el     quiet generic and reusable Markdown fontification
  mevedel-collaboration.el    live browser room and lifecycle facade
  mevedel-collaboration-guest.el  untrusted guest protocol and input handling
  mevedel-collaboration-agent.el  browser agent roster and transcript fetch
  mevedel-collaboration-artifact-projection.el ApplyPatch artifact projection
  mevedel-collaboration-artifact.el browser artifact fetch and notifications
  mevedel-collaboration-projection.el canonical browser transcript projection
  mevedel-collaboration-share.el bearer-link and QR presentation surface
  mevedel-collaboration-transport.el sealed relay WebSocket client
  mevedel-view-markdown.el    Markdown links, images, paths, source panels
  mevedel-view-path.el        deferred target path verification and memoization
  mevedel-view-table.el       rendered pipe tables and window realignment
  mevedel-cockpit.el          shared tabulated cockpit surface plumbing
  mevedel-menu.el             session cockpit transient and model selection
  mevedel-gptel-bridge.el     view-launched gptel menu and restoration
  mevedel-executions-list.el  session-wide live execution cockpit and user controls
  mevedel-artifacts-list.el   session artifacts cockpit: list, open, delete-as-unpublish
  mevedel-permissions-list.el remembered authority cockpit and per-row revoke
  mevedel-worktree.el         Git worktrees, status/list surfaces, fork plumbing
  mevedel-instruction-registry.el workspace instruction buckets, IDs, links
  mevedel-overlays.el         instruction geometry, tags, context, prompts
  mevedel-directive-source.el directive anchor/presentation lifecycle
  mevedel-overlay-ui.el       instruction overlay actions and rendering
  mevedel-mentions.el         @ref and @file mention expansion
  mevedel-directive-persistence.el  workspace directive record codec
  mevedel-persistence.el      save/load instructions
  mevedel-session-codec.el    closed session sidecar codec and validation
  mevedel-session-artifacts.el  paths, artifacts, snapshots, and segment writes
  mevedel-session-durability.el lease and storage primitives
  mevedel-session-recovery.el  specialized recovery protocol and markers
  mevedel-session-transfer.el  durable cooperative control transfer protocol
  mevedel-session-publication.el immutable publication, generation collection, diagnostics
  mevedel-session-save-as.el portable Save As transaction and adoption
  mevedel-session-persistence.el  lifecycle/resume/listing/locking/cleanup facade
  mevedel-session-rewind.el   restore plans, transactional Rewind, published-head redo
  mevedel-session-fork.el     Fork/Worktree projection, publication, and rename
  mevedel-session-control-fs.el   pinned target-side session control filesystem
  mevedel-session-control-transfer.el  control-transfer state, drains, descriptors
  mevedel-compact-estimation.el compaction token accounting and admission
  mevedel-compact-evidence.el transcript evidence and tool-safe truncation
  mevedel-compact-target.el   root/agent archive and application transactions
  mevedel-compact-run.el      async compaction retry/cancel/settlement
  mevedel-compact.el          public compaction command and gptel gate

Prompt / presets / agents
  mevedel-system.el           system prompt assembly
  mevedel-presets.el          gptel presets and request-time FSM assembly
  mevedel-specialist-nudges.el  generic-tool specialist prompting policy
  mevedel-agents.el           worker/explorer/verifier/reviewer definitions
  mevedel-agent-conversation.el  retained conversation buffers, activity, and saves
  mevedel-agent-control.el    retained-agent tree addressing, turns, mail, waits
  mevedel-agent-exec.el       sub-agent request runner and FSM handlers
  mevedel-agent-persistence.el durable agent registry codec and cold hydration
  mevedel-agent-runtime.el    retained agent request lifecycle and settlement
  mevedel-goal.el             phase-free Goal continuation controller
  mevedel-review.el           /review picker, reviewer output parsing, parent transcript injection

Tools (each dispatches through mevedel-pipeline)
  mevedel-tool-ptc.el        ToolScript roster, request adapter, registration
  mevedel-tool-fs.el          filesystem tool registration and shared path/resource primitives
  mevedel-tool-fs-read.el     Read text/media decoding and bounded output
  mevedel-tool-fs-search.el   Glob/Grep execution and resource-output privacy
  mevedel-tool-patch.el       ApplyPatch parse/match/apply engine + tool
  mevedel-tool-code.el        XrefReferences, XrefDefinitions, Imenu, Treesitter
  mevedel-tool-exec-permission.el Bash/Eval authority and prompt adapters
  mevedel-tool-exec.el        Bash/Eval lifecycle, rendering, registration
  mevedel-tool-web.el         WebSearch, WebFetch
  mevedel-interaction-prompt.el  shared interaction overlay lifecycle
  mevedel-permission-prompt.el   generic, Bash, Eval, and execution-authority prompt UI
  mevedel-tool-ask.el         Ask handler, result renderer, registration
  mevedel-tool-ask-ui.el      Ask form state, controllers, and presentation
  mevedel-tool-ui.el          Agent/InterruptAgent/ToolSearch/SendMessage assembly
  mevedel-tool-task.el        TaskCreate/Update/List/Get + overlay
  mevedel-tool-skills.el      Skill and ListSkills tool schemas
  mevedel-tool-introspect.el  wraps gptel-agent introspection tools
  mevedel-buddy.el            edit recording, diff assembly, review requests
  mevedel-buddy-note.el       ephemeral note overlays and their model tools
  mevedel-tools.el            complete tool registration + deferred-tool machinery
  mevedel-tools-list.el       native tools cockpit list

Support
  mevedel-file-state.el       LRU file cache
  mevedel-diff-apply.el       transactional unified diff staging/application
  mevedel-utilities.el        package version + shared tinting/env helpers
  mevedel-init.el             repository guidance bootstrap command
```

## External dependencies

- **gptel**, **gptel-agent**, **websocket**, **qrencode**, **Emacs >=31.1**,
  **org-mode**

Eask dependency installs can get stale. 
Run `npx @emacs-eask/cli upgrade PACKAGE` to update. For example:

```bash
npx @emacs-eask/cli upgrade gptel gptel-agent
```

## gptel and gptel-agent source rule

mevedel is tightly coupled to gptel and also depends on gptel-agent. Before
implementing or changing behavior that touches prompts, requests, callbacks,
tool calls, presets, buffers, transcripts, session flow, agents, or
coordination, consult gptel and gptel-agent source and reuse their existing
APIs or patterns instead of duplicating them.

Ensure the repositories are cloned:

```bash
# First time
mkdir -p .scratch/upstream
git clone https://github.com/karthink/gptel .scratch/upstream/gptel
git clone https://github.com/karthink/gptel-agent .scratch/upstream/gptel-agent
```

Prefer a refreshed upstream checkout, because Eask dependency installs can get
stale:

```bash
# Refresh before consulting
git -C .scratch/upstream/gptel pull --ff-only
git -C .scratch/upstream/gptel-agent pull --ff-only
```

## Development Commands

### Testing
Run `npx @emacs-eask/cli clean elc` before tests so stale bytecode never
shadows edited source files.

```bash
# Clear stale bytecode first
npx @emacs-eask/cli clean elc

# With Eask installed
eask test ert test/test-*.el

# Via npx
npx @emacs-eask/cli test ert test/test-*.el

# Single file
npx @emacs-eask/cli test ert test/test-mevedel-compact.el
```

Test files mirror modules: `test/test-mevedel-MODULE.el`. Shared helpers
(including the `mevedel-deftest` macro) are in `test/helpers.el`. Tests
use real temp files/directories rather than mocking. Eask gives ERT a temporary
`HOME` and XDG roots; the shared helper rejects unsafe test invocations that
could reach real user state.

### Byte compilation
```bash
npx @emacs-eask/cli compile

# Clean up .elc files
npx @emacs-eask/cli clean elc
```
Keep the byte compiler silent: no free-variable or unknown-function
warnings.

### Key interactive commands
- `mevedel-create-reference` / `mevedel-create-directive` /
  `mevedel-convert-instructions`
- `mevedel-save-instructions` / `mevedel-load-instructions`
- `mevedel-implement-directive` / `mevedel-discuss-directive` /
  `mevedel-request-directive-changes` / `mevedel-retry-directive`
- `mevedel`
- `mevedel-rewind` / `mevedel-redo` / `mevedel-save-session` /
  `mevedel-rename-session`
- `mevedel-take-control` / `mevedel-release-control` /
  `mevedel-toggle-follow` / `mevedel-refresh-session`
- `mevedel-process-directives`, `mevedel-next/previous-instruction`
- `mevedel-diff-apply-buffer`
- `mevedel-compact`
- `mevedel-review` / `mevedel-verify`
- `/plan` / `/plan PROMPT` / `mevedel-retry-plan-implementation`
- `/btw` / `/btw PROMPT`
- `mevedel-add/remove/list-project-roots`
- `mevedel-toggle-tasks`
- `mevedel-buddy-mode` / `mevedel-buddy-global-mode` /
  `mevedel-buddy-review` / `mevedel-buddy-guide`
- `mevedel-buddy-dismiss-note` / `mevedel-buddy-dismiss-notes` /
  `mevedel-buddy-abort`
- `mevedel-retry-plan-implementation`

## Code style

- **Lexical binding**: `;;; file.el -- Description -*- lexical-binding: t -*-`
- **Headers**: standard `;;; Commentary:` / `;;; Code:` sections
- **Section headers**: two blank lines above. Major: `;;` + blank + `;;;`.
  Subsections add more semicolons: `;;;;`, `;;;;;`, ...
- **Forward declarations**: grouped at file top by source package with
  `;; \`gptel'` style comment headers. Sort source-package groups
  alphabetically; within each group, put all `declare-function` forms first
  alphabetically, then all `defvar` forms alphabetically.
- **Customization**: `defcustom` uses `:group 'mevedel`
- **Private symbols**: double-dash `--` (e.g. `mevedel--workspace`,
  `mevedel-tools--validate-params`)
- **Path construction**: use `file-name-concat`, not `concat`, to join
  filesystem path components.
- **Provide**: each file ends with `(provide 'mevedel-MODNAME)` and
  `;;; mevedel-MODNAME.el ends here`
- **Minimize explicit runtime `require`s**: prefer actual autoloaded entry
  points. Use `declare-function` and `defvar` for byte-compiler declarations
  only; they do not load libraries, and variable access does not trigger
  autoloading.
- **Load dependencies at feature boundaries**: when a runtime dependency is
  not autoloaded or otherwise guaranteed to be loaded, `require` it once in a
  cold command/setup entry point, or at top level when it is unconditional
  and acyclic. Use `eval-when-compile` only for compile-time dependencies.
- **Never call `require` on a hot path**: code reached per segment, chunk,
  redraw tick, or guest step must rely on an earlier load boundary. A profiled
  session attributed 20% of CPU samples to repeated `require` calls. Avoid
  circular dependencies through module direction rather than scattering
  lazy `require`s through helpers.
- **ASCII in code, unicode only in UI-facing strings**: comments,
  identifiers, and non-UI strings stay ASCII (use `->` not `→`,
  `lambda`/`fn` not `λ`). Unicode is fine in `propertize`, overlays,
  prompts, and other strings the user actually sees.
- **No spec references in code comments**: don't write `(spec 13)` or
  `(see spec 19)`. Specs are implementation-phase artifacts; code
  comments must stand on their own. Describe what a slot/variable
  holds, not where it was designed.
- **`error` strings**: capitalized, no package prefix —
  `(error "Unknown tool: %s" name)`. The backtrace identifies the
  source. checkdoc enforces capitalization. When the first word is a
  literal binary, option, or parameter name that must stay lowercase,
  quote it instead of changing its spelling: `(error "'pdftoppm' not
  installed")`.
- **`message` strings**: lowercase `"mevedel: ..."` prefix is fine —
  `(message "mevedel: stale request found, replacing")`. Output goes
  to `*Messages*` where there's no backtrace, so the prefix earns its
  keep.

## Testing conventions

- **Framework**: ERT via `mevedel-deftest` macro (`test/helpers.el`)
- **Naming**: the primary `test/test-mevedel-{module}.el` matches source.
  Focused `test/test-mevedel-{module}-{subject}.el` supplements are allowed
  when a single-function suite would make the primary file unwieldy.
- **One deftest per function**: all cases in one macro call; label with
  `:doc` strings. Rare exceptions are allowed where setup differs drastically.
- **Real files**, not mocks. Clean up in teardown.
- **Helpers require**:
  ```elisp
  (require 'helpers
           (file-name-concat
            (file-name-directory
             (or buffer-file-name load-file-name byte-compile-current-file))
            "helpers"))
  ```
- **Generated test names**: `FUNCTION/test` or `FUNCTION/test@N`
- **Doc strings**: describe what is tested, group with shared prefix
- **View/status redraws**: when changing async view, status-zone, agent, or
  task redraw paths, test that an active composer draft stays unchanged,
  including a multiline draft whose first editable character is `>`.
- **New functions need tests**; modify tests when behavior changes
- **Silent output**: a test run prints only ERT's own progress lines
  (`passed N/M name (time)`) and its final summary. No `mevedel:` messages,
  no `Warning`/`Error` lines, no Emacs notices such as `Making
  gptel-org-branching-context buffer-local while locally let-bound!`, and
  nothing at all during the loading phase. Noise hides real failures and is
  treated as a defect in the test or in the code it exercises:
  - A test that deliberately injects a failure must capture the resulting
    message or warning instead of letting it reach the run log.
  - Product code must not warn on a path a passing test takes; if the warning
    is legitimate, the test must assert it rather than emit it.
  - Loading a test file must have no side effects: no sessions, no requests,
    no messages. Anything that runs belongs inside a test body.
  - `mevedel-test--with-captured-diagnostics` captures messages and warnings;
    `mevedel-test--with-captured-messages` captures messages only, for a case
    that still inspects the warnings it raises. Both take a place to bind the
    captured text, or nil when the durable state the diagnostic echoes is what
    the case asserts. A capture must not forward to the original function:
    that re-prints what it just captured.
  - `mevedel-test--muted-message-regexps` in `test/helpers.el` drops the few
    third-party progress messages mevedel cannot suppress at their source.
    Nothing mevedel itself emits belongs there.
  - `mevedel-deftest`'s `:quiet t` captures the messages and warnings every
    case of one deftest provokes. Use it when the function under test
    correctly reports to the echo area on the paths its cases take and those
    cases assert the durable state the diagnostic echoes. Prefer an explicit
    capture with a place when the diagnostic text is itself the behaviour
    under test.
  - A notice Emacs raises from C, such as `Making VAR buffer-local while
    locally let-bound!`, reaches the log through neither mechanism. Remove
    its cause: let-binding a variable that product code then sets
    buffer-locally is the test's mistake, not the code's.
- **Isolation**: a test leaves no global state behind — no live session in the
  execution registry, no workspace registry entry, no live timer, no target
  connection, and no file outside its own temporary directory. Leaked state
  makes later tests slow, noisy, and order-dependent.

## Byte compilation rules

- No free-variable or unknown-function warnings
- `declare-function` for external functions, `defvar` for external vars
- `eval-when-compile` for compile-time-only deps like `cl-lib`
- Run `npx @emacs-eask/cli compile` before committing
- To clear stale `.elc` files use `npx @emacs-eask/cli clean elc`, not
  `find -delete` — eask owns the build dir layout and clears related
  caches that bare `find` misses

## Agent skills

### Issue tracker

Issues and PRDs are tracked as local markdown files under `.scratch/<feature-slug>/`. `.scratch/` is gitignored local agent state; promote durable PRD decisions to maintained docs. See `docs/agents/issue-tracker.md`.

### Triage labels

The default canonical triage labels are used unchanged. See `docs/agents/triage-labels.md`.

### Domain docs

This repo uses a single-context domain layout with root `CONTEXT.md` and ADRs under `docs/adr/`. See `docs/agents/domain.md`.
