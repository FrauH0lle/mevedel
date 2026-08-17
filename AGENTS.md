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
- [`docs/permissions.md`](docs/permissions.md) — 9-step decision chain,
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
- [`docs/reminders.md`](docs/reminders.md) — system-reminder injection,
  implemented reminder surface, and candidate reminder backlog grouped
  by implementation readiness
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
- [`docs/tutor.md`](docs/tutor.md) — Socratic tutor workflow, hint
  persistence
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
  mevedel-structs.el          workspace, session, request, task data invariants
  mevedel-directive.el        directive lifecycle, plan invalidation, rewind
  mevedel-turn.el             canonical success/failure turn settlement
  mevedel-workspace.el        workspace detection and registry
  mevedel-workspace-identity.el project-owned durable workspace identity
  mevedel-models.el           model tier/provider resolution, context budget
  mevedel-hooks.el            project/user/skill/agent hook loading + runner
  mevedel-prompt-submission.el accepted prompt + lifecycle-context transaction
  mevedel-bash-analysis.el    conservative shell parsing and normalized command facts
  mevedel-bash-policy.el      argument-aware read-only command policies
  mevedel-transport.el        remote reentrancy detection and idle-transport deferral
  mevedel-execution-target.el immutable local/TRAMP target, path domains, readiness
  mevedel-execution.el        bounded child-process lifecycle and session state
  mevedel-execution-scheduler.el fair session-scoped Bash admission
  mevedel-sandbox.el          optional Bubblewrap child-process confinement
  mevedel-sandbox-grants.el   exact FD-backed grants and symlink mount planning
  mevedel-telemetry.el        append-only lifecycle events and profiler capture
  mevedel-plan.el             lifecycle-neutral plan data and artifacts
  mevedel-plan-handoff.el     durable accepted-plan preparation and kickoff
  mevedel-permissions.el      9-step permission decision chain
  mevedel-pipeline.el         tool execution pipeline
  mevedel-tool-media.el       tool media storage, scrubbing, provider payloads
  mevedel-tool-registry.el    mevedel-tool struct, mevedel-define-tool macro
  mevedel-tool-repair.el      structured validation and atomic input repair
  mevedel-tool-repair-gptel.el  lossless gptel argument decoding bridge
  mevedel-tool-repair-diagnostics.el  repair audit and telemetry
  mevedel-queue.el            shared FIFO queue machinery
  mevedel-permission-queue.el permission/Bash/Eval/execution-authority queue
  mevedel-reminders.el        system-reminder injection
  mevedel-skills-core.el      skill model, discovery, state, reload
  mevedel-mention-bindings.el shared atomic mention validation and edit lifecycle
  mevedel-skills-invoke.el    skill preparation, invocation, model tools
  mevedel-skills-plan.el      deterministic user invocation planning and preparation
  mevedel-skills-prompt.el    model-visible roster, reminders, activation
  mevedel-skills-ui.el        slash commands, cockpit, completion, font-lock

Chat / view
  mevedel-chat.el             session lifecycle
  mevedel-side-conversation.el  ephemeral /btw conversation lifecycle
  mevedel-directive-activity.el  read-only workspace directive inspector
  mevedel-directive-plan.el   directive-owned planning and approval workflow
  mevedel-transcript.el       transcript span classification for view/persistence/compaction
  mevedel-transcript-audit.el hidden audit record encoding and structural parsing
  mevedel-transcript-restore.el  transcript property restoration via the canonical grammar
  mevedel-context-summary.el  stateless validated continuation/handoff summary generation
  mevedel-view.el             view mode, zones, and session coordination
  mevedel-view-agent.el       agent transcript inspection, status rows, refresh
  mevedel-view-composer.el    composer editing, submission, queueing, fork/send flow
  mevedel-pending-inputs.el   Pending Inputs cockpit and queue management
  mevedel-patch-review.el     staged ApplyPatch review UI
  mevedel-plan-mode.el        Plan conversations and proposal approval UI
  mevedel-view-interaction.el interaction registration, ordering, callback overlays, redraw
  mevedel-view-render.el      transcript rendering, folding, source mapping, navigation
  mevedel-view-stream.el      streaming, request progress, and gptel stream integration
  mevedel-view-audit.el       audit disclosure rendering
  mevedel-view-zone.el        managed view-zone lifecycle + fragments
  mevedel-view-history.el     view input history ring and persistence
  mevedel-collaboration.el    live browser room and lifecycle facade
  mevedel-collaboration-projection.el canonical browser transcript projection
  mevedel-collaboration-transport.el loopback HTTP/WebSocket transport
  mevedel-view-markdown.el    Markdown tables, links, images, paths, source panels
  mevedel-executions-list.el  session-wide live execution cockpit and user controls
  mevedel-permissions-list.el remembered authority cockpit and per-row revoke
  mevedel-overlays.el         instruction overlays (references/directives)
  mevedel-mentions.el         @ref and @file mention expansion
  mevedel-directive-persistence.el  workspace directive record codec
  mevedel-persistence.el      save/load instructions
  mevedel-session-durability.el lease and storage primitives
  mevedel-session-recovery.el  specialized recovery protocol and markers
  mevedel-session-transfer.el  durable cooperative control transfer protocol
  mevedel-session-publication.el immutable publication and diagnostics
  mevedel-session-save-as.el portable Save As transaction and adoption
  mevedel-session-persistence.el  session save/resume/rewind/fork
  mevedel-session-control-fs.el   pinned target-side session control filesystem
  mevedel-session-control-transfer.el  control-transfer state, drains, descriptors
  mevedel-compact.el          conversation compaction (split-on-compact)

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
  mevedel-tool-fs.el          Read, Glob, Grep
  mevedel-tool-patch.el       ApplyPatch parse/match/apply engine + tool
  mevedel-tool-code.el        XrefReferences, XrefDefinitions, Imenu, Treesitter
  mevedel-tool-exec.el        Bash, Eval
  mevedel-tool-web.el         WebSearch, WebFetch, YouTube
  mevedel-interaction-prompt.el  shared interaction overlay lifecycle
  mevedel-permission-prompt.el   generic, Bash, Eval, and execution-authority prompt UI
  mevedel-tool-ask.el         Ask questionnaire, handler, renderer
  mevedel-tool-ui.el          Agent/InterruptAgent/ToolSearch/SendMessage assembly
  mevedel-tool-task.el        TaskCreate/Update/List/Get + overlay
  mevedel-tool-skills.el      Skill and ListSkills tool schemas
  mevedel-tool-tutor.el       GetHints, RecordHint
  mevedel-tool-introspect.el  wraps gptel-agent introspection tools
  mevedel-tools.el            complete tool registration + deferred-tool machinery
  mevedel-tools-list.el       native tools cockpit list

Support
  mevedel-file-state.el       LRU file cache
  mevedel-diff-apply.el       overlay-preserving diff application
  mevedel-utilities.el        shared helpers (tinting, ediff glue, env info)
```

## External dependencies

- **gptel**, **gptel-agent**, **web-server 0.1.2**, **Emacs >=30.2**, **ediff**, **org-mode**

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
eask test ert test/test-*

# Via npx
npx @emacs-eask/cli test ert test/test-*

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
- `mevedel-create-reference` / `mevedel-create-directive`
- `mevedel-save-instructions` / `mevedel-load-instructions`
- `mevedel-implement-directive` / `mevedel-discuss-directive` /
  `mevedel-request-directive-changes` / `mevedel-retry-directive` /
  `mevedel-tutor-directive`
- `mevedel` / `mevedel-tutoring`
- `mevedel-resume` / `mevedel-rewind` / `mevedel-save-session` /
  `mevedel-rename-session`
- `mevedel-process-directives`, `mevedel-next/previous-instruction`
- `mevedel-diff-apply-buffer` / `mevedel-ediff-patch`
- `mevedel-compact`
- `mevedel-review` / `mevedel-verify`
- `/plan` / `/plan PROMPT` / `mevedel-retry-plan-implementation`
- `/btw` / `/btw PROMPT`
- `mevedel-add/remove/list-project-roots`
- `mevedel-toggle-tasks` / `mevedel-toggle-hints`
- `mevedel-display-hints` / `mevedel-clear-hints`
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
- **Avoid `require` at top level** in library files; prefer
  `declare-function`/`defvar` plus `require` inside functions or
  `eval-when-compile`
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
  `:doc` strings. Rare exceptions (e.g. `test-mevedel-diff-apply.el`)
  where setup differs drastically.
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
