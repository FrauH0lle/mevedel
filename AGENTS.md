# AGENTS.md

## Project Overview

**mevedel** is an Emacs Lisp package that provides a visual workflow for
interacting with LLMs during programming. It enables overlay-based
instruction management for AI-assisted development with direct gptel
integration.

## Documentation map

This file is the entry point. Detail docs live in `docs/` and are loaded
lazily — read them when planning work in the relevant area. The
`docs/` tree is the maintained working documentation.

- [`docs/architecture.md`](docs/architecture.md) — key data structures
  (`mevedel-workspace`, `-session`, `-request`, `-tool`), workspace
  context chain, gptel integration, persistent memory layout, chat
  buffer formatting
- [`docs/view.md`](docs/view.md) — dual-buffer view model, status /
  interaction / input zones, rendered agent transcript views, input
  history
- [`docs/tools.md`](docs/tools.md) — tool pipeline
  (validate → permission → snapshot → handler → persist), `:wrap` /
  `:groups`, renderers and render-data side channel, oversized result
  persistence
- [`docs/permissions.md`](docs/permissions.md) — 9-step decision chain,
  bucket precedence, plan-mode exception, Bash/Eval specifics,
  sub-agent permission propagation, example config
- [`docs/agents.md`](docs/agents.md) — explorer/coordinator/
  verifier/reviewer, background spawning + BWAIT, SendMessage mailboxes,
  coordinator and review skills, task overlay
- [`docs/preview.md`](docs/preview.md) — inline diff overlay,
  keybindings, mode dispatch, handler return shape
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
- [`docs/sessions.md`](docs/sessions.md) — on-disk layout, segment
  persistence contract, resume/rewind/fork, locking, auto-cleanup,
  defcustoms
- [`docs/compaction.md`](docs/compaction.md) — manual and automatic
  conversation compaction, token thresholds, gptel token baseline,
  anchored summaries, tail preservation, segment integration
- [`docs/tutor.md`](docs/tutor.md) — Socratic tutor workflow, hint
  persistence
- [`docs/commits.md`](docs/commits.md) — commit message format and
  guidelines
- [`docs/tech-debt-tracker.md`](docs/tech-debt-tracker.md) — known
  shortcuts and deferred cleanups; consult before planning non-trivial
  work in any listed area
- [`docs/deferred-tracker.md`](docs/deferred-tracker.md) — broader
  backlog of deferred features and follow-up implementation ideas

Each `.el` file has its own `;;; Commentary:` block describing its
purpose. Open the file for details.

## Module layer map

```
Entry point
  mevedel.el                  top-level loader, install/uninstall, directives

Data model
  mevedel-structs.el          workspace, session, request, agent-invocation
  mevedel-workspace.el        workspace detection and registry
  mevedel-models.el           model tier/provider resolution
  mevedel-hooks.el            project/user/skill/agent hook loading + runner
  mevedel-permissions.el      9-step permission decision chain
  mevedel-pipeline.el         tool execution pipeline
  mevedel-tool-registry.el    mevedel-tool struct, mevedel-define-tool macro
  mevedel-queue.el            shared FIFO queue machinery
  mevedel-permission-queue.el permission/Bash/Eval queue
  mevedel-reminders.el        system-reminder injection
  mevedel-skills.el           SKILL.md discovery, slash commands

Chat / view
  mevedel-chat.el             session lifecycle
  mevedel-transcript.el       transcript span classification for view/persistence/compaction
  mevedel-view.el             compact user-facing view buffer
  mevedel-view-fragment.el    identity-backed view chrome blocks
  mevedel-view-history.el     view input history ring and persistence
  mevedel-overlays.el         instruction overlays (references/directives)
  mevedel-mentions.el         @ref and @file mention expansion
  mevedel-persistence.el      save/load instructions
  mevedel-session-persistence.el  session save/resume/rewind/fork
  mevedel-preview-mode.el     inline diff preview for Write/Edit
  mevedel-compact.el          conversation compaction (split-on-compact)

Prompt / presets / agents
  mevedel-system.el           system prompt assembly
  mevedel-presets.el          gptel presets (discuss/implement/revise/tutor)
  mevedel-agents.el           explorer/verifier/coordinator/reviewer definitions
  mevedel-agent-exec.el       sub-agent task runner, FSM handlers, registry
  mevedel-review.el           /review picker, reviewer output parsing, parent transcript injection

Tools (each dispatches through mevedel-pipeline)
  mevedel-tool-fs.el          Read, Glob, Grep, Write, Edit, MkDir
  mevedel-tool-code.el        XrefReferences, XrefDefinitions, Imenu, Treesitter
  mevedel-tool-exec.el        Bash, Eval
  mevedel-tool-web.el         WebSearch, WebFetch, YouTube
  mevedel-tool-ui.el          Ask, RequestAccess, Agent, SendMessage, ToolSearch
  mevedel-tool-task.el        TaskCreate/Update/List/Get + overlay
  mevedel-tool-plan.el        conversational Plan mode and plan approval queue
  mevedel-tool-tutor.el       GetHints, RecordHint
  mevedel-tool-introspect.el  wraps gptel-agent introspection tools
  mevedel-tools.el            tool aggregator + deferred-tool machinery

Support
  mevedel-file-state.el       LRU file cache
  mevedel-diff-apply.el       overlay-preserving diff application
  mevedel-utilities.el        shared helpers (tinting, ediff glue, env info)
```

## External dependencies

- **gptel**, **gptel-agent**, **Emacs >=30.1**, **ediff**, **org-mode**

## gptel and gptel-agent source rule

mevedel is tightly coupled to gptel and also depends on gptel-agent. Before
implementing or changing behavior that touches prompts, requests, callbacks,
tool calls, presets, buffers, transcripts, session flow, agents, or
coordination, consult gptel and gptel-agent source and reuse their existing
APIs or patterns instead of duplicating them.

Prefer a refreshed upstream checkout, because Eask dependency installs can get
stale:

```bash
# First time
mkdir -p .scratch/upstream
git clone https://github.com/karthink/gptel .scratch/upstream/gptel
git clone https://github.com/karthink/gptel-agent .scratch/upstream/gptel-agent

# Refresh before consulting
git -C .scratch/upstream/gptel pull --ff-only
git -C .scratch/upstream/gptel-agent pull --ff-only
```

If you need to inspect the package currently visible to Eask/Emacs, refresh
Eask first and load `find-func`. This answers "what is on this load-path", not
"what is latest upstream":

```bash
# Optional refresh
npx @emacs-eask/cli upgrade gptel gptel-agent

# Locate installed source
npx @emacs-eask/cli emacs --batch \
  --eval '(require (quote find-func))' \
  --eval '(let ((file (find-library-name "gptel"))) (princ file) (terpri))'
npx @emacs-eask/cli emacs --batch \
  --eval '(require (quote find-func))' \
  --eval '(let ((file (find-library-name "gptel-agent"))) (princ file) (terpri))'
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
use real temp files/directories rather than mocking.

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
- `mevedel-implement-directive` / `mevedel-revise-directive` /
  `mevedel-discuss-directive` / `mevedel-tutor-directive`
- `mevedel` / `mevedel-tutoring`
- `mevedel-resume` / `mevedel-rewind` / `mevedel-save-session` /
  `mevedel-rename-session`
- `mevedel-process-directives`, `mevedel-next/previous-instruction`
- `mevedel-diff-apply-buffer` / `mevedel-ediff-patch`
- `mevedel-compact`
- `mevedel-review` / `mevedel-verify`
- `mevedel-add/remove/list-project-roots`
- `mevedel-toggle-todos` / `mevedel-toggle-hints`
- `mevedel-display-hints` / `mevedel-clear-hints`

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
- **Naming**: `test/test-mevedel-{module}.el` matches source
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
