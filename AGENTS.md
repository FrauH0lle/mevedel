# AGENTS.md

## Project Overview

**mevedel** is an Emacs Lisp package that provides a visual workflow for
interacting with LLMs during programming. It enables overlay-based instruction
management for AI-assisted development with direct gptel integration.

## Architecture

### Module layer map

Each `.el` file has its own `;;; Commentary:` block describing its purpose.
Open the file for details.

```
Entry point
  mevedel.el                  top-level loader, install/uninstall, directives

Data model
  mevedel-structs.el          workspace, session, request, agent-invocation
  mevedel-workspace.el        workspace detection and registry
  mevedel-permissions.el      9-step permission decision chain
  mevedel-pipeline.el         tool execution pipeline
  mevedel-tool-registry.el    mevedel-tool struct, mevedel-define-tool macro
  mevedel-reminders.el        system-reminder injection
  mevedel-skills.el           SKILL.md discovery, slash commands

Chat / view
  mevedel-chat.el             session lifecycle
  mevedel-view.el             compact user-facing view buffer
  mevedel-overlays.el         instruction overlays (references/directives)
  mevedel-mentions.el         @ref and @file mention expansion
  mevedel-persistence.el      save/load instructions
  mevedel-preview-mode.el     inline diff preview for Write/Edit
  mevedel-compact.el          conversation compaction

Prompt / presets / agents
  mevedel-system.el           system prompt assembly
  mevedel-presets.el          gptel presets (discuss/implement/revise/tutor)
  mevedel-agents.el           explore/planner/verifier/coordinator definitions

Tools (each dispatches through mevedel-pipeline)
  mevedel-tool-fs.el          Read, Glob, Grep, Write, Edit, MkDir
  mevedel-tool-code.el        XrefReferences, XrefDefinitions, Imenu, Treesitter
  mevedel-tool-exec.el        Bash, Eval
  mevedel-tool-web.el         WebSearch, WebFetch, YouTube
  mevedel-tool-ui.el          Ask, RequestAccess, Agent, SendMessage, ToolSearch
  mevedel-tool-task.el        TaskCreate/Update/List/Get + overlay
  mevedel-tool-plan.el        PresentPlan, CreatePlan
  mevedel-tool-tutor.el       GetHints, RecordHint
  mevedel-tool-introspect.el  wraps gptel-agent introspection tools
  mevedel-tools.el            tool aggregator + deferred-tool machinery

Support
  mevedel-file-state.el       LRU file cache
  mevedel-diff-apply.el       overlay-preserving diff application
  mevedel-utilities.el        shared helpers (tinting, ediff glue, env info)
  mevedel-debug.el            development scaffolding (not a runtime dep)
```

Tool descriptions live in `tools/*.md` and are loaded via
`mevedel-define-tool`'s `:prompt-file` keyword.

### Key data structures (in `mevedel-structs.el` / `mevedel-tool-registry.el`)

- **`mevedel-workspace`**: root, state-dir, additional-roots
- **`mevedel-session`**: workspace, permission-rules, agents, tools
- **`mevedel-request`**: file-snapshots, cancel-fn
- **`mevedel-tool`**: name, handler, args, check-permission, get-path,
  read-only-p, async-p, max-result-size, renderer, groups
- `mevedel--instructions`: buffer -> overlay alist
- `mevedel--id-counter` / `mevedel--id-usage-map`: instruction IDs
- Instruction types: **References** (context) and **Directives** (prompts)

### External dependencies

- **gptel (>=0.9.0)**, **gptel-agent**, **Emacs >=30.1**, **ediff**
- **org-mode** (optional, for foldable prompt blocks)

## Development Commands

### Testing
```bash
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
```
Keep the byte compiler silent: no free-variable or unknown-function warnings.

### Key interactive commands
- `mevedel-create-reference` / `mevedel-create-directive`
- `mevedel-save-instructions` / `mevedel-load-instructions`
- `mevedel-implement-directive` / `mevedel-revise-directive` /
  `mevedel-discuss-directive` / `mevedel-tutor-directive`
- `mevedel` / `mevedel-tutoring`
- `mevedel-process-directives`, `mevedel-next/previous-instruction`
- `mevedel-diff-apply-buffer` / `mevedel-ediff-patch`
- `mevedel-compact`
- `mevedel-add/remove/list-project-roots`
- `mevedel-toggle-todos` / `mevedel-toggle-hints`
- `mevedel-display-hints` / `mevedel-clear-hints`

## Core Concepts

### Tool pipeline

All tools go through `mevedel-pipeline-run-tool`:
```
validate -> check-permission -> snapshot -> handler -> persist oversized result
```
Handlers receive `(callback args)` where args is a keyword plist. The
pipeline handles all cross-cutting concerns; handlers contain no
boilerplate for validation, permissions, snapshots, or persistence.

Tool flags: `:read-only-p`, `:get-path`, `:check-permission`, `:async-p`,
`:max-result-size`, `:renderer`.

`mevedel-define-tool :wrap SOURCE` adopts an existing `gptel-tool` via
`gptel-get-tool` on every call (so upstream changes take effect without
rewrapping). `mevedel-tool-wrap-gptel-category` wraps a whole category.

Tools carry `:groups`. `(:deferred GROUP)` in a preset's or agent's tool
list pulls every tool tagged with GROUP into the session's deferred set.
`mevedel-preset-extra-tool-specs` / `mevedel-agent-extra-tool-specs` add
specs without redefining the preset/agent.

### Permission system

Single decision function `mevedel-check-permission`. Nine-step chain:

1. Extract specifier values via `get-path` / `get-pattern` / `get-domain` /
   `get-name` slots
2. Deny rules
3. Protected paths (`.git/`, `.ssh/`, `.gnupg/`) → ask
4. Tool's own `check-permission` slot
5. Allow rules
6. Inside workspace → allow (implicit)
7. Outside workspace with no covering rule → ask
8. Permission mode
9. Default: ask

Rules live on `mevedel-permission-rules` with form
`(TOOL-NAME &key SPECIFIER VALUE :action ACTION)`. One specifier per rule:

| Key        | Matches                | Used by                           |
|------------|------------------------|-----------------------------------|
| `:path`    | path (glob, `~` exp.)  | Read, Edit, Write, Glob, Grep, ...|
| `:pattern` | command string (glob)  | Bash                              |
| `:domain`  | host name (glob)       | WebFetch, YouTube                 |
| `:name`    | free-form name (glob)  | Agent (subagent_type)             |

Precedence: specifier rules outrank generic; within a group
`deny > ask > allow`; protected paths always prompt.

Modes: `default` / `accept-edits` / `plan` / `trust-all`.

Prompt offers 5 choices (allow/deny × once/session/always). Persisted
rules live in `.mevedel/permissions.el`.

Bash has domain logic in `check-permission`: parses commands, enforces
`mevedel-bash-dangerous-commands` blocklist, fails safe under
`mevedel-bash-fail-safe-on-complex-syntax` on variable expansion /
`eval` / `exec` / here-docs / brace expansion. Bash never returns `ask`
to the pipeline — it prompts internally. Unknown commands default to ask
even under `trust-all`. The dangerous blocklist only downgrades `allow`
to `ask`; explicit `deny`/`ask` wins.

Eval always asks unconditionally; expression shown in prompt subject to
`mevedel-eval-expression-display-limit`.

### Workspace context chain
```
Chat Buffer (authoritative, holds mevedel--workspace)
  |
Derived buffers (diff preview, patch) store mevedel--chat-buffer
  |
Tools execute in chat-buffer context (asserted)
```
File modifications tracked per-request via
`mevedel-request-file-snapshots`.

### gptel integration

Direct via `gptel-request` and `gptel-fsm`. Tools registered in
`gptel--known-tools`. Four presets: `mevedel-discuss` → `implement` →
`revise`; `tutor` inherits from `discuss`. System prompt assembled
dynamically: base + memory + env + workspace config (AGENTS.md/CLAUDE.md).

### Multi-agent system

Agents declared with `mevedel-define-agent`:
- **explore**: read-only investigation, caller-specified thoroughness
- **planner**: interactive planning via `PresentPlan`
- **coordinator**: orchestrates workers via `Agent(run_in_background=true)`;
  never implements
- **verifier**: adversarial read-only verification; per-turn
  `verifier-read-only` reminder attached at invocation

Each agent's `:tools` resolved via `mevedel-tool-resolve-gptel` at
invocation time. Registered buffer-locally via `gptel-agent--agents` per
request (no caching). Each invocation gets a cloned reminder list with
independent `last-fired`.

**Background spawning.** `run_in_background` makes `mevedel-tools--task`
call `process-tool-result` immediately with a launch-status string,
unblocking the parent FSM. The sub-agent completes fire-and-forget; its
result is wrapped in `<agent-result>` and pushed to the parent's mailbox.
When the LLM produces no tool calls but background agents are still
running, the FSM parks in **BWAIT** instead of terminating. Completion
resumes BWAIT→WAIT. Transition injection:
`mevedel-preset--inject-bwait-transitions` (main) and
`mevedel-tools--inject-bwait-transition` (sub-agent). `background-agents`
slot on session/invocation tracks running children.

Foreground-callback suppression: when a foreground agent has background
children, `mevedel-tools--task` stashes the result on the invocation's
`stashed-result` slot; `main-cb` is called once all children finish.

### Inter-agent messaging (SendMessage)

Fire-and-forget async messages. Aliases `"main"`, `"chat"`, `"coordinator"`
resolve to the main session mailbox; exact agent-id or `"<type>--"` prefix
match resolves to a sub-agent. Messages queue on the recipient's mailbox
and drain via `mevedel-tools--handle-message-inject` in WAIT state, wrapped
as `<agent-message from="SENDER">...</agent-message>` and injected as a
user turn via `gptel--inject-prompt`. Polymorphic accessor
`mevedel-tools--ctx-messages` dispatches on session vs invocation.

### Coordinator skill

Bundled at `skills/coordinator/SKILL.md` (discovered via
`mevedel-skills--bundled-dir`). `context: fork` delegates to the
coordinator agent. User skills in `~/.claude/skills/` or
`.mevedel/skills/` override bundled by name.

### Inline diff preview

Entry point: `mevedel-preview-mode-add-preview` (keyword API). Dispatches
on `mevedel-preview-mode--effective-mode`:
- `default` / `plan` → interactive inline overlay
- `accept-edits` / `trust-all` → `--auto-apply` (runs `apply-fn`
  immediately, still produces a persistent diff summary in the view)

`mevedel-preview-mode` is a buffer-local minor mode with a lighter
` Preview[N]`. Prefix `C-c p`: `n`/`p` navigate, `a` approve-all,
`r` reject-all. Per-overlay: approve (`C-c C-c`/`a`/`RET`), reject
(`C-c C-k`/`r`/`q`), ediff (`C-c C-e`/`e`), feedback (`C-c C-f`/`f`),
trust-rest (`S`), toggle (`TAB`).

`S` approves all pending overlays and escalates permission mode to
`accept-edits` (not `trust-all` — shell commands still prompt). Registering
a preview installs `mevedel-preview-mode-dismiss-all` as the active
request's `cancel-fn`, so `mevedel-abort` tears everything down cleanly.

Handler return shape: callbacks fire with plist
`(:result STR :render-data (:kind diff :patch PATCH :path PATH :rel-path REL))`.
The pipeline splits `:result` (LLM-facing) from `:render-data`
(LLM-invisible side channel). Plain strings still work for legacy handlers.

### Tool renderers

Individual tools may ship a `:renderer FN` for rich collapsible views in
the view buffer. Contract:
`(lambda (NAME ARGS RESULT RENDER-DATA) -> rendering-plist-or-nil)`.
Pure function — no I/O, no mutation. Nil falls back to
`mevedel-view--tool-one-liner`.

Rendering plist: `(:header STRING :body STRING :body-mode SYMBOL
:initially-collapsed-p BOOL)`. Validated by
`mevedel-view--rendering-plist-p`.

Render-data side channel: when a handler returns
`(:result STR :render-data DATA)`, the pipeline writes `:result` to the
data buffer and appends a hidden block wrapped in
`<!-- mevedel-render-data -->` delimiters, propertized
`'gptel 'ignore` and `'invisible t`. Parser:
`mevedel-pipeline-extract-render-data`.

The view is a pure function of the data buffer — re-parsed on every
rerender, no cached overlay state.
`mevedel-view--invoke-renderer` `condition-case`s the call; malformed
output emits a warning and falls through to the one-liner.

Wrapped tools (gptel/MCP) always have `render-data` = nil; their
renderer must parse the result string.

### Tutor mode

Socratic guidance, NEVER direct solutions. Workflow:
1. `GetHints()` at start
2. Tutor via questioning / hints / docs / decomposition
3. `RecordHint()` per hint given

Hints persist in `.mevedel/hints.md` per concept and buffer-locally.
`mevedel-tutor` preset enables this. Commands:
`mevedel-display-hints`, `mevedel-clear-hints`.

### @-mention system (@ref, @file, @agent, @mcp)

Expansion runs as a gptel prompt transform (priority -90) via
`mevedel--transform-expand-mentions`, dispatching through
`mevedel-mention-handlers`. Each mention becomes a compact
`[kind:KEY -- STATUS]` placeholder with full content injected as a
`<system-reminder>` block above the user prompt.

- **@ref:N** / **@ref:{tag query}** — refs by ID or tag
- **@file:path** — hierarchical file completion; optional `#L<start>[-<end>]`
  pins a line range (not recorded in touched-files, since LLM may still
  need other parts). Directories return a gitignore-filtered recursive
  listing (`rg --files --hidden --follow --sort path`) capped at
  `mevedel-file-mention-directory-max-entries` (default 1000). Contents
  read via `mevedel-tool-fs--slurp-file-contents` (512 KB cap, line
  numbers). Runs `mevedel-check-permission "Read"` first — any non-allow
  yields "permission denied". Directories, unreadable files, and binary
  extensions rejected.
- **@agent:name** — asks main agent to delegate via
  `Agent(subagent_type="NAME", ...)` (looked up in `mevedel-agent--registry`)
- **@mcp:server:uri** — attaches an MCP resource via mcp.el
  (`mcp-hub-get-servers`, `mcp-server-connections`, `mcp-read-resource`).
  URI capture is greedy past internal colons so `file:///...` works.
  mcp.el is optional (declared via `declare-function`).

Every rejection branch emits a follow-up `<system-reminder>` telling the
LLM the bracketed placeholder is a system annotation, not user text.

Dedup:
- Per-session: `mevedel-session-mentions-shown` keyed on `(KIND . KEY)`
  stores `(turn . content-hash)`; unchanged hashes skip re-injection
- Read dedup: `@file` records reads on `mevedel-session-touched-files`
  so later Read calls short-circuit

Completion: `mevedel-ref-capf`, `mevedel-file-capf`, `mevedel-agent-capf`,
`mevedel-mcp-capf` (two-stage: server names at `@mcp:`, resource URIs at
`@mcp:server:`). Font-lock uses `success`/`shadow`/`link` box faces.
Registered in `mevedel-install`/`-uninstall`.

### Conversation compaction

`mevedel-compact` finds the compaction boundary (end of last LLM
response), summarizes old content, marks it `gptel 'ignore` dimmed with
`shadow`, and inserts a folded summary. Token estimation: chars / 4,
excluding ignore regions. Header-line `mevedel--token-header-segment`
shows context usage at >80%. Cannot compact during an active request.

### Persistent memory

`.mevedel/memory/MEMORY.md` under workspace root; first 200 lines
included in every system prompt via `mevedel-system--memory-prompt`.
Main file can link topic files. LLM-writable.

### Project instructions (AGENTS.md / CLAUDE.md)

`mevedel-system-build-prompt` checks workspace root for `AGENTS.md`
first, then `CLAUDE.md`, and appends the contents as
`## Workspace Configuration` in the system prompt.

### Task overlay

Tasks tracked per caller (main chat and each sub-agent separately).
`blockedBy` propagates completion. `mevedel-tools--agents-fsm`
(buffer-local on chat buffer) maps agent-id → sub-agent FSM for
SendMessage resolution.

### Tool result persistence

When `:max-result-size` is set and result exceeds the effective limit
(min of tool value and 50,000-char global cap), the full result is saved
to `.mevedel/tool-results/` and replaced with a preview wrapped in
`<persisted-output>` XML. The LLM can `Read` the file to see the full
output. Error results (`"Error:"` prefix) are never persisted. No
workspace → no persistence.

Per-tool limits match Claude Code's approach: Grep 20k, Bash/Eval 30k,
Glob 30k, Xref*/Imenu 20k, Treesitter 30k, Agent 50k, WebFetch/YouTube
50k. Read/Write/Edit/MkDir/Ask: nil (self-bounded or short).

### Chat buffer formatting

Optional org-mode for foldable prompt blocks (`:PROMPT:` drawers, folded
by default). Markdown-mode gets foldable code blocks. Tool results
containing `:PROPERTIES:` are escaped with `,` to prevent nested-drawer
confusion.

### Bash permission example

```elisp
(setq mevedel-permission-rules
      '(("Bash" :action ask)                       ; default ask
        ("Bash" :pattern "echo"     :action allow)
        ("Bash" :pattern "echo *"   :action allow)
        ("Bash" :pattern "ls"       :action allow)
        ("Bash" :pattern "ls *"     :action allow)
        ("Bash" :pattern "git log*" :action allow)
        ("Bash" :pattern "rm *"     :action deny)))

(setq mevedel-bash-dangerous-commands
      '("rm" "sudo" "dd" "chmod" "curl" "wget" "ssh"))
```

Use space-boundary patterns (`"ls"` + `"ls *"`) rather than `"ls*"` to
avoid matching `lsof`. Parseable: simple commands, chains (`&&`/`||`/`;`),
pipes, command substitutions (incl. nested), sudo/env/nice prefixes.
Fails safe: variable expansion (`$VAR`), `eval`/`exec`, here-docs, brace
expansion, unbalanced quotes.

## Development Guidelines

### Redesign specs
Phase overview and dependency graph live in `specs/README.md`.

### Code style

- **Lexical binding**: `;;; file.el -- Description -*- lexical-binding: t -*-`
- **Headers**: standard `;;; Commentary:` / `;;; Code:` sections
- **Section headers**: two blank lines above. Major: `;;` + blank + `;;;`.
  Subsections add more semicolons: `;;;;`, `;;;;;`, ...
- **Forward declarations**: grouped at file top by source package with
  `;; \`gptel'` style comment headers. Use `declare-function` / `defvar`.
- **Customization**: `defcustom` uses `:group 'mevedel`
- **Private symbols**: double-dash `--` (e.g. `mevedel--workspace`,
  `mevedel-tools--validate-params`)
- **Provide**: each file ends with `(provide 'mevedel-MODNAME)` and
  `;;; mevedel-MODNAME.el ends here`
- **Avoid `require` at top level** in library files; prefer
  `declare-function`/`defvar` plus `require` inside functions or
  `eval-when-compile`

### Testing conventions

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
- **New functions need tests**; modify tests when behavior changes

### Byte compilation

- No free-variable or unknown-function warnings
- `declare-function` for external functions, `defvar` for external vars
- `eval-when-compile` for compile-time-only deps like `cl-lib`
- Run `npx @emacs-eask/cli compile` before committing
