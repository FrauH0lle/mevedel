# AGENTS.md

## Project Overview

**mevedel** is an Emacs Lisp package that provides a visual workflow for interacting with LLMs during programming. It enables overlay-based instruction management for AI-assisted development with direct gptel integration.

## Architecture

### Core Components

1. **mevedel.el** (~471 lines): Main entry point and gptel integration
   - Installation/uninstallation system
   - Direct gptel-request integration
   - LLM action processing (implement, revise, discuss, tutor directives)
   - Prompt generation and workspace management
   - Optional org-mode support for chat buffer formatting
   - `@ref` expansion hook registration
   - Token header integration for compaction

2. **mevedel-chat.el** (~756 lines): Chat session lifecycle
   - Chat buffer creation and setup
   - Session initialization (workspace, tools, presets, agents)
   - Multi-session support with session switching
   - Plan implementation (full context and clear context)
   - Plans directory management

3. **mevedel-overlays.el** (~2212 lines): Core overlay system
   - Instruction overlay creation, modification, and deletion
   - Tag-based categorization and navigation
   - Visual styling and linking system
   - ID-based instruction linking

4. **mevedel-mentions.el** (~471 lines): Mention system
   - `@ref` mention system: parsing, expansion, completion, and font-lock
   - `@file:` mention system: hierarchical file completion in chat buffers
   - Completion-at-point functions for both mention types

5. **mevedel-structs.el** (~223 lines): Domain data structures
   - `mevedel-workspace` struct: root, state-dir, additional-roots
   - `mevedel-session` struct: workspace, permission-rules, agents, tools
   - `mevedel-request` struct: file-snapshots, cancel-fn
   - Session and request lifecycle functions (create, end, cleanup)

6. **mevedel-permissions.el** (~400 lines): Unified permission system
   - 9-step decision chain: extract context -> deny rules -> protected paths -> tool check-permission -> allow rules -> workspace root -> outside workspace -> mode -> default ask
   - `mevedel-permission-rules` defcustom for path-based and tool-wide rules
   - `mevedel-protected-paths` defcustom (defaults: `.git/`, `.ssh/`, `.gnupg/`)
   - `mevedel-permission-mode` defcustom: default, accept-edits, plan, trust-all
   - Session-scoped and persistent rule storage (`.mevedel/permissions.el`)
   - Rule matching with glob patterns, precedence: deny > ask > allow

7. **mevedel-pipeline.el** (~270 lines): Tool execution pipeline
   - 4-step standard pipeline: validate -> check-permission -> snapshot -> handler
   - Flag-based step skipping (read-only-p, get-path, async-p)
   - Error conditions: `mevedel-pipeline-error`, `mevedel-permission-denied`, `mevedel-validation-error`
   - Cancellation via cancel-fn on request struct
   - Confirm step deferred to spec 12 (preview-mode); handlers invoke confirmation directly

8. **mevedel-tool-registry.el** (~491 lines): Tool registration and structs
   - `mevedel-tool` struct: name, handler, args, check-permission, get-path, read-only-p, async-p
   - `mevedel-define-tool` macro: creates mevedel-tool struct + gptel-tool, wires handler through pipeline
   - `mevedel-tools--validate-params` macro for legacy tool validation
   - Tool group lists used by presets and agents
   - Prompt file loading from `tools/*.md`

9. **mevedel-tool-fs.el** (~682 lines): File system tools
   - Read, Glob, Grep (read-only), Write, Edit, MkDir (modifying)
   - File snapshots for undo tracking
   - String replacement with unique/replace-all modes
   - Binary file detection, blocked device paths
   - Line number formatting, result truncation

10. **mevedel-tool-code.el** (~396 lines): Code exploration tools
    - XrefReferences, XrefDefinitions (cross-reference navigation)
    - Imenu (buffer symbol index)
    - Treesitter (AST-based code structure)
    - All read-only with get-path for permission scoping

11. **mevedel-tool-exec.el** (~740 lines): Execution tools
    - Bash: command parsing, permission system, output capture with timeout
    - Eval: elisp expression evaluation with stdout capture
    - Bash check-permission: parses commands, prompts with overlay showing actual command
    - Eval check-permission: always asks, displays expression (toggleable for long expressions)
    - `mevedel-eval-expression-display-limit` defcustom for prompt truncation

12. **mevedel-tool-web.el** (~99 lines): Web tools
    - WebSearch (DuckDuckGo via eww), WebFetch, YouTube
    - All read-only, delegate to gptel-agent backend functions

13. **mevedel-tool-ui.el** (~1098 lines): User interaction tools
    - Ask (user questions), RequestAccess (directory access prompts)
    - Agent (sub-agent spawning), ToolSearch (deferred tool loading)
    - TodoWrite/TodoRead (legacy gptel-make-tool, pending replacement by task system)
    - Permission prompt overlay system (5-choice: allow-once, allow-session, always-allow, deny-once, deny-session)
    - Todo overlay display with multi-context tracking

14. **mevedel-tool-plan.el** (~315 lines): Planning tools
    - PresentPlan: interactive plan display with keybindings (implement, implement-clear, feedback, abort)
    - CreatePlan: delegates to planner agent
    - Post-tool-call intercept for triggering plan implementation

15. **mevedel-tool-tutor.el** (~537 lines): Tutoring tools
    - GetHints: retrieves hint history from file + session
    - RecordHint: persists hints to file and buffer-local storage
    - Hint file I/O (`.mevedel/hints.md`, markdown format)
    - Hint overlay display with depth tracking
    - Interactive commands: `mevedel-display-hints`, `mevedel-clear-hints`, `mevedel-toggle-hints`

16. **mevedel-preview-mode.el** (~591 lines): Inline diff preview
    - Inline diff display with approve/reject/edit/feedback workflow
    - Size threshold for inline vs. separate buffer
    - Ediff integration for manual editing
    - Multi-preview navigation

17. **mevedel-tools.el** (~237 lines): Legacy tool support
    - ToolSearch implementation (deferred tool loading)
    - Tool group list definitions used by presets and agents

18. **mevedel-workspace.el** (~253 lines): Multi-workspace support
    - Workspace struct creation from project roots
    - State directory management (`.mevedel/`)
    - Additional root tracking for cross-project access

19. **mevedel-presets.el** (~260 lines): gptel preset configuration
    - Four presets: discuss (read-only), implement (editing), revise (with context), tutor (tutoring assistant)
    - Preset inheritance via `:parents` (implement inherits discuss, revise inherits implement, tutor inherits discuss)
    - FSM termination handlers for cleanup
    - Dynamic agent registration (buffer-local via `gptel-agent--agents`)

20. **mevedel-agents.el** (~326 lines): Specialized agent definitions
    - Four specialized agents: codebase-analyst, researcher, planner, introspector
    - Agent-specific tool filtering and system prompts
    - Integration with gptel-agent for multi-agent workflows

21. **mevedel-system.el** (~341 lines): System prompt generation
    - Tone prompt (code style, terseness, accuracy)
    - Base system prompt with task execution protocol and delegation rules
    - Persistent memory prompt (loads `.mevedel/memory/MEMORY.md`)
    - System prompt builder: assembles base + memory + environment + workspace config
    - AGENTS.md / CLAUDE.md workspace configuration loading
    - Tutor assistant prompt (GetHints, RecordHint)
    - Pattern-based delegation guidance

22. **mevedel-persistence.el** (~341 lines): Instruction persistence
    - Save/load instructions to/from files
    - Version management and file association
    - Automatic patching for outdated files

23. **mevedel-diff-apply.el** (~656 lines): Advanced patch application
    - Overlay-preserving diff application
    - File creation/deletion support
    - Minimal change region detection

24. **mevedel-compact.el** (~314 lines): Conversation compaction
    - Token estimation (character count / 4, excluding ignored regions)
    - Compaction boundary detection (finds end of last LLM response)
    - LLM-based summarization of old conversation content
    - Compact apply: marks old content as ignored, dims with shadow face, inserts folded summary block
    - Token header segment for gptel header-line (shows context usage warning)

25. **mevedel-utilities.el** (~652 lines): Utility functions
    - Color tinting for overlays
    - Ediff integration for patch review
    - Text manipulation and formatting utilities
    - Environment info string generation

26. **mevedel-skills.el** (~800 lines): Skills and slash commands
    - `mevedel-skill` struct with full ccs frontmatter fields (name, description, when-to-use, context, agent, allowed-tools, path-patterns, etc.)
    - SKILL.md discovery under `~/.claude/skills/`, `.claude/skills/`, `.mevedel/skills/` (configurable via `mevedel-skill-dirs`)
    - Metadata-only scan via `gptel-agent-read-file` + lazy body loading on invocation
    - `Skill` tool with inline and fork execution modes (fork delegates to `mevedel-tools--task` sub-agent)
    - Variable substitution: `$ARGUMENTS`, `$N`, `${CLAUDE_SESSION_ID}`, `${CLAUDE_SKILL_DIR}`
    - Shell injection: inline `` !`cmd` `` and fenced ` ```! ` blocks
    - Local slash commands: `/tokens`, `/model`, `/compact`, `/mode`, `/clear`, `/help`
    - `:before-while` advice on `gptel-send` for slash-command dispatch (local runs + aborts, skill expands + proceeds, unknown warns + aborts)
    - `completion-at-point` for `/` prefix (local commands and skill names, annotated)
    - Skills-listing reminder (budget-capped to 1% of context window, 250 chars/entry, excludes disabled and dormant skills)
    - Conditional activation: skills with `path-patterns` start dormant; a buffer-local post-tool-call hook flips `active-p` when a tool touches a matching file (path extracted via the tool's `get-path` slot)

### Tool Prompt Files

Tool descriptions are stored as external markdown files in `tools/`:
`agent.md`, `ask.md`, `bash.md`, `createplan.md`, `edit.md`, `eval.md`,
`gethints.md`, `glob.md`, `grep.md`, `imenu.md`, `mkdir.md`,
`presentplan.md`, `read.md`, `recordhint.md`, `requestaccess.md`,
`toolsearch.md`, `treesitter.md`, `webfetch.md`, `websearch.md`,
`write.md`, `xref-definitions.md`, `xref-references.md`, `youtube.md`

These are loaded by `mevedel-define-tool` via the `:prompt-file` keyword and
appended to the tool's description for LLM context.

### Key Data Structures

- **`mevedel-workspace`** struct: root, state-dir, additional-roots (defined in `mevedel-structs.el`)
- **`mevedel-session`** struct: workspace, permission-rules, agents, tools (defined in `mevedel-structs.el`)
- **`mevedel-request`** struct: file-snapshots, cancel-fn (defined in `mevedel-structs.el`)
- **`mevedel-tool`** struct: name, handler, args, check-permission, get-path, read-only-p, async-p (defined in `mevedel-tool-registry.el`)
- `mevedel--instructions`: Main instruction overlay storage (alist: buffer -> overlays)
- `mevedel--id-counter` / `mevedel--id-usage-map`: Instruction ID management for linking
- **Instruction types**: References (provide context) and Directives (LLM prompts)

### External Dependencies

- **gptel (>=0.9.0)**: Direct LLM integration backend
- **gptel-agent**: Multi-agent workflow support (Agent tool, delegation infrastructure)
- **Emacs >=30.1**: Required for modern features
- **ediff**: For patch review functionality
- **org-mode**: Optional, for foldable prompt blocks in chat buffers

## Development Commands

### Emacs Lisp Development

### Testing
```bash
# Run unit tests with ERT using Eask (if Eask is installed)
eask test ert test/test-*

# Run tests with Eask via npx (if Eask is not installed)
npx @emacs-eask/cli test ert test/test-*
```

#### Testing Infrastructure Features
- ERT (Emacs Lisp Regression Testing) framework for all tests
- `mevedel-deftest` macro (in `test/helpers.el`) for template-based test generation
- Uses real temporary files instead of mocking for more realistic tests
- Automatic cleanup of test buffers and temporary files
- Overlay creation utilities for testing mevedel instruction overlays
- Proper diff generation for testing mevedel-diff-apply-buffer

#### Test Files
- `test-mevedel-structs.el`: Workspace, session, and request struct lifecycle
- `test-mevedel-permissions.el`: Permission decision chain, rules, modes, session rules
- `test-mevedel-pipeline.el`: Pipeline step execution, error handling, step skipping
- `test-mevedel-tool-registry.el`: Tool registration, struct creation, group management
- `test-mevedel-tool-fs.el`: Read, Glob, Grep, Write, Edit, MkDir handlers
- `test-mevedel-tool-code.el`: Xref, Imenu, Treesitter handlers
- `test-mevedel-tools-bash-permissions.el`: Bash permission system, command parsing, Eval permissions
- `test-mevedel-tools-edit.el`: Edit tool string replacement and diff logic
- `test-mevedel-tools-validation.el`: `mevedel-tools--validate-params` macro
- `test-mevedel-diff-apply.el`: Extensive overlay-preserving diff application tests
- `test-mevedel-compact.el`: Token estimation and file-local-variables detection
- `test-mevedel-hints.el`: Tutor hint file I/O and display
- `test-mevedel-utilities.el`: Tag query prefix-from-infix conversion

### Key Interactive Commands
- `mevedel-create-reference` / `mevedel-create-directive`: Create instructions
- `mevedel-save-instructions` / `mevedel-load-instructions`: Persistence
- `mevedel-implement-directive` / `mevedel-revise-directive` / `mevedel-discuss-directive` / `mevedel-tutor-directive`: LLM processing
- `mevedel` / `mevedel-tutoring`: Start chat / tutoring chat sessions
- `mevedel-process-directives`: Process multiple directives sequentially
- `mevedel-next-instruction` / `mevedel-previous-instruction`: Navigation
- `mevedel-diff-apply-buffer` / `mevedel-ediff-patch`: Apply / edit patches with overlay preservation
- `mevedel-compact`: Summarize old conversation to reduce token usage
- `mevedel-add-project-root` / `mevedel-remove-project-root` / `mevedel-list-project-roots`: Workspace root management
- `mevedel-toggle-todos` / `mevedel-toggle-hints`: Toggle overlay visibility in chat buffer
- `mevedel-display-hints` / `mevedel-clear-hints`: Display/clear tutor hints for project

## Important Patterns

### Overlay-Based Architecture
- Uses Emacs overlays for non-intrusive instruction marking
- Visual styling through customizable color tinting
- State tracking through overlay properties
- Overlay preservation during patch application

### Tag System
- Sophisticated boolean query system (and, or, not operators)
- References use tags for categorization
- Directives query references by tags

### Tool Registration and Pipeline

All tools are registered with `mevedel-define-tool`, which creates both a
`mevedel-tool` struct and a `gptel-tool`. The gptel-tool's `:function` slot
is a wrapper that runs the tool through the pipeline:

```
validate -> check-permission -> snapshot -> handler
```

Tool handlers receive `(callback args)` where args is a keyword plist.
The pipeline handles validation, permissions, and snapshots — handlers
contain zero boilerplate for these concerns.

Key flags on `mevedel-tool` struct:
- `:read-only-p` — skips snapshot step
- `:get-path` — lambda extracting path from args for permission scoping
- `:check-permission` — `(tool-struct input) -> allow|deny|ask|nil` for domain-specific permission logic
- `:async-p` — handler receives callback as first arg

Tool descriptions are split: short `:description` for the tool call schema,
detailed instructions in `tools/*.md` via `:prompt-file`.

### Unified Permission System

Replaces scattered per-tool `check-directory-permissions` calls with a
single `mevedel-check-permission` decision function, called as a pipeline step.

**9-step decision chain:**
1. Extract path via tool's `get-path`
2. Check deny rules -> deny
3. Check protected paths (.git/, .ssh/, .gnupg/) -> ask
4. Call tool's `check-permission` if present -> use result or continue
5. Check allow rules -> allow
6. Check workspace root (implicit allow for paths inside) -> allow
7. Path outside workspace with no covering rule -> ask
8. Check permission mode -> allow/ask/deny per mode
9. Default: ask

**Precedence:** deny > ask > allow. Protected paths always prompt.

**Permission modes:** `default` (prompt for edits/bash), `accept-edits`
(auto-approve file changes), `plan` (deny writes), `trust-all` (skip
prompts except dangerous/protected).

**Interactive prompt:** 5 choices — allow-once, allow-session, always-allow,
deny-once, deny-session. Rules stored on session struct or persisted to
`.mevedel/permissions.el`.

**Bash permissions:** Domain-specific logic in `check-permission` slot.
Parses commands, checks dangerous blocklist, prompts with actual command
displayed. Never returns `ask` — handles prompting internally.

**Eval permissions:** Always asks unconditionally. Displays expression in
prompt (toggleable for long expressions via `mevedel-eval-expression-display-limit`).

### Workspace Management (Critical for Multi-Project Use)
- **Workspace struct**: `mevedel-workspace` with root, state-dir, additional-roots
- **Session struct**: `mevedel-session` with workspace, permission-rules, agents, tools
- **Request struct**: `mevedel-request` with file-snapshots, cancel-fn
- **Buffer-local workspace context**: Each chat buffer has `mevedel--workspace` set
- **Authoritative chat buffer**: Tools execute in chat buffer context (validated with assertions)
- **Explicit buffer parameters**: Workspace query functions accept optional buffer parameter
- **Delegation pattern**: Derived buffers (diff preview, patch) store `mevedel--chat-buffer` reference
- **File snapshots**: Tool modifications tracked per request via `mevedel-request-file-snapshots`

### gptel Integration
- Direct integration via `gptel-request` and `gptel-fsm`
- Custom tools registered in `gptel--known-tools`
- Four presets: `mevedel-discuss`, `mevedel-implement`, `mevedel-revise`, `mevedel-tutor`
- Preset inheritance: implement inherits discuss, revise inherits implement, tutor inherits discuss
- Tool results properly escaped for org-mode compatibility
- FSM termination handlers for cleanup and fixup
- System prompt assembled dynamically: base prompt + persistent memory + environment info + workspace config (AGENTS.md/CLAUDE.md)

### Multi-Agent System
- **Four specialized agents** for focused tasks:
  - **codebase-analyst**: Deep architectural analysis, pattern recognition, dependency mapping (all read tools, NO web access)
  - **researcher**: Online research and documentation discovery (web tools + Read/Grep for cross-referencing)
  - **planner**: Interactive implementation planning with PresentPlan tool (all read tools + PresentPlan)
  - **introspector**: Elisp/Emacs introspection and debugging (Eval tool + Ask + RequestAccess) - from gptel-agent
- **Agent tool filtering**: Each agent has specific tool access based on its responsibilities
- **Delegation rules**: System prompt guides main agent when to delegate to specialists
- **Dynamic registration**: Agents registered buffer-locally via `gptel-agent--agents`
- **Pattern-based delegation**:
  - "how does X work?" -> codebase-analyst
  - "find documentation for Y" -> researcher
  - "create a plan for Z" -> planner
  - "I need to understand..." about elisp/Emacs -> introspector

### PresentPlan Tool (Interactive Planning)
- **Purpose**: Presents implementation plans for user feedback in chat buffer
- **Async interaction**: Blocks agent execution until user responds
- **User actions**: Accept (proceed), Reject (with feedback), Modify (specific sections)
- **Iteration support**: Planner agent can call PresentPlan multiple times to refine plan
- **Plan structure**: Title, summary, and sections (types: step, risk, alternative, dependency)

### Todo Overlay Display
- **Multi-context**: Tracks todos per caller (main agent and each sub-agent separately)
- **Display**: Overlay in chat buffer with icons: completed (strikethrough), in_progress (bold), pending
- **Toggle**: `mevedel-toggle-todos` or `TAB` on the overlay to show/hide
- **Agent tracking**: `mevedel-tools--agents-fsm` tracks sub-agent FSMs buffer-locally
- **Cleanup**: `mevedel-tools--todo-cleanup-stale` removes stale entries

### Tutor Mode
- **Purpose**: Guides users through problems without providing direct solutions, using Socratic questioning and hints
- **Core principle**: NEVER provide solutions - encourage discovery learning
- **Hint persistence**: Hints are stored in `.mevedel/hints.md` organized by concept
- **Required workflow**:
  1. Call GetHints() at start of each interaction to see hint history (from file + current session)
  2. Provide tutoring guidance using four methods: Socratic questioning, hints/tips, documentation references, problem decomposition
  3. Call RecordHint() for each hint given (persists to file + buffer-local)
- **Tutor tools**:
  - **GetHints**: Retrieves hint history from project file and current session
  - **RecordHint**: Records each hint to both file and buffer-local storage
- **Tutor preset**: `mevedel-tutor` preset enables tutor mode with appropriate tools and system prompt
- **Hint depth tracking**: System suggests appropriate hint depth based on user's progress
- **Interactive commands**:
  - `mevedel-display-hints`: Display hints for current workspace (with prefix: filter by concept)
  - `mevedel-clear-hints`: Clear all hints or hints for specific concept

### @ref and @file Mention System
- **@ref mentions**: `@ref:N` (by ID) and `@ref{tag query}` (by tags) in chat buffers
- **Expansion**: `mevedel--transform-expand-refs` runs as a gptel prompt transform (priority -90), expanding mentions into full reference content before sending to the LLM
- **@file mentions**: `@file:path` provides hierarchical directory-by-directory file completion
- **Completion at point**: `mevedel-ref-capf` and `mevedel-file-capf` provide completion for IDs, tags, and file paths
- **Font-lock**: Valid references highlighted with `success` box face, invalid with `shadow` box face
- **Registration**: Hooks added/removed in `mevedel-install`/`mevedel-uninstall` via `gptel-prompt-transform-functions` and `gptel-mode-hook`

### Inline Diff Preview System
- **Purpose**: Shows diffs from Write/Edit tools inline in chat buffer for approval
- **Size threshold**: `mevedel-inline-preview-threshold` controls inline vs. separate buffer (ratio of chat window height)
- **Overlay-based**: Creates overlays with keymaps for approve (`C-c C-c`/`a`), reject (`C-c C-k`/`r`), edit via ediff (`C-c C-e`/`e`), feedback (`C-c C-f`/`f`), toggle (`TAB`), navigate (`n`/`p`)
- **Ediff integration**: `mevedel-tools--edit-inline-preview` launches ediff; `mevedel-tools--return-to-inline-preview` hook updates overlay after editing
- **Multi-preview navigation**: `mevedel-tools--next-preview-overlay` / `mevedel-tools--previous-preview-overlay`

### Conversation Compaction
- **Purpose**: Reduces token usage in long chat sessions by summarizing old content
- **Algorithm**: Find compaction boundary (end of last LLM response) -> send old content with structured summary prompt -> mark old content as `gptel 'ignore` and dim with `shadow` face -> insert folded summary block
- **Token estimation**: Character count / 4, excluding `gptel 'ignore` regions
- **Header-line integration**: `mevedel--token-header-segment` shows context usage when above 80% of threshold; `warning` face normally, `error` face when over 100%
- **Guard**: Cannot compact during an active request

### Persistent Memory
- **Location**: `.mevedel/memory/MEMORY.md` under workspace root
- **Auto-loading**: First 200 lines included in every system prompt via `mevedel-system--memory-prompt`
- **Structure**: `MEMORY.md` is the main file; separate topic files can be linked from it
- **LLM-writable**: The LLM can be instructed to update memory files to persist discoveries

### Project Instructions (AGENTS.md / CLAUDE.md)
- **Detection**: `mevedel-system-build-prompt` checks workspace root for `AGENTS.md` first, then `CLAUDE.md`
- **Inclusion**: File contents appended as `## Workspace Configuration` section in the system prompt
- **Purpose**: Per-project LLM behavior customization, checkable into version control

### Chat Buffer Formatting
- **Org-mode support**: Optional use of org-mode for foldable prompt blocks
- **Prompt drawers**: In org-mode buffers, prompts stored in `:PROMPT:` drawers (folded by default)
- **Markdown support**: Foldable code blocks in markdown-mode buffers
- **Property escaping**: Tool results containing `:PROPERTIES:` are escaped to prevent parsing issues

### ID Management
- Unique ID generation for instruction overlays
- Conflict resolution through usage tracking
- ID retirement system for cleanup
- IDs used for linking related instructions together

### Tool Organization and Result Handling
- **Tool groups**: Each `mevedel-tool` struct carries a `:groups` list (e.g.,
  `(read edit eval util code)`). Presets and agents resolve groups via
  `mevedel-tool-resolve` which calls `mevedel-tool-for-groups` to collect all
  tools tagged with a given group symbol. No central lookup table — groups are a
  property of the tool itself.
- Property drawer escaping (`,` prefix) prevents nested drawer confusion in org-mode buffers
- Tool results properly escaped for compatibility with chat buffer format
- Workspace context explicitly passed through tool call chain

### Bash Permission System (Security Feature)
- **Granular command extraction**: Parses bash command strings to extract ALL executable commands
- **Multi-layer detection**: Handles command chains (`&&`, `||`, `;`), pipes (`|`), and command substitutions (`$()`, `` `...` ``)
- **Sudo/prefix extraction**: Extracts both sudo/doas/su AND the actual command for comprehensive detection
- **Pattern-based permissions**: `mevedel-bash-permissions` uses glob patterns with `allow`, `deny`, or `ask` actions
- **Dangerous command blocklist**: `mevedel-bash-dangerous-commands` always requires confirmation even if patterns allow
- **Fail-safe mode**: `mevedel-bash-fail-safe-on-complex-syntax` (default: t) requires confirmation for unparseable syntax
- **Complex syntax detection**: Variable expansion (`$VAR`), `eval`, `exec`, here-docs, brace expansion trigger warnings
- **Defense-in-depth**: Multiple layers: pattern rules -> dangerous blocklist -> complex syntax -> user confirmation
- **Integration**: Bash's `check-permission` slot handles all prompting internally; never returns `ask` to the pipeline

**Permission precedence**: `deny` > `ask` > `allow`
**Pattern precedence**: Later entries override earlier ones (put specific patterns LAST)

**Example configuration**:
```elisp
(setq mevedel-bash-permissions
      '(("*" . ask)          ; Default: ask for everything
        ("echo*" . allow)     ; Allow echo commands
        ("ls*" . allow)       ; Allow ls commands
        ("git*" . allow)      ; Allow git commands
        ("rm*" . deny)))      ; Explicitly deny rm

(setq mevedel-bash-dangerous-commands
      '("rm" "sudo" "dd" "chmod" "curl" "wget" "ssh"))
```

**What CAN be parsed**:
- Simple commands: `ls`, `git status`
- Command chains: `cmd1 && cmd2 || cmd3`
- Pipes: `cat file | grep pattern`
- Command substitution: `echo $(pwd)`, `` echo `date` ``
- Nested substitution: `echo $(cmd1 $(cmd2))`
- Prefixes: `sudo cmd`, `env X=y cmd`, `nice -n 10 cmd`

**What CANNOT be parsed** (fail-safe to ask):
- Variable expansion: `$VAR`, `${VAR}`
- Eval/exec: `eval 'code'`, `exec cmd`
- Here-docs: `cmd << EOF`
- Brace expansion: `{a,b,c}`
- Unbalanced quotes

**Security guarantees**:
- Will not accidentally allow hidden dangerous commands in parseable syntax
- Fails safe when syntax is too complex to parse reliably
- Dangerous command blocklist provides defense-in-depth
- Cannot defend against variable expansion attacks (e.g., `X=rm && $X -rf /`)

## File Relationships

- Instructions are file-specific but can reference across files
- Persistence maintains file associations
- Version management handles file changes between saves/loads
- Ediff integration for reviewing generated patches
- Workspace-aware file operations with access control

## Configuration

Extensive customization variables control:
- Visual styling (colors, tinting intensities)
- Behavior (auto-apply patches, display preferences)
- Tag matching and prompt generation behavior
- Workspace identification functions
- Action buffer display options
- Permission rules and modes

## Critical Implementation Notes

### Workspace Context Chain
```
Chat Buffer (authoritative)
  | mevedel--chat-buffer reference
Diff Preview Buffer
  | chat-buffer parameter
Workspace Query Functions
```

### Tool Execution Flow
```
gptel FSM calls gptel-tool :function
  | (wrapper lambda from mevedel-define-tool)
mevedel-pipeline-run-tool
  | validate args against tool spec
  | check permission (9-step chain, may prompt user)
  | snapshot file if modifying
  | call tool handler (sync or async)
  | handler may invoke confirm (show-changes-and-confirm)
callback with result string
```

### Session Lifecycle
```
mevedel (chat command)
  | mevedel-session-create (workspace, tools, presets, agents)
Chat interaction
  | mevedel-request structs track per-request state
  | Permission rules accumulate on session
Session end
  | mevedel-session-end (cleanup)
```

## Development Guidelines

### Redesign specs

Detailed specifications for the redesign are in `specs/`. See
`specs/README.md` for the phase overview and dependency graph.

### Code style

- **Lexical binding**: Every `.el` file starts with `;;; file.el -- Description -*- lexical-binding: t -*-`
- **File header**: Standard `;;; Commentary:` and `;;; Code:` sections
- **Section headers**: Two empty lines above a section header. Major sections use `;;` + blank line + `;;;`. Minor sections add depth with more semicolons: `;;;;` for subsections, `;;;;;` for sub-subsections, etc.
- **Forward declarations**: Group at the top of each file, organized by source package with a comment header (e.g., `;; \`gptel'`). Use `declare-function` for functions, `defvar` for variables.
- **Customization**: All `defcustom` variables use `:group 'mevedel`
- **Private vs public**: Use `--` double-dash for private/internal symbols (e.g., `mevedel--workspace`, `mevedel-tools--validate-params`)
- **Provide**: Every file ends with `(provide 'mevedel-MODNAME)` and `;;; mevedel-MODNAME.el ends here`
- **No `require` at top level** in library files unless absolutely necessary; prefer `declare-function`/`defvar` and `require` inside functions or `eval-when-compile`

### Testing conventions

- **Framework**: ERT via the `mevedel-deftest` macro (defined in `test/helpers.el`)
- **Test file naming**: `test/test-mevedel-{module}.el` matching the source file under test
- **One deftest per function**: All test cases for a single function go into one `mevedel-deftest` call. Use `:doc` strings to label individual cases. Exceptions are rare (e.g., `test-mevedel-diff-apply.el` where each test needs extensive setup with different file contents).
- **Real files, not mocks**: Use real temporary files/directories instead of mocking the filesystem. Clean up in test teardown.
- **Require pattern**: Every test file requires helpers with:
  ```elisp
  (require 'helpers
           (file-name-concat
            (file-name-directory
             (or buffer-file-name
                 load-file-name
                 byte-compile-current-file))
            "helpers"))
  ```
- **Test naming**: `mevedel-deftest` auto-generates test names as `FUNCTION/test` (single case) or `FUNCTION/test@N` (multiple cases)
- **Doc strings**: Each `:doc` should describe what is being tested, e.g., `"balanced quotes: \`mevedel-tools--quotes-balanced-p' accepts strings with no quotes"`. Group related cases under a shared prefix.
- **Running tests**:
  ```bash
  # All tests
  npx @emacs-eask/cli test ert test/test-*

  # Single test file
  npx @emacs-eask/cli test ert test/test-mevedel-compact.el
  ```
- **New functions should have tests**: When writing a new function, add corresponding test cases. When modifying a function, update or extend its tests if the behavior changes.

### Byte compilation

- Keep the byte compiler happy: no free variable warnings, no unknown function warnings
- Use `declare-function` for external functions, `defvar` for external variables
- Use `eval-when-compile` for `cl-lib` and other compile-time-only dependencies
- Test with `npx @emacs-eask/cli compile` before committing
