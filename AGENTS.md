# AGENTS.md

## Project Overview

**mevedel** is an Emacs Lisp package that provides a visual workflow for interacting with LLMs during programming. It enables overlay-based instruction management for AI-assisted development with direct gptel integration.

## Architecture

### Core Components

1. **mevedel.el** (~934 lines): Main entry point and gptel integration
   - Installation/uninstallation system
   - Direct gptel-request integration
   - LLM action processing (implement, revise, discuss, tutor directives)
   - Prompt generation and workspace management
   - Optional org-mode support for chat buffer formatting
   - `@ref` expansion hook registration
   - Token header integration for compaction

2. **mevedel-instructions.el** (~2626 lines): Core overlay system
   - Instruction overlay creation, modification, and deletion
   - Tag-based categorization and navigation
   - Visual styling and linking system
   - ID-based instruction linking
   - `@ref` mention system: parsing, expansion, completion, and font-lock
   - `@file:` mention system: hierarchical file completion in chat buffers

3. **mevedel-restorer.el** (~342 lines): Persistence layer
   - Save/load instructions to/from files
   - Version management and file association
   - Automatic patching for outdated files

4. **mevedel-utilities.el** (~609 lines): Utility functions
   - Color tinting for overlays
   - Ediff integration for patch review
   - Text manipulation and formatting utilities

5. **mevedel-workspace.el** (~258 lines): Multi-workspace support
   - Workspace identification and management
   - Project root tracking with additional root support
   - Buffer-local workspace context isolation

6. **mevedel-tools.el** (~5307 lines): LLM tool definitions
   - File operations: Read, Write, Edit, Insert, MkDir
   - File search: Glob, Grep
   - Code exploration: XrefReferences, XrefDefinitions, Imenu, Treesitter
   - User interaction: Ask, PresentPlan, RequestAccess, GetHints, RecordHint
   - Task tracking: TodoWrite, TodoRead
   - Execution: Bash (with permission system), Eval (via gptel-agent)
   - Web: WebSearch (DuckDuckGo via eww), WebFetch, YouTube (via gptel-agent)
   - Inline diff preview system with approve/reject/edit/feedback workflow
   - Todo overlay display system with multi-context support
   - Tool result escaping for org-mode compatibility

7. **mevedel-agents.el** (~314 lines): Specialized agent definitions
   - Four specialized agents: codebase-analyst, researcher, planner, introspector
   - Agent-specific tool filtering and system prompts
   - Integration with gptel-agent for multi-agent workflows

8. **mevedel-system.el** (~314 lines): System prompt generation
   - Tone prompt (code style, terseness, accuracy)
   - Base system prompt with task execution protocol and delegation rules
   - Persistent memory prompt (loads `.mevedel/memory/MEMORY.md`)
   - System prompt builder: assembles base + memory + environment + workspace config
   - AGENTS.md / CLAUDE.md workspace configuration loading
   - Tutor assistant prompt (GetHints, RecordHint)
   - Pattern-based delegation guidance

9. **mevedel-presets.el** (~183 lines): gptel preset configuration
   - Four presets: discuss (read-only), implement (editing), revise (with context), tutor (tutoring assistant)
   - Preset inheritance via `:parents` (implement inherits discuss, revise inherits implement, tutor inherits discuss)
   - FSM termination handlers for cleanup
   - Dynamic agent registration (buffer-local via `gptel-agent--agents`)

10. **mevedel-diff-apply.el** (~657 lines): Advanced patch application
    - Overlay-preserving diff application
    - File creation/deletion support
    - Minimal change region detection

11. **mevedel-compact.el** (~314 lines): Conversation compaction
    - Token estimation (character count / 4, excluding ignored regions)
    - Compaction boundary detection (finds end of last LLM response)
    - LLM-based summarization of old conversation content
    - Compact apply: marks old content as ignored, dims with shadow face, inserts folded summary block
    - Token header segment for gptel header-line (shows context usage warning)

### Key Data Structures

- `mevedel--instructions`: Main instruction overlay storage (alist: buffer → overlays)
- `mevedel--id-counter` / `mevedel--id-usage-map`: Instruction ID management for linking
- `mevedel--workspace`: Buffer-local workspace identifier
- **Instruction types**: References (provide context) and Directives (LLM prompts)

### External Dependencies

- **gptel (≥0.9.0)**: Direct LLM integration backend
- **gptel-agent**: Multi-agent workflow support (Agent tool, delegation infrastructure)
- **Emacs ≥30.1**: Required for modern features
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
- `test-mevedel-diff-apply.el`: Extensive overlay-preserving diff application tests
- `test-mevedel-tools-bash-permissions.el`: Bash permission system and command parsing
- `test-mevedel-tools-edit.el`: Edit tool string replacement and diff logic
- `test-mevedel-tools-validation.el`: `mevedel-tools--validate-params` macro
- `test-mevedel-compact.el`: Token estimation and file-local-variables detection
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

### Workspace Management (Critical for Multi-Project Use)
- **Buffer-local workspace context**: Each chat buffer has `mevedel--workspace` set
- **Authoritative chat buffer**: Tools execute in chat buffer context (validated with assertions)
- **Explicit buffer parameters**: Workspace query functions accept optional buffer parameter
- **Delegation pattern**: Derived buffers (diff preview, patch) store `mevedel--chat-buffer` reference
- **Access control**: Directory access requests tracked per workspace with `mevedel-workspace-additional-roots`
- **File snapshots**: Tool modifications tracked buffer-locally per request

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
  - "how does X work?" → codebase-analyst
  - "find documentation for Y" → researcher
  - "create a plan for Z" → planner
  - "I need to understand..." about elisp/Emacs → introspector

### PresentPlan Tool (Interactive Planning)
- **Purpose**: Presents implementation plans for user feedback in chat buffer
- **Async interaction**: Blocks agent execution until user responds
- **User actions**: Accept (proceed), Reject (with feedback), Modify (specific sections)
- **Iteration support**: Planner agent can call PresentPlan multiple times to refine plan
- **Plan structure**: Title, summary, and sections (types: step, risk, alternative, dependency)

### Todo Overlay Display
- **Multi-context**: Tracks todos per caller (main agent and each sub-agent separately)
- **Display**: Overlay in chat buffer with icons: `✓` completed (strikethrough), `→` in_progress (bold), `○` pending
- **Toggle**: `mevedel-toggle-todos` or `TAB` on the overlay to show/hide
- **Agent tracking**: `mevedel-tools--agents-fsm` tracks sub-agent FSMs buffer-locally
- **Cleanup**: `mevedel-tools--todo-cleanup-stale` removes stale entries

### Tutor Mode
- **Purpose**: Guides users through problems without providing direct solutions, using Socratic questioning and hints
- **Core principle**: NEVER provide solutions - encourage discovery learning
- **Required workflow**:
  1. Call GetHints() at start of each interaction to see hint history
  2. Provide tutoring guidance using four methods: Socratic questioning, hints/tips, documentation references, problem decomposition
  3. Call RecordHint() for each hint given to build accurate history
- **Tutor tools**:
  - **GetHints**: Retrieves hint history for current directive
  - **RecordHint**: Records each hint given with type, concept, and depth
- **Tutor preset**: `mevedel-tutor` preset enables tutor mode with appropriate tools and system prompt
- **Hint depth tracking**: System suggests appropriate hint depth based on user's progress

### @ref and @file Mention System
- **@ref mentions**: `@ref:N` (by ID) and `@ref{tag query}` (by tags) in chat buffers
- **Expansion**: `mevedel--transform-expand-refs` runs as a gptel prompt transform (priority -90), expanding mentions into full reference content before sending to the LLM
- **@file mentions**: `@file:path` provides hierarchical directory-by-directory file completion
- **Completion at point**: `mevedel-ref-capf` and `mevedel-file-capf` provide completion for IDs, tags, and file paths
- **Font-lock**: Valid references highlighted with `success` box face, invalid with `shadow` box face
- **Registration**: Hooks added/removed in `mevedel-install`/`mevedel-uninstall` via `gptel-prompt-transform-functions` and `gptel-mode-hook`

### Inline Diff Preview System
- **Purpose**: Shows diffs from Write/Edit/Insert tools inline in chat buffer for approval
- **Size threshold**: `mevedel-inline-preview-threshold` controls inline vs. separate buffer (ratio of chat window height)
- **Overlay-based**: Creates overlays with keymaps for approve (`C-c C-c`/`a`), reject (`C-c C-k`/`r`), edit via ediff (`C-c C-e`/`e`), feedback (`C-c C-f`/`f`), toggle (`TAB`), navigate (`n`/`p`)
- **Ediff integration**: `mevedel-tools--edit-inline-preview` launches ediff; `mevedel-tools--return-to-inline-preview` hook updates overlay after editing
- **Multi-preview navigation**: `mevedel-tools--next-preview-overlay` / `mevedel-tools--previous-preview-overlay`

### Conversation Compaction
- **Purpose**: Reduces token usage in long chat sessions by summarizing old content
- **Algorithm**: Find compaction boundary (end of last LLM response) → send old content with structured summary prompt → mark old content as `gptel 'ignore` and dim with `shadow` face → insert folded summary block
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
- **Tool groups**: Tools organized into explicit lists used by presets and agents:
  - `mevedel-tools--read-tools`: Read, Glob, Grep, WebFetch
  - `mevedel-tools--code-tools`: XrefReferences, XrefDefinitions, Imenu, Treesitter
  - `mevedel-tools--edit-tools`: Write, Edit, Insert, MkDir
  - `mevedel-tools--eval-tools`: Bash, Eval
  - `mevedel-tools--util-tools`: TodoWrite, TodoRead, Ask, RequestAccess, Agent, WebSearch, WebFetch, YouTube
- Property drawer escaping (`,` prefix) prevents nested drawer confusion in org-mode buffers
- Tool results properly escaped for compatibility with chat buffer format
- Workspace context explicitly passed through tool call chain
- Per-tool instructions embedded directly in each tool's `:description` field

### Bash Permission System (Security Feature)
- **Granular command extraction**: Parses bash command strings to extract ALL executable commands
- **Multi-layer detection**: Handles command chains (`&&`, `||`, `;`), pipes (`|`), and command substitutions (`$()`, `` `...` ``)
- **Sudo/prefix extraction**: Extracts both sudo/doas/su AND the actual command for comprehensive detection
- **Pattern-based permissions**: `mevedel-bash-permissions` uses glob patterns with `allow`, `deny`, or `ask` actions
- **Dangerous command blocklist**: `mevedel-bash-dangerous-commands` always requires confirmation even if patterns allow
- **Fail-safe mode**: `mevedel-bash-fail-safe-on-complex-syntax` (default: t) requires confirmation for unparseable syntax
- **Complex syntax detection**: Variable expansion (`$VAR`), `eval`, `exec`, here-docs, brace expansion trigger warnings
- **Defense-in-depth**: Multiple layers: pattern rules → dangerous blocklist → complex syntax → user confirmation

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
- ✓ Simple commands: `ls`, `git status`
- ✓ Command chains: `cmd1 && cmd2 || cmd3`
- ✓ Pipes: `cat file | grep pattern`
- ✓ Command substitution: `echo $(pwd)`, `` echo `date` ``
- ✓ Nested substitution: `echo $(cmd1 $(cmd2))`
- ✓ Prefixes: `sudo cmd`, `env X=y cmd`, `nice -n 10 cmd`

**What CANNOT be parsed** (fail-safe to ask):
- ✗ Variable expansion: `$VAR`, `${VAR}`
- ✗ Eval/exec: `eval 'code'`, `exec cmd`
- ✗ Here-docs: `cmd << EOF`
- ✗ Brace expansion: `{a,b,c}`
- ✗ Unbalanced quotes

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

## Critical Implementation Notes

### Workspace Context Chain
```
Chat Buffer (authoritative)
  ↓ mevedel--chat-buffer reference
Diff Preview Buffer
  ↓ chat-buffer parameter
Workspace Query Functions
```

### Tool Execution Context
- Tools execute via `gptel--handle-tool-use` in chat buffer context (gptel-request.el:1697)
- Assertions validate `mevedel--workspace` is set in current buffer
- FSM info provides `:buffer` key with chat buffer reference for termination handlers
