# AGENTS.md

## Project Overview

**mevedel** is an Emacs Lisp package that provides a visual workflow for interacting with LLMs during programming. It enables overlay-based instruction management for AI-assisted development with direct gptel integration.

## Architecture

### Core Components

1. **mevedel.el** (~750 lines): Main entry point and gptel integration
   - Installation/uninstallation system
   - Direct gptel-request integration (no macher dependency)
   - LLM action processing (implement, revise, discuss directives)
   - Prompt generation and workspace management
   - Org-mode heading management with UUID tracking

2. **mevedel-instructions.el** (2,276 lines): Core overlay system
   - Instruction overlay creation, modification, and deletion
   - Tag-based categorization and navigation
   - Visual styling and linking system
   - History/undo functionality

3. **mevedel-restorer.el** (355 lines): Persistence layer
   - Save/load instructions to/from files
   - Version management and file association
   - Automatic patching for outdated files

4. **mevedel-utilities.el** (937 lines): Utility functions
   - Color tinting for overlays
   - Ediff integration for patch review
   - Text manipulation and formatting utilities

5. **mevedel-workspace.el** (~210 lines): Multi-workspace support
   - Workspace identification and management
   - Project root tracking with additional root support
   - Buffer-local workspace context isolation

6. **mevedel-tools.el** (~3400 lines): LLM tool definitions
   - File operations: read, write, edit, insert
   - Code exploration: grep, glob, xref, imenu, treesitter
   - User interaction: ask_user, present_plan, request_directory_access
   - Bash execution with workspace context and permission system
   - Tool result escaping for org-mode compatibility

7. **mevedel-agents.el** (~280 lines): Specialized agent definitions
   - Three specialized agents: codebase-analyst, researcher, planner
   - Agent-specific tool filtering and system prompts
   - Integration with gptel-agent for multi-agent workflows

8. **mevedel-system.el** (~600 lines): System prompt generation
   - Base system prompt with delegation rules
   - Tool-specific instructions for each available tool
   - Pattern-based delegation guidance

9. **mevedel-presets.el** (~190 lines): gptel preset configuration
   - Three presets: discuss (read-only), implement (editing), revise (with context)
   - FSM termination handlers for cleanup
   - Org-mode buffer fixup (indentation, narrowing, separators)
   - Dynamic agent registration

10. **mevedel-diff-apply.el** (~680 lines): Advanced patch application
    - Overlay-preserving diff application
    - File creation/deletion support
    - Minimal change region detection

### Key Data Structures

- `mevedel--instructions`: Main instruction overlay storage (alist: buffer → overlays)
- `mevedel--id-counter` / `mevedel--id-usage-map`: UUID management system
- `mevedel--workspace`: Buffer-local workspace identifier
- `mevedel--current-directive-uuid`: Tracks currently processing directive
- **Instruction types**: References (provide context) and Directives (LLM prompts)

### External Dependencies

- **gptel (≥0.9.0)**: Direct LLM integration backend
- **gptel-agent**: Multi-agent workflow support (Agent tool, delegation infrastructure)
- **Emacs ≥30.1**: Required for modern features
- **ediff**: For patch review functionality
- **org-mode**: Optional, for structured chat buffers with headings

## Development Commands

### Emacs Lisp Development
```bash
# No traditional build system - pure Emacs Lisp package
# Test interactively in Emacs by loading files
emacs -Q -l mevedel.el
```

### Testing
```bash
# Run unit tests with buttercup
emacs --script run-tests.el

# Or run tests in batch mode
emacs --batch -l run-tests.el

# Run tests interactively in Emacs
# M-x load-file RET test-mevedel-utilities-simple.el RET
# M-x buttercup-run-discover RET
```

#### Test Files
- `test-mevedel-utilities-simple.el`: Clean tests using real temporary files (recommended)
- `test-mevedel-utilities.el`: Comprehensive tests with mocking (legacy)
- `run-tests.el`: Test runner script

#### Testing Infrastructure Features
- Uses real temporary files instead of mocking for more realistic tests
- Automatic cleanup of test buffers and temporary files
- Overlay creation utilities for testing mevedel instruction overlays
- Proper diff generation for testing mevedel-diff-apply-buffer

### Key Interactive Commands
- `mevedel-create-reference` / `mevedel-create-directive`: Create instructions
- `mevedel-save-instructions` / `mevedel-load-instructions`: Persistence
- `mevedel-implement-directive` / `mevedel-revise-directive` / `mevedel-discuss-directive`: LLM processing
- `mevedel-process-directives`: Process multiple directives sequentially
- `mevedel-next-instruction` / `mevedel-previous-instruction`: Navigation
- `mevedel-diff-apply-buffer`: Apply patches with overlay preservation

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
- Three presets: `mevedel-discuss`, `mevedel-implement`, `mevedel-revise`
- Tool results properly escaped for org-mode compatibility
- FSM termination handlers for cleanup and fixup

### Multi-Agent System
- **Three specialized agents** for focused tasks:
  - **codebase-analyst**: Deep architectural analysis, pattern recognition, dependency mapping (all read tools, NO web access)
  - **researcher**: Online research and documentation discovery (web tools + Read/Grep for cross-referencing)
  - **planner**: Interactive implementation planning with PresentPlan tool (all read tools + PresentPlan)
- **Agent tool filtering**: Each agent has specific tool access based on its responsibilities
- **Delegation rules**: System prompt guides main agent when to delegate to specialists
- **Dynamic registration**: Agents registered buffer-locally via `gptel-agent--agents`
- **Pattern-based delegation**:
  - "how does X work?" → codebase-analyst
  - "find documentation for Y" → researcher
  - "create a plan for Z" → planner

### PresentPlan Tool (Interactive Planning)
- **Purpose**: Presents implementation plans for user feedback in chat buffer
- **Async interaction**: Blocks agent execution until user responds
- **User actions**: Accept (proceed), Reject (with feedback), Modify (specific sections)
- **Iteration support**: Planner agent can call PresentPlan multiple times to refine plan
- **Plan structure**: Title, summary, and sections (types: step, risk, alternative, dependency)
- **Org-mode formatting**: Plans displayed as org headings with properties for clean integration

### Org-Mode Chat Buffers
- **UUID-based heading management**: Directives create/reuse org headings with `MEVEDELUUID` property
- **Structured conversation**: Each directive gets its own heading with action tags
- **Prompt drawers**: Prompts stored in `:PROMPT:` drawers (folded by default)
- **Auto-fixup**: Termination handler indents subtree, widens buffer, adds separators
- **Property escaping**: Tool results containing `:PROPERTIES:` are escaped to prevent parsing issues

### ID Management
- Unique UUID generation for instructions
- Conflict resolution through usage tracking
- ID retirement system for cleanup
- UUIDs used for org heading tracking and history management

### Tool Result Handling
- Org-mode property drawer escaping (`,` prefix) prevents nested drawer confusion
- All tool results validated for org-mode compatibility
- Workspace context explicitly passed through tool call chain

### Bash Permission System (Security Feature)
- **Granular command extraction**: Parses bash command strings to extract ALL executable commands
- **Multi-layer detection**: Handles command chains (`&&`, `||`, `;`), pipes (`|`), and command substitutions (`$()`, `` `...` ``)
- **Sudo/prefix extraction**: Extracts both sudo/doas/su AND the actual command for comprehensive detection
- **Pattern-based permissions**: `mevedel-bash-permissions` uses glob patterns with `allow`, `deny`, or `ask` actions
- **Dangerous command blocklist**: `mevedel-bash-dangerous-commands` always requires confirmation even if patterns allow
- **Fail-safe mode**: `mevedel-tools--bash-fail-safe-on-complex-syntax` (default: t) requires confirmation for unparseable syntax
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

### Known Issues & Workarounds
- **Org property drawers in tool results**: Escaped with `,` prefix to prevent parsing issues
- **Line range beyond EOF**: Tool correctly returns available lines, escaping handles org content
- **Multiple workspaces**: Explicit buffer context tracking prevents cross-workspace contamination
