# AGENTS.md

## Project Overview

**mevedel** is an Emacs Lisp package that provides a visual workflow for interacting with LLMs during programming. It enables overlay-based instruction management for AI-assisted development with direct gptel integration.

## Architecture

### Module layer map

Each `.el` file has its own `;;; Commentary:' block describing its
purpose. This section gives the architectural layering; open the file
for details.

```
Entry point
  mevedel.el                  top-level loader, install/uninstall, directives

Data model
  mevedel-structs.el          workspace, session, request, agent-invocation
  mevedel-workspace.el        workspace detection and registry
  mevedel-permissions.el      9-step permission decision chain
  mevedel-pipeline.el         tool execution pipeline (validate -> ... -> persist)
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
  mevedel-tools.el            tool aggregator + deferred-tool machinery

Support
  mevedel-file-state.el       LRU file cache
  mevedel-diff-apply.el       overlay-preserving diff application
  mevedel-utilities.el        shared helpers (tinting, ediff glue, env info)
  mevedel-debug.el            development scaffolding (not a runtime dep)
```

### Tool prompt files

Tool descriptions are stored as external markdown files in `tools/`
and loaded via `mevedel-define-tool`'s `:prompt-file` keyword. Current
files: `agent.md`, `ask.md`, `bash.md`, `createplan.md`, `edit.md`,
`eval.md`, `gethints.md`, `glob.md`, `grep.md`, `imenu.md`, `mkdir.md`,
`presentplan.md`, `read.md`, `recordhint.md`, `requestaccess.md`,
`sendmessage.md`, `taskcreate.md`, `taskget.md`, `tasklist.md`,
`taskupdate.md`, `toolsearch.md`, `treesitter.md`, `webfetch.md`,
`websearch.md`, `write.md`, `xref-definitions.md`, `xref-references.md`,
`youtube.md`.

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
- `test-mevedel-workspace.el`: Workspace detection, state directory, additional roots
- `test-mevedel-permissions.el`: Permission decision chain, rules, modes, session rules
- `test-mevedel-pipeline.el`: Pipeline step execution, error handling, step skipping
- `test-mevedel-tool-registry.el`: Tool registration, struct creation, group management
- `test-mevedel-tools.el`: Deferred-tool infrastructure, mailbox, slash-command dispatch
- `test-mevedel-tools-validation.el`: `mevedel-tools--validate-params` macro
- `test-mevedel-tool-fs.el`: Read, Glob, Grep, Write, Edit, MkDir handlers
- `test-mevedel-tools-edit.el`: Edit tool string replacement and diff logic
- `test-mevedel-tool-code.el`: Xref, Imenu, Treesitter handlers
- `test-mevedel-tools-bash-permissions.el`: Bash permission system, command parsing, Eval permissions
- `test-mevedel-tool-plan.el`: PresentPlan/CreatePlan registration and arg validation
- `test-mevedel-tool-task.el`: Task CRUD, dependency propagation, overlay rendering
- `test-mevedel-tool-web.el`: WebSearch/WebFetch/YouTube registration
- `test-mevedel-diff-apply.el`: Overlay-preserving diff application
- `test-mevedel-file-state.el`: LRU file cache, snapshot promotion and eviction
- `test-mevedel-preview-mode.el`: Preview minor mode register/dismiss lifecycle
- `test-mevedel-reminders.el`: Reminder injection, trigger/content, interval throttling
- `test-mevedel-skills.el`: SKILL.md discovery, slash commands, skill invocation
- `test-mevedel-mentions.el`: `@ref`/`@file` parsing, expansion, and font-lock matchers
- `test-mevedel-system.el`: System prompt builder and AGENTS.md/CLAUDE.md inclusion
- `test-mevedel-presets.el`: Preset definitions and tool filtering
- `test-mevedel-view.el`: View-buffer rendering and compact-tool summaries
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
The pipeline handles validation, permissions, snapshots, and result
persistence — handlers contain zero boilerplate for these concerns.

Key flags on `mevedel-tool` struct:
- `:read-only-p` — skips snapshot step
- `:get-path` — lambda extracting path from args for permission scoping
- `:check-permission` — `(tool-struct input) -> allow|deny|ask|nil` for domain-specific permission logic
- `:async-p` — handler receives callback as first arg
- `:max-result-size` — char limit before result is persisted to disk (nil = no persistence)

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
- **Specialized agents** for focused tasks, declared with `mevedel-define-agent`:
  - **explore**: Read-only investigation with configurable thoroughness (caller-specified quick/moderate/thorough)
  - **planner**: Interactive implementation planning with PresentPlan tool
  - **coordinator**: Orchestration agent that dispatches workers via `Agent(run_in_background=true)`, monitors results, and verifies implementations — never implements directly
  - **verifier**: Adversarial, read-only verification of implementations; carries a per-turn `verifier-read-only` reminder attached at invocation time
  - **introspector**: Elisp/Emacs introspection and debugging (from gptel-agent)
- **Agent tool filtering**: Each agent's `:tools` is resolved via `mevedel-tool-resolve-gptel` at invocation time
- **Delegation rules**: System prompt guides main agent when to delegate
- **Dynamic registration**: Agents registered buffer-locally via `gptel-agent--agents` on every request (no caching across requests)
- **Reminder isolation**: Each invocation gets a cloned reminder list with independent `last-fired` tracking; max-turns warnings and the verifier read-only reminder are attached at invocation time to avoid a load-time cycle with `mevedel-reminders`
- **Background spawning**: Agent tool supports `run_in_background` parameter — returns immediately with launch status, sub-agent result delivered to parent's mailbox when complete (see Background Agent Spawning below)

### Inter-Agent Messaging (SendMessage)
- **Purpose**: Asynchronous, fire-and-forget messages between the main chat and spawned sub-agents, or between sibling sub-agents
- **Recipient aliases**: `"main"`, `"chat"`, `"coordinator"` all resolve to the main session mailbox; exact agent-id match or `"<agent-type>--"` prefix match resolves to a specific sub-agent
- **Delivery**: Messages are queued on the recipient's mailbox (session or invocation) and drained by a WAIT-state handler (`mevedel-tools--handle-message-inject`) that wraps each message in `<agent-message from="SENDER">...</agent-message>` and injects them as a user-role turn via `gptel--inject-prompt`
- **Mailbox storage**: Polymorphic accessor `mevedel-tools--ctx-messages` dispatches on `mevedel-session` vs `mevedel-agent-invocation`

### Coordinator Skill
- **Bundled skill**: `skills/coordinator/SKILL.md` is shipped with mevedel and discovered via `mevedel-skills--bundled-dir`
- **Execution context**: `context: fork` — delegates to the coordinator agent via `mevedel-tools--task`
- **Workflow**: Analyze → TaskCreate with dependencies → dispatch workers via `Agent(run_in_background=true)` → monitor mailbox for `<agent-message>` results → `SendMessage` for course-correction → spawn `verifier` for verification → synthesize results
- **Discovery shadowing**: User-provided skills in `~/.claude/skills/` or `.mevedel/skills/` override the bundled version by name; tests bind `mevedel-skills--include-bundled` nil to assert exact user-skill contents

### PresentPlan Tool (Interactive Planning)
- **Purpose**: Presents implementation plans for user feedback in chat buffer
- **Async interaction**: Blocks agent execution until user responds
- **User actions**: Accept (proceed), Reject (with feedback), Modify (specific sections)
- **Iteration support**: Planner agent can call PresentPlan multiple times to refine plan
- **Plan structure**: Title, summary, and sections (types: step, risk, alternative, dependency)

### Task Overlay Display
- **Multi-context**: Tasks are tracked per caller (main chat and each sub-agent separately)
- **Dependency tracking**: `blockedBy` links propagate completion — finishing an upstream task automatically unblocks dependents
- **Display**: Overlay in chat buffer with status icons (completed, in_progress, pending, blocked)
- **Agent tracking**: `mevedel-tools--agents-fsm` (buffer-local on chat buffer) maps agent-id strings to sub-agent FSMs, used for SendMessage recipient resolution

### Background Agent Spawning
- **Problem**: gptel's FSM blocks in TOOL state until ALL tool callbacks fire. A foreground `Agent` call blocks the parent FSM — the parent cannot invoke other tools (like `SendMessage`) until the sub-agent completes, making true orchestration impossible.
- **Solution**: `run_in_background` parameter on the Agent tool. When true, `mevedel-tools--task` calls `process-tool-result` immediately with a launch status string, unblocking the parent FSM. The sub-agent continues running independently.
- **Result delivery**: When the background sub-agent finishes, its result is wrapped in `<agent-result>` XML and pushed to the parent's mailbox via `mevedel-tools--ctx-push-message`. The parent receives it as an `<agent-message>` block on its next WAIT-state turn (via `mevedel-tools--handle-message-inject`).
- **Async vs background**: "Async" (tool flag `async-p`) means the tool doesn't block Emacs's event loop but DOES block the parent FSM (it waits for the callback). "Background" means the parent FSM is unblocked immediately — the sub-agent runs fire-and-forget with mailbox delivery.
- **BWAIT parking state**: When the LLM produces no tool calls but background agents are still running (or undelivered results sit in the mailbox), the FSM parks in BWAIT instead of terminating in DONE. The background agent's completion callback resumes the parent from BWAIT to WAIT, which drains the mailbox and fires a new LLM request. Transition table injection: `mevedel-preset--inject-bwait-transitions` for the main session (buffer-local `gptel-send--transitions`), `mevedel-tools--inject-bwait-transition` for sub-agent FSMs (via the agent request advice). The `background-agents` slot on both `mevedel-session` and `mevedel-agent-invocation` tracks running background children.
- **Foreground callback suppression**: When a foreground agent (e.g., the coordinator via Skill `context: fork`) has background children, `gptel-agent--task`'s callback fires before the FSM transitions to BWAIT. The foreground wrapper in `mevedel-tools--task` detects pending background agents and stashes the result on the invocation's `stashed-result` slot instead of calling `main-cb`. The FSM then parks in BWAIT. When all background agents finish and deliver results, the FSM resumes BWAIT→WAIT, the LLM processes the agent-messages and produces a final response, and the callback fires again — now with no background agents pending, so `main-cb` is called with the accumulated result.
- **Use case**: Coordinator agent dispatches multiple workers in parallel via `Agent(run_in_background=true)`, then uses `SendMessage` to guide them and monitors results as they arrive on its mailbox.

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

### @ref, @file, @agent, and @mcp Mention System
- **@ref mentions**: `@ref:N` (by ID) and `@ref:{tag query}` (by tags) in chat buffers
- **@file mentions**: `@file:path` provides hierarchical directory-by-directory file completion; optional `#L<start>[-<end>]` suffix pins a line range (e.g., `@file:foo.el#L10-20` or `@file:foo.el#L42`). Range reads are NOT recorded in the session touched-files map, since the LLM may still need to read other parts of the file.
- **@file directory mode**: if the path is a directory, the handler returns a gitignore-filtered recursive file listing (via `rg --files --hidden --follow --sort path`) capped at `mevedel-file-mention-directory-max-entries` (default 1000). Dedup key is `(dir . EXPANDED)`; listing is truncated with a note if the cap is exceeded.
- **@agent mentions**: `@agent:name` asks the main agent to delegate the current turn to a registered sub-agent. The handler looks up `mevedel-agent--registry`; when the agent exists, the reminder instructs the model to call `Agent(subagent_type="NAME", prompt=...)` instead of answering directly. Unknown names produce a `[agent:NAME -- no such agent]` placeholder with a clarifying reminder. Dedup key is `(agent . NAME)`.
- **@mcp mentions**: `@mcp:server:uri` attaches an MCP resource via mcp.el. The handler looks up the server in `mcp-hub-get-servers`, verifies `:status` = `connected`, pulls the connection from `mcp-server-connections`, and calls `mcp-read-resource` synchronously. The result's `:contents[].text` entries are concatenated into a fenced code block in the reminder. Dedup key is `(mcp . (SERVER . URI))`. The URI capture is greedy past internal colons so `file:///...` URIs work. mcp.el is an optional runtime dependency (declared via `declare-function`); when absent the handler emits `[mcp:... -- mcp.el not available]`.
- **Expansion**: `mevedel--transform-expand-mentions` runs as a gptel prompt transform (priority -90), dispatching per `mevedel-mention-handlers` to replace each raw mention with a compact bracketed placeholder and inject its full content as a `<system-reminder>` block above the user prompt
- **Placeholder syntax** (consistent across mention types):
  - `[ref:N -- contents attached above]` / `[ref:N -- removed since an earlier turn]`
  - `[refs matching 'QUERY': #N, #M -- contents attached above]` / `[ref:{QUERY} -- no matches]`
  - `[file:PATH -- contents attached above]` / `[file:PATH -- does not exist]` / `... -- is a directory` / `... -- binary` / `... -- permission denied`
  - `[agent:NAME -- delegation requested]` / `[agent:NAME -- no such agent]`
  - `[mcp:SERVER:URI -- contents attached above]` / `[mcp:SERVER:URI -- unknown server \`NAME\`]` / `... -- server \`NAME\` not connected` / `... -- read failed: MSG` / `... -- mcp.el not available`
- **@file safety**: handler runs the `Read` tool's permission check (`mevedel-check-permission "Read"`) before reading; any non-`allow` result (including `ask` for paths outside the workspace) yields a "permission denied" placeholder since prompt transforms cannot interactively prompt. Also rejects directories, unreadable files, and binary extensions.
- **Graceful-failure reminders**: every rejection branch (missing file, directory, binary, permission denied, missing ref, empty tag query, unknown agent, mcp.el missing, unknown/disconnected mcp server, mcp read failure) emits a `<system-reminder>` explaining that the `[... -- reason]` token in the user prompt is a system annotation, not user-written text. This prevents the LLM from interpreting the bracketed reason as something the user typed.
- **@file size cap**: file contents are read via `mevedel-tool-fs--slurp-file-contents`, which reuses Read's 512 KB cap and line-numbered formatting; oversize files surface a graceful placeholder instead of stuffing context
- **Read dedup**: when a session is available, `@file` records the read on `mevedel-session-touched-files` so a subsequent `Read` tool call recognizes the file as already-read and short-circuits
- **Per-session dedup**: `mevedel-session-mentions-shown` hash-table keyed on `(KIND . KEY)` stores `(turn . content-hash)`; if the hash is unchanged, the reminder block is skipped on later turns to avoid re-injecting the same content
- **Completion at point**: `mevedel-ref-capf`, `mevedel-file-capf`, `mevedel-agent-capf`, and `mevedel-mcp-capf` provide completion for IDs, tags, file paths, registered agent names (using each agent's description as the candidate annotation), and MCP servers/resources (two-stage: server names at `@mcp:`, then resource URIs at `@mcp:server:` — annotation shows resource name/description)
- **Font-lock**: valid references/agents highlighted with `success` box face, invalid with `shadow` box face; files with `link` box face when resolvable; connected MCP servers with `success` box face, disconnected/unknown with `shadow`
- **Registration**: Hooks added/removed in `mevedel-install`/`mevedel-uninstall` via `gptel-prompt-transform-functions` and `gptel-mode-hook`

### Inline Diff Preview System
- **Purpose**: Shows diffs from Write/Edit tools inline in chat buffer for approval
- **Entry point**: `mevedel-preview-mode-add-preview` (keyword API) is the single call site for tool handlers
- **Minor mode**: `mevedel-preview-mode` tracks pending previews in a buffer-local list, auto-activates on first add, auto-deactivates when the list empties
- **Mode-line lighter**: Shows `" Preview[N]"` with N pending overlays
- **Mode keymap** (prefix `C-c p`): `n` next / `p` previous / `a` approve-all / `r` reject-all
- **Per-overlay keymap**: approve (`C-c C-c`/`a`/`RET`), reject (`C-c C-k`/`r`/`q`), edit via ediff (`C-c C-e`/`e`), feedback (`C-c C-f`/`f`), toggle (`TAB`), navigate (`n`/`p`)
- **Abort integration**: Registering a preview installs `mevedel-preview-mode-dismiss-all` as the active request's `cancel-fn`, so `mevedel-abort` tears down all pending overlays without firing continuations
- **Size threshold**: `mevedel-inline-preview-threshold` controls inline vs. separate buffer (ratio of chat window height)
- **Ediff integration**: `mevedel-tools--edit-inline-preview` launches ediff; `mevedel-tools--return-to-inline-preview` hook updates overlay after editing

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
  | handler may invoke confirm (mevedel-preview-mode-add-preview)
  | persist oversized result to disk (if max-result-size set)
callback with result string (or preview + file path)
```

### Tool Result Persistence
When a tool's `max-result-size` is set and the result exceeds the effective
limit (minimum of the tool value and the 50,000-char global cap), the full
result is saved to `.mevedel/tool-results/` and replaced with a preview
wrapped in `<persisted-output>` XML. The LLM can use Read to access the
full output selectively.

**Per-tool limits** (matching Claude Code's approach):

| Tool | max-result-size | Rationale |
|------|----------------|-----------|
| Grep | 20,000 | Search results; already has 200KB hard cap |
| Bash, Eval | 30,000 | Command output; already has 512KB hard cap |
| Glob | 30,000 | Directory listings |
| XrefReferences, XrefDefinitions, Imenu | 20,000 | Code navigation |
| Treesitter | 30,000 | AST output |
| Agent | 50,000 | Sub-agent results |
| WebFetch, YouTube | 50,000 | Full page content / transcripts |
| Read | nil | Self-bounded by line/byte limits |
| Write, Edit, MkDir, Ask, etc. | nil | Short results, no persistence |

Error results (`"Error:"` prefix) are never persisted. When no workspace
is available, persistence is skipped and the full result passes through.

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
