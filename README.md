# mevedel

*mevedel* is an Emacs LLM workflow package, originally inspired by `evedel`,
now focused on directive-driven coding sessions, tool use, agents, and
persistent project context. It is built on
[gptel](https://github.com/karthink/gptel), is versatile enough so that it can
be utilized in various types of buffers, and isn't limited to just programming
buffers.

> [!WARNING]
> `mevedel` uses a lot of tokens. Consider using a model which allows caching.

> [!WARNING]
> The package is functional but still work in progress. Expect missing features, unexpected behavior and bugs.

## What does this package do?

`mevedel` began as a fork of the Emacs package
[evedel](https://github.com/daedsidog/evedel), building on its overlay-based
instruction model and integrating it with
[gptel](https://github.com/karthink/gptel) for direct LLM interaction. It has
since grown into an independent LLM workbench for Emacs, while still giving
credit to the original `evedel` idea.

Key features:

- Uses [gptel](https://github.com/karthink/gptel) for LLM integration with
  support for multiple backends.
- Uses overlays for tracking instructions instead of raw text, decoupling
  instructions from your raw text. The overlays are mostly intuitive and can be
  customized, and try not to interfere with the regular Emacs workflow.
- Can save your instruction overlays so you won't have to restart the labeling
  process each session.
- Can categorize your references with tags, and use complex query expressions to
  determine what to send to the model in directives.
- Can easily cycle through between instruction overlays across all buffers.
- Specialized sub-agents (explorer, coordinator, verifier, reviewer)
  for focused tasks via [gptel-agent](https://github.com/karthink/gptel-agent),
  with background dispatch and inter-agent messaging.
- Supervised Goals (`/goal <objective>`) for read-only planning, explicit
  approval, implementation, and read-only review in one session-owned cycle.
- Skills (`SKILL.md` packages) for reusable `$skill` commands and prompt bundles,
  scanned from user / project / bundled directories.
- Persistent sessions per workspace with resume, rewind to any prior prompt,
  fork-on-next-send, and workspace input history.
- Interactive inline diff previews with approve/reject/edit workflow directly in
  the chat view.
- Unified permission system covering Bash, file paths, web domains, and
  sub-agent dispatch, with `default` / `accept-edits` / `trust-all` modes.
- Project and user hooks for prompt, permission, tool, compaction, turn, and
  sub-agent lifecycle automation.
- Conversation compaction that rotates segments rather than mutating the live
  buffer, so older turns stay browsable on disk.
- Persistent memory (`.mevedel/memory/` and `.agents/memory/`) and project
  instruction files (`AGENTS.md`) for customizing LLM behavior.
- `@ref`, `@file`, `@agent`, and `@mcp` mention syntax in chat buffers.

[output.webm](https://github.com/user-attachments/assets/738c9f8e-2798-466c-875e-5a77bc166a56)

## Requirements

- Emacs version 30.2 or higher
- [gptel](https://github.com/karthink/gptel) 0.9.9.5 or higher
- [gptel-agent](https://github.com/karthink/gptel-agent) 0.0.1 or higher for
  multi-agent workflows
- [ripgrep](https://github.com/BurntSushi/ripgrep) for search tools and `@file`
  directory listings
- Optional: [mcp.el](https://github.com/lizqwerscott/mcp.el) for `@mcp` mentions
- Optional: Poppler (`pdftoppm`) for rendering selected PDF pages with `Read`
- Optional: ImageMagick (`magick` or `convert`) for image/PDF resize and
  compression options in `Read`

## Installation and configuration

The package is not available on MELPA but you can install it directly from
Github using [straight.el](https://github.com/radian-software/straight.el).

``` emacs-lisp
(straight-use-package
 '(mevedel :host github
           :repo "FrauH0lle/mevedel"
           :files ("*.el"
                   "agents"
                   "prompts"
                   "skills"
                   "tools")))

(use-package mevedel
  :after (gptel gptel-agent)
  :config
  (mevedel-install))
```

`mevedel-install` is an interactive command and should be run whenever you
change one of the custom variables.

Run `mevedel-uninstall` to deactivate `mevedel`.

## Usage

mevedel’s function revolves around the creation and manipulation of references
and directives within your Emacs buffers. Directives are prompts you send to the
model, while references provide context to help complete these directives more
accurately.

The video at the [beginning](#what-does-this-package-do) of this README should
give you a good impression on the usage.

You can send your requests to the LLM either via the chat buffer (accessible by
using `mevedel`) or by creating and submitting a directive.

**Please note:** Requests sent from a directive **DO NOT** use the context of
the chat buffer, only what is defined by the directive and its references.

### Quick start

1. Configure a gptel backend.
2. Install the package and run `M-x mevedel-install`.
3. Open a project and run `M-x mevedel` to create or resume a chat session.
4. Type in the composer at the bottom of the view; use `/help` for local slash
   commands.
5. Run `/init` or `M-x mevedel-init` to bootstrap or improve project guidance.
6. Use `/goal <objective>` for supervised work, or `/goal auto <objective>`
   for guardian-approved automatic plan continuation.
7. Use `/review` or `/verify` to inspect changes before committing.

### Workflow map

The rest of this README is intentionally manual-style. The main moving parts are:

- Workspaces define the project root, allowed directories, session storage, and
  project-local context.
- References and directives provide the overlay-based workflow for attaching
  context and prompts to ordinary buffers.
- The chat view provides the interactive gptel-backed workbench for ongoing
  conversations, tool calls, permissions, previews, agents, and slash commands.
- Tools, permissions, inline previews, hooks, skills, agents, sessions,
  compaction, memory, and `@` mentions are documented below, with deeper design
  notes linked from [Documentation](#documentation).

### References and directives

References are pieces of context selected from normal buffers. Directives are
prompts that can be processed with one of mevedel's presets. A directive gathers
context from matching references, linked references, and reference commentary;
then the selected action decides whether the model may edit files, only discuss,
revise existing work, or tutor Socratically.

This means there are two complementary workflows:

- Use the chat view for conversational work with accumulated transcript context.
- Use directives when the prompt should be anchored to explicit buffer overlays
  and their selected references, independent of the chat transcript.

### Overlays

All instructions, references or directives, are highlighted in the buffer via an
overlay. The overlay contains an action menu which can be toggled via
`mevedel-ov-dispatch-key` and grants access to common operations.

![Directive Overlay](.assets/images/ov-actions-menu.png)

> [!NOTE]
> A note for [evil](https://github.com/emacs-evil/evil) users:

If you want to customize the dispatch keybind such that it uses a key only in
`normal-mode`, you will need to add something like this to your config:

``` emacs-lisp
;; Unbind default key
(setopt mevedel-ov-dispatch-key nil)
;; Use RET in normal mode to open the overlay menu
;; See https://github.com/emacs-evil/evil/issues/1477
(dolist (map mevedel--actions-maps)
  (define-key (symbol-value map)
              [return]
              `(menu-item "" mevedel--ov-actions-dispatch
                :filter ,(lambda (cmd) (when (evil-normal-state-p) cmd))))
  (define-key (symbol-value map)
              "RET"
              `(menu-item "" mevedel--ov-actions-dispatch
                :filter ,(lambda (cmd) (when (evil-normal-state-p) cmd)))))
```

| Custom Variable                      | Variable Description                                                      |
|--------------------------------------|---------------------------------------------------------------------------|
| `mevedel-ov-dispatch-key`            | Keybind to open overlay actions.                                          |
| `mevedel-instructions-truncated-max` | Maximum display length for truncated directive text.                      |

### Workspaces

mevedel assigns buffers to a project workspace which is used to determine in
which folders the LLM is allowed to operate. When the LLM needs access to a
directory outside the workspace root, it uses the `RequestAccess` tool which
prompts the user for confirmation. The default allowed roots are the workspace
root, Emacs' temporary directory, configured memory roots, the configured plans
directory, and any roots granted for the session.

| Command                       | Command Description                                                |
|-------------------------------|--------------------------------------------------------------------|
| `mevedel-add-project-root`    | Add a directory to the additional allowed roots for the workspace. |
| `mevedel-remove-project-root` | Remove a directory from the additional allowed roots.              |
| `mevedel-list-project-roots`  | Display all currently allowed roots for the workspace.             |

| Custom Variable                      | Variable Description                                                      |
|--------------------------------------|---------------------------------------------------------------------------|
| `mevedel-workspace-functions`        | Functions to determine the workspace for the current buffer.              |
| `mevedel-workspace-types-alist`      | Alist mapping workspace types to their defining functions.                |
| `mevedel-workspace-additional-roots` | Alist mapping workspace roots to lists of additional allowed directories. |
| `mevedel-file-cache-max-entries`     | Maximum number of files kept in the workspace file cache.                |
| `mevedel-file-cache-max-bytes`       | Maximum total content size kept in the workspace file cache.             |

### Management

| Command                           | Command Description                                                   |
|-----------------------------------|-----------------------------------------------------------------------|
| `mevedel-create-reference`        | Create or resize a reference instruction within a region.             |
| `mevedel-create-directive`        | Create or resize a directive instruction at point or within a region. |
| `mevedel-delete-instructions`     | Remove instructions either at point or within the selected region.    |
| `mevedel-delete-all-instructions` | Delete all mevedel instructions across all buffers.                   |
| `mevedel`                         | Start or switch to a chat session; prefix arg prompts for directory/session. |
| `mevedel-in-directory`            | Start or switch to a chat session rooted in a workspace subdirectory. |
| `mevedel-tutoring`                | Start a tutoring chat session in the current workspace.               |
| `mevedel-init`                    | Bootstrap or refresh project instruction files.                       |
| `mevedel-process-directives`      | Process multiple directives sequentially (region, point, or buffer).  |
| `mevedel-abort`                   | Abort any active request in the current buffer.                       |
| `mevedel-version`                 | Show (or insert with prefix arg) the current mevedel version.         |

| Custom Variable               | Variable Description                                             |
|-------------------------------|------------------------------------------------------------------|
| `mevedel-default-chat-preset` | Default preset when `mevedel` is invoked without prefix arg.     |
| `mevedel-user-dir`            | Global user-state directory for skills, config, and permissions. |

- If the region mark started from outside the reference/directive overlay and a
  part of it is within the selected region, the instruction will be "shrunk" to
  fit the new region boundaries.
- If the region mark started from inside the reference/directive overlay, the
  instruction will grow to include the entire new selection.

Below is an example of scaling existing instruction overlay (in this case, a
reference) by invoking the `mevedel-create-reference` command within a region
that contains one:

[instruction-scaling.webm](https://github.com/user-attachments/assets/7b2f0966-184a-4eda-ad26-8cfc500f9e1d)

### Sessions

Each chat lives in its own session under
`<workspace>/.mevedel/sessions/<name>-<timestamp>-<id>/`. Sessions auto-save at
turn boundaries, keep tracked-file backups, and can be reopened, renamed, or
rewound to any earlier prompt. Sending after a rewind forks the session,
preserving the original verbatim. Session directories use lock files; if another
Emacs owns the session, resume can open it read-only instead of corrupting the
writer's transcript.

| Command                  | Command Description                                                |
|--------------------------|--------------------------------------------------------------------|
| `mevedel-resume`         | Resume the most recent saved session, or pick one with prefix arg. |
| `mevedel-save-session`   | Force a save; with prefix arg, save a copy under a fresh id.        |
| `mevedel-rename-session` | Rename the current session and its on-disk directory.              |
| `mevedel-rewind`         | Pick a previous prompt in the current session and rewind to it.    |

| Custom Variable                          | Variable Description                                  |
|------------------------------------------|-------------------------------------------------------|
| `mevedel-sessions-directory`             | Directory for sessions (default `.mevedel/sessions/`). |
| `mevedel-session-max-age-days`           | Auto-cleanup age, in days. `nil` disables.             |
| `mevedel-file-history-max-snapshots`     | Per-session file backup retention.                     |
| `mevedel-file-history-max-snapshot-bytes` | Maximum size for an individual file snapshot.         |
| `mevedel-view-input-history-size`        | Size of the workspace input history ring.              |

The chat view provides comint-style input history: `M-p` / `M-n` cycle previous
and next inputs, `M-r` searches, `C-c C-l` browses the ring. History persists
per workspace as `.mevedel/input-history.el`.

### Chat View

mevedel runs gptel in an internal Org data buffer, but presents a compact
`mevedel-view-mode` buffer for day-to-day use. The view keeps the editable input
at the bottom, renders tools and agent activity as collapsible rows, and preserves
the raw data buffer for persistence and gptel state. mevedel data buffers are
kept linear: `gptel-org-branching-context` is set buffer-locally to `nil`, so Org
headings in transcripts do not change the request context.

`Shift-TAB` cycles the session permission mode from the input prompt. If you
submit while a request is running, the prompt is queued for the next turn and can
be edited or cleared before it is drained. Dropping a local file into the view
inserts an `@file` mention and grants a one-shot exact-file `Read` permission for
the next send.

| Command                              | Command Description                                      |
|--------------------------------------|----------------------------------------------------------|
| `mevedel-view-send`                  | Send the current view input to the backing data buffer.  |
| `mevedel-view-abort`                 | Abort the active request from the view buffer.           |
| `mevedel-view-toggle-section`        | Expand or collapse the section or turn at point.         |
| `mevedel-view-cycle-permission-mode` | Cycle the current session's permission mode.             |
| `mevedel-view-edit-last-queued-message` | Move the queued follow-up batch back into the composer. |
| `mevedel-view-clear-queued-messages` | Clear queued follow-up prompts for the current session.  |
| `mevedel-view-toggle-transcript`     | Switch from the view to the raw data buffer.             |
| `mevedel-view-rerender`              | Debounced full refresh of the rendered view.             |

| Custom Variable                                | Variable Description                                               |
|------------------------------------------------|--------------------------------------------------------------------|
| `mevedel-view-fontify-responses`               | Fontify assistant response bodies using Org-style faces.           |
| `mevedel-view-pending-tools-visible-max`       | Maximum live `Calling X…` rows shown before summarizing the rest.  |
| `mevedel-view-stream-render-delay`             | Debounce delay for incremental streaming view refreshes.           |
| `mevedel-view-rerender-debounce`               | Debounce delay for explicit full view rerenders.                   |
| `mevedel-view-mailbox-collapse-line-threshold` | Line threshold for initially collapsed agent mailbox deliveries.   |
| `mevedel-view-spinner-animate`                 | Whether the live spinner animates.                                 |
| `mevedel-view-spinner-interval`                | Seconds between spinner animation frames.                          |
| `mevedel-view-spinner-frames`                  | Strings used as spinner animation frames.                          |

A recommended `.gitignore` line is `.mevedel/sessions/` (or just `.mevedel/`).

### Saving & Loading

| Command                     | Command Description                                    |
|-----------------------------|--------------------------------------------------------|
| `mevedel-save-instructions` | Save current instruction overlays to a specified file. |
| `mevedel-load-instructions` | Load instruction overlays from a specified file.       |
| `mevedel-instruction-count` | Return the number of instructions currently loaded.    |

| Custom Variable                       | Variable Description                                             |
|---------------------------------------|------------------------------------------------------------------|
| `mevedel-patch-outdated-instructions` | Automatically patch instructions when the save file is outdated. |
| `mevedel-instruction-anchor-context-chars` | Surrounding context stored with instruction anchors.       |
| `mevedel-instruction-anchor-text-max-chars` | Maximum selected text stored directly in anchors.         |

The variable `mevedel-patch-outdated-instructions` controls the automatic
patching of instructions during loading when the save file is outdated. The
process is not perfect (word-wise diff), so you should always try and maintain a
separate instruction file per branch.

### Modification

| Command                               | Command Description                                               |
|---------------------------------------|-------------------------------------------------------------------|
| `mevedel-convert-instructions`        | Convert between reference and directive types at point.           |
| `mevedel-modify-directive`            | Modify an existing directive instruction at point.                |
| `mevedel-modify-reference-commentary` | Modify reference commentary at the current point.                 |
| `mevedel-add-tags`                    | Add tags to the reference under the point.                        |
| `mevedel-remove-tags`                 | Remove tags from the reference under the point.                   |
| `mevedel-modify-directive-tag-query`  | Enter a tag search query for a directive under the current point. |
| `mevedel-link-instructions`           | Link instructions by their ids.                                   |
| `mevedel-unlink-instructions`         | Unlink instructions by their ids.                                 |

| Custom Variable                            | Variable Description                                 |
|--------------------------------------------|------------------------------------------------------|
| `mevedel-empty-tag-query-matches-all`      | Determines matching behavior of queryless directives |
| `mevedel-always-match-untagged-references` | Determines matching behavior of untagged references  |

#### Categorization

[tag-query.webm](https://github.com/user-attachments/assets/a5492895-3e20-45b2-bf16-712caacee9e2)

The categorization system in allows you to use tags to label and organize
references. You can add or remove tags to a reference using the commands
`mevedel-add-tags` and `mevedel-remove-tags`. Each tag is a symbolic label that
helps identify the nature or purpose of the reference.

You can also modify the tag query for a directive, which is a way to filter and
search for references by tags. The tag query uses an infix notation system,
allowing complex expressions with the operators `and`, `or`, and `not`. For
example, the query `signature and function and doc` means the directive should
match references tagged with `signature`, `function`, and `doc`. You may use
parentheses in these expressions.

Additionally, there are special meta tag symbols that have exclusive meanings:

- `is:bufferlevel`: Returns only references that contain the entire buffer.
- `is:tagless`: Returns references with no tags whatsoever.
- `is:directly-tagless`: Returns references which may have inherited tags, but
  no tags of their own.
- `is:subreference`: Returns references which have another reference as their
  parent.
- `is:with-commentary`: Returns references that directly contain commentary
  text.
- `id:<positive-integer>`: Returns references the id matched by
  `positive-integer`.

`mevedel-empty-tag-query-matches-all` determines the behavior of directives
without a tag search query. If set to `t`, directives lacking a specific tag
search query will use all available references. Alternatively, if set to `nil`,
such directives will not use any references, leading to potentially narrower
results.

`mevedel-always-match-untagged-references` controls the inclusion of untagged
references in directive prompts. When set to `t`, untagged references are always
incorporated into directive references, ensuring comprehensive coverage.
Conversely, when set to `nil`, untagged references are ignored unless
`mevedel-empty-tag-query-matches-all` is set to `t`.

#### Commentary

You can add commentaries to references with the
`mevedel-modify-reference-commentary` command. Commentaries can add extra context
and information to a reference. Example:

![Ref commentary](.assets/images/ref-commentary.png)

#### Linking

References can be linked to one another, which sets up a dependency or of
automatic inclusion relationship between the two. This means that when the first
reference is utilized, it will automatically bring into play the reference it is
linked to, as well. This chaining of references is recursive: if a linked
reference is itself linked to another, and so forth, all these links will be
followed automatically. This continues until either there are no more links to
follow or a cycle is detected in the linkage graph.

Linked references are also included when a directive is executed from within a
reference which links to another, in a similar fashion to commentaries.

Currently, linking is only relevant for references.

### Processing

| Command                            | Command Description                                                          |
|------------------------------------|------------------------------------------------------------------------------|
| `mevedel-implement-directive`      | Implement directive with full editing capabilities.                          |
| `mevedel-revise-directive`         | Revise directive with additional context from existing patches.              |
| `mevedel-discuss-directive`        | Discuss directive in read-only mode without making changes.                  |
| `mevedel-tutor-directive`          | Tutoring mode that guides without providing direct solutions (experimental). |
| `mevedel-preview-directive-prompt` | Preview directive prompt at the current point.                               |
| `mevedel-diff-apply-buffer`        | Apply the diff in the patch buffer with overlay preservation.                |
| `mevedel-ediff-patch`              | Launch an ediff session on the patch buffer for manual editing.              |
| `mevedel-clear-patch-buffer`       | Clear the contents of the patch buffer for the current workspace.            |

| Custom Variable                     | Variable Description                                       |
|-------------------------------------|------------------------------------------------------------|
| `mevedel-include-full-instructions` | Controls if instructions are fully included in the prompt  |
| `mevedel-show-patch-buffer`         | Controls if patch buffer should be displayed automatically |
| `mevedel-show-chat-buffer`          | Controls if chat buffer should be displayed automatically  |
| `mevedel-action-preset-alist`       | Alist mapping actions to presets.                          |

You can use the `mevedel-preview-directive-prompt` command to do a dry-run and
see how the AI prompt will look like. Here's an example of previewing a
directive prompt:

[directive-preview.webm](https://github.com/user-attachments/assets/72c77cfa-5a6d-45a1-9fc4-ee6b1ad66034)

The `mevedel-implement-directive`, `mevedel-revise-directive`,
`mevedel-discuss-directive`, or `mevedel-tutor-directive` commands will process
the directive.

Note: The tutoring preset is experimental and uses a Socratic questioning
approach to guide learning rather than providing direct solutions.

Commands are also available via overlay actions. For example, you can preview
the patch before applying it or running an `ediff` session with the patch and
modify it to your liking.

### Navigation

| Command                               | Command Description                                         |
|---------------------------------------|-------------------------------------------------------------|
| `mevedel-next-instruction`            | Cycle through instructions in the forward direction.        |
| `mevedel-previous-instruction`        | Cycle through instructions in the backward direction.       |
| `mevedel-next-reference`              | Cycle through references in the forward direction.          |
| `mevedel-previous-reference`          | Cycle through references in the backward direction.         |
| `mevedel-next-directive`              | Cycle through directives in the forward direction.          |
| `mevedel-previous-directive`          | Cycle through directives in the backward direction.         |
| `mevedel-cycle-instructions-at-point` | Cycle through instructions at the point, highlighting them. |

### Customization

| Custom Variable                                  | Variable Description                             |
|--------------------------------------------------|--------------------------------------------------|
| `mevedel-reference-color`                        | Tint color for reference overlays                |
| `mevedel-directive-color`                        | Tint color for directive overlays                |
| `mevedel-directive-processing-color`             | Tint color for directives being processed        |
| `mevedel-directive-success-color`                | Tint color for successfully processed directives |
| `mevedel-directive-fail-color`                   | Tint color for failed directives                 |
| `mevedel-highlighted-instruction-color`          | Tint color for highlighted/focused instructions  |
| `mevedel-instruction-bg-tint-intensity`          | Intensity for instruction background tint        |
| `mevedel-instruction-label-tint-intensity`       | Intensity for instruction label tint             |
| `mevedel-highlighted-instruction-tint-intensity` | Intensity for highlighted instruction tint       |
| `mevedel-subinstruction-tint-coefficient`        | Coefficient applied to nested instruction tints  |

## Tools & Agents

mevedel comes with its own set of tools which are used by the LLM to process the
user's request. Each tool flows through a single pipeline that handles
permissions, validation, file snapshots, oversized-result persistence, and
display rendering.

### Available Tools

**File operations:** `Read`, `Write`, `Edit`, `MkDir`, `Glob`, `Grep`.
`Read` also supports PNG/JPG/GIF/WEBP/PDF media when the active backend can
accept native media, and can render selected PDF pages through Poppler.

**Code exploration:** `XrefReferences`, `XrefDefinitions`, `Imenu`, `Treesitter`

**User interaction:** `Ask` (ask the user a question with optional file/line
navigation), `RequestAccess` (request directory access outside workspace root)

**Tasks:** `TaskCreate`, `TaskUpdate`, `TaskNote`, `TaskList`, `TaskGet` (a
structured task list with statuses, dependencies, owner status notes, and an
optional task status fragment; use `mevedel-toggle-tasks` or `TAB`/`RET` on the
fragment to show or hide completed tasks)

**Sub-agents:** `Agent` (dispatch a registered sub-agent, foreground or
background), `SendMessage` (post a message to another agent's mailbox),
`StopAgent` (stop a running sub-agent), `ToolSearch` (look up deferred tool
schemas on demand)

**Execution:** `Bash` (with permission system, see below), `Eval` (Emacs Lisp
evaluation, always confirmed; supports `live` and `batch` modes, with optional
UI preservation for live evaluation)

**Web:** `WebSearch`, `WebFetch`, `YouTube` (via
[gptel-agent](https://github.com/karthink/gptel-agent))

**Tutor-specific:** `GetHints`, `RecordHint` (hint history for tutoring)

**Skills:** `Skill` (invoke an active skill by name from the model side)

The wrapped `gptel-agent` Emacs introspection tools are also available on
demand via the deferred-tool mechanism — they don't appear in the schema until
the LLM searches for them through `ToolSearch`, keeping the default tool list
short.

### Agents

A set of sub-agents (powered by
[gptel-agent](https://github.com/karthink/gptel-agent)) are dispatched via the
`Agent` tool, with optional `run_in_background = true` for fire-and-forget
workers. Each agent has its own tool list, prompt, and default model tier.

- `explorer`: read-only investigation of the codebase. The caller specifies
  thoroughness; the explorer returns findings without making changes.
- `coordinator`: orchestrates work by dispatching workers (foreground or
  background) and routing results via `SendMessage` mailboxes. Never
  implements directly.
- `verifier`: adversarial, read-only review. Tries to break implementations
  through edge cases, tests, and code review.
- `reviewer`: structured code review used by `/review`. Inspects diffs and
  surrounding code, then returns prioritized JSON findings.

Agent handles in the view are clickable. Running agents can open a live
transcript; finished background agents open their saved transcript when session
persistence has materialized it. `mevedel-view-close-agent-transcript` closes the
selected transcript side window.

Background agents complete fire-and-forget; their results land in the parent
agent's mailbox and the FSM parks until all live workers finish. If an agent is
no longer relevant or appears stuck, the model can use `StopAgent`, and the user
can run `mevedel-stop-agent`.

Named model tiers and workload assignments live in the current session preset.
The global `mevedel-model-tiers` and `mevedel-model-workloads` values are the
defaults for presets that omit them. A workload selects a tier or exact gptel
provider and may override reasoning effort; an `Agent` call or skill can
override policy for one invocation.

| Custom Variable                         | Variable Description                                                |
|-----------------------------------------|---------------------------------------------------------------------|
| `mevedel-agent-extra-tool-specs`        | Add active or deferred tool specs to built-in agent definitions.    |
| `mevedel-agent-background-timeout`      | Watchdog interval for stale background-agent BWAIT states.          |
| `mevedel-agent-no-progress-timeout`     | Shared foreground/background no-progress auto-stop grace period.    |
| `mevedel-agent-message-max-size`        | Maximum queued inter-agent message/result body before truncation.   |
| `mevedel-agent-view-display-action`     | Display action used for rendered agent transcript views.            |
| `mevedel-tool-ui-agent-description-width` | Maximum one-line task text width in agent handle headers.        |
| `mevedel-agent-runtime-debug`              | Log sub-agent dispatch handoffs to `*Messages*` when non-nil.       |

### Goals

`/goal <objective>` starts a supervised Goal in the current session. Each cycle
begins with read-only exploration and a `<proposed_plan>` block. mevedel keeps
that proposal in the Goal's mutable `current-plan.md` and shows the plan
approval prompt. Approval copies it to an immutable, sequential
`cycle-NNN-plan.md`, then starts implementation with the selected session
permission mode. A read-only review returns one structured verdict: `complete`
finishes the whole Goal, `continue` carries its findings into a new planning
cycle, and `blocked` stops with a concrete reason. Malformed reviews and a
successive materially identical plan pause for intervention. Goal status is
one of `active`, `paused`, `blocked`, or `complete`.

`/goal auto <objective>` uses the same lifecycle but sends every proposed
plan to a separate, tool-free Goal guardian. Only a structured `approve`
verdict can start implementation automatically, and only while the session has
no queued user input or unresolved interaction. `ask`, malformed output,
timeout, request failure, or a pending intervention opens the ordinary plan
approval prompt with the guardian reason. Guardian decisions are persisted in
the cycle index and rendered as compact transcript disclosures. Automatic Goal
approval never changes the session permission mode; unattended tool use still
requires selecting `trust-all` explicitly.

Bare `/goal` shows the current lifecycle position, or prompts for an objective
when no Goal exists. `/goal edit <objective>` preserves identity and cycle
history but pauses for explicit review; `/goal pause` waits for the active
request to settle before stopping continuation; `/goal resume [context]`
continues from the saved safe phase; and `/goal clear` removes only current
lifecycle state. Clear and replacement never delete transcript, plan artifacts,
or filesystem work. Starting over an unfinished Goal requires confirmation,
while a complete Goal is replaced directly. Aborting an active request pauses
its Goal.

### Review and Verify Commands

`M-x mevedel-review` / `/review` and `M-x mevedel-verify` / `/verify` run
dedicated foreground validation tasks. The commands share a target picker:
uncommitted changes, a base branch, a specific commit, the last commit, or
custom instructions. Base-branch targets pre-resolve the merge-base SHA and pass
that concrete diff target to the agent. The path is first-class rather than
normal skill resolution, so project/user skills named `review` do not override
the review command.

`/review` uses the `reviewer` agent and returns strict JSON findings. mevedel
renders those findings as a normal assistant summary and also stores a hidden
synthetic review action in the parent transcript so follow-up prompts like "fix
finding 2" have the full review context available.

`/verify` uses the `verifier` agent and verifier-style wording: inspect
adversarially, run or recommend relevant checks when allowed, and finish with a
`VERDICT: PASS`, `VERDICT: FAIL`, or `VERDICT: PARTIAL` line. Its output is
inserted as-is rather than parsed as review JSON.

Both commands receive Git inspection grants for read-only commands (`git diff`,
`git status`, `git log`, `git show`, `git merge-base`, `git rev-parse`,
`git ls-files`, `git cat-file`), plus `head` for bounded object inspection and
the explicit `GIT_PAGER=cat git diff ...` form. Review adds a local deny rule for
other Bash commands; verify leaves validation commands to the normal permission
policy.

While the agent runs, the parent view shows a live inline `Review` or `Verify`
handle with transcript status and tool-call counts. The final response remains
the readable agent summary.

### Inline Diff Preview

![Edit tool](.assets/images/edit-tool.png)

When the LLM proposes file edits via the `Write` or `Edit` tools, a diff preview
is shown for user approval before any changes are applied. Small diffs are shown
inline in the chat view; larger diffs open in a separate preview buffer
(controlled by `mevedel-inline-preview-threshold`). Under `accept-edits` and
`trust-all` permission modes the diff is auto-applied and a summary entry is
still added to the view. `MkDir` goes through the permission system, but creates
directories directly rather than showing a diff preview.

Per-overlay keybindings:

| Key                     | Action                                                |
|-------------------------|-------------------------------------------------------|
| `C-c C-c` / `a` / `RET` | Approve and apply the diff                            |
| `C-c C-k` / `r` / `q`   | Reject the diff                                       |
| `C-c C-e` / `e`         | Edit the diff via ediff before apply                  |
| `C-c C-f` / `f`         | Provide feedback and reject                           |
| `S`                     | Approve all pending and switch to `accept-edits` mode |
| `TAB`                   | Collapse / expand the preview overlay                 |

Buffer-wide commands live under the `C-c p` prefix: `n`/`p` navigate, `a`
approves all pending, `r` rejects all.

### Permission System

![Bash tool](.assets/images/bash-permission.png)

A single decision chain governs every tool dispatch — file reads/writes, Bash
commands, web fetches, sub-agent spawns. Permission rules live on the unified
`mevedel-permission-rules` alist with the form
`(TOOL-NAME &key SPECIFIER VALUE :action ACTION)`. One specifier per rule:

| Key        | Matches                | Used by                            |
|------------|------------------------|------------------------------------|
| `:path`    | path (glob, `~` exp.)  | `Read`, `Edit`, `Write`, `Glob`, … |
| `:pattern` | command string (glob)  | `Bash`                             |
| `:domain`  | host name (glob)       | `WebFetch`, `YouTube`              |
| `:name`    | free-form name (glob)  | `Agent` (subagent_type)            |

Precedence: specifier rules outrank generic; within a group `deny > ask >
allow`. Protected paths (`mevedel-protected-paths`, default `.git/`, `~/.ssh/`,
`~/.gnupg/`) always prompt regardless of mode.

**Permission modes** (`mevedel-permission-mode`):

- `default` — prompt for non-read-only tools; inline diff previews require
  interactive approval.
- `accept-edits` — same as `default`, but `Write`/`Edit` previews are
  auto-applied.
- `trust-all` — skip most prompts, except protected paths, Eval, and Bash
  commands that are unknown or require extra scrutiny.

When set inside a chat buffer, the mode is scoped to that session; set from any
other buffer it updates the global default.

```emacs-lisp
(setopt mevedel-permission-rules
        '(("Bash" :pattern "echo"     :action allow) ; bare command
          ("Bash" :pattern "echo *"   :action allow) ; with args
          ("Bash" :pattern "ls"       :action allow)
          ("Bash" :pattern "ls *"     :action allow)
          ("Bash" :pattern "git log*" :action allow) ; trailing wildcard
          ("Bash" :pattern "rm *"     :action deny)  ; explicit deny
          ("Read" :path "~/notes/**"  :action allow) ; allow outside workspace
          ("WebFetch" :domain "*.example.com" :action allow)))
```

Use space-boundary patterns (`"ls"` + `"ls *"`) rather than unbounded globs
like `"ls*"` to avoid accidentally matching unrelated commands such as `lsof`.

**Dangerous commands** (`mevedel-bash-dangerous-commands`): commands that
downgrade Bash allows to confirmation prompts (e.g., `rm`, `sudo`, `dd`,
`chmod`, `curl`). Explicit deny rules still deny immediately.

**Fail-safe mode** (`mevedel-bash-fail-safe-on-complex-syntax`): when enabled
(the default), commands with unparseable syntax (variable expansion, `eval`,
here-docs, brace expansion) automatically escalate to `ask`.

**Bash timeout** (`mevedel-bash-timeout`): maximum seconds a Bash command may
run before mevedel terminates it. Defaults to 120 seconds; `timeout_seconds`
can override it per Bash call.

**Bash guardian** (`mevedel-permission-guardian`): optional, advisory-only risk
guidance shown in Bash permission prompts. It can use the current gptel model or
a custom function, and never overrides explicit deny rules, protected paths,
Goal phase restrictions, or the user's decision.
`mevedel-permission-guardian-timeout` caps how long the prompt waits for
guidance.

Persistent rules accepted via the prompt's "always" choices are saved to
`.mevedel/permissions.el` per workspace.

### Hooks

Hooks let user or project configuration run automation around prompts, tool
calls, permission prompts, compaction, and sub-agent lifecycle events. They are
intended for local guardrails and workflow glue: block risky commands, inject
project context, run formatters after edits, annotate failed tools, or stop an
agent spawn that violates project policy.

Hook config can be written as Lisp data or JSON:

- `mevedel-hook-rules`: trusted user configuration.
- `~/.mevedel/hooks.el` and `~/.mevedel/hooks.json`: trusted user files.
- `<workspace>/.mevedel/hooks.el` and
  `<workspace>/.mevedel/hooks.json`: project files. By default, these must be
  trusted with `mevedel-hooks-trust-project` before command hooks run.
- Skill frontmatter `hooks`: active only while that skill is preparing or
  running.
- Agent definition `:hooks`: active only for invocations of that registered
  agent.

If both `.el` and `.json` exist in the same layer, mevedel merges them
additively. Command hooks run with JSON on stdin and may print a JSON decision
on stdout. Elisp hooks receive a plist and return a plist.

Available events:

| Event                 | When it runs                                  | Matcher           | Useful for                            |
|-----------------------|-----------------------------------------------|-------------------|---------------------------------------|
| `SessionStart`        | session creation or resume                    | source            | add session context                   |
| `UserPromptSubmit`    | before a view prompt is sent                  | none              | block, rewrite, or add context        |
| `UserPromptExpansion` | before a user-invoked skill prompt is sent    | none              | block, rewrite, or add context        |
| `PreToolUse`          | after validation, before permission           | tool name         | block, ask, rewrite args, add context |
| `PermissionRequest`   | before a permission prompt                    | tool name         | allow, deny, or force ask             |
| `PermissionDenied`    | after a tool is denied                        | tool name         | explain or log the denial             |
| `PostToolUse`         | after a successful tool result is shaped      | tool name         | add context or replace result         |
| `PostToolUseFailure`  | after an `Error:` tool result                 | tool name         | add recovery hints or replace result  |
| `PreCompact`          | before manual or automatic compaction         | `manual` / `auto` | block or add summary instructions     |
| `PostCompact`         | after compaction completes                    | `manual` / `auto` | log or notify                         |
| `SubagentStart`       | before an `Agent` request starts              | agent type        | block or add agent context            |
| `SubagentStop`        | after an agent reaches terminal status        | agent type        | log or notify                         |
| `Stop`                | after a successful top-level assistant turn   | none              | log, cleanup, or notify               |
| `StopFailure`         | after an errored or aborted top-level turn    | none              | log, cleanup, or notify               |
| `SessionEnd`          | buffer kill or session teardown               | reason            | cleanup or notify                     |

In skill or agent-local hook declarations, `Stop` is scoped to the child
invocation and normalized to `SubagentStop`. Top-level `Stop` only belongs to
the main assistant turn.

Common decision fields:

| Field                                           | Meaning                                                  |
|-------------------------------------------------|----------------------------------------------------------|
| `:continue nil` / `"continue": false`           | Stop the current operation where supported.              |
| `:stop-reason` / `"stopReason"`                 | User-facing reason for a block.                          |
| `:additional-context` / `"additionalContext"`   | Model-visible context to inject.                         |
| `:permission-decision` / `"permissionDecision"` | `allow`, `deny`, or `ask`.                               |
| `:permission-reason` / `"permissionReason"`     | Reason shown to the model/user for permission decisions. |
| `:updated-input` / `"updatedInput"`             | Replace a prompt or, for `PreToolUse`, tool arguments.   |
| `:updated-result` / `"updatedResult"`           | Replace a post-tool result.                              |

Example project `.mevedel/hooks.el`:

```emacs-lisp
((PreToolUse
  ((:matcher "Bash"
    :hooks ((:type command
             :command ".mevedel/hooks/block-rm.sh"
             :timeout 5
             :fail-closed t)))))
 (PostToolUse
  ((:matcher "Edit|Write"
    :hooks ((:type command
             :command ".mevedel/hooks/format-changed-file.sh"
             :timeout 30))))))
```

Example project `.mevedel/hooks.json`:

```json
{
  "hooks": {
    "PermissionRequest": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": ".mevedel/hooks/allow-safe-git.sh",
            "timeout": 5
          }
        ]
      }
    ],
    "PreCompact": [
      {
        "matcher": "auto",
        "hooks": [
          {
            "type": "command",
            "command": ".mevedel/hooks/keep-test-summary.sh"
          }
        ]
      }
    ]
  }
}
```

Example `PreToolUse` shell guard:

```bash
#!/usr/bin/env bash
set -euo pipefail

payload="$(cat)"
if printf '%s' "$payload" | grep -q '"command"[[:space:]]*:[[:space:]]*"rm -rf'; then
  printf '{"continue":false,"stopReason":"rm -rf is blocked by project hooks"}\n'
fi
```

Example `PermissionRequest` shell helper using `jq`:

```bash
#!/usr/bin/env bash
set -euo pipefail

cmd="$(jq -r '.tool_input.command // ""')"
case "$cmd" in
  "git status"*|"git diff"*|"git log"*)
    printf '{"permissionDecision":"allow","permissionReason":"read-only git command"}\n'
    ;;
esac
```

Example user-level Elisp hook:

```emacs-lisp
(add-hook 'mevedel-user-prompt-submit-functions
          (lambda (_event)
            '(:additional-context
              "Project policy: before reporting completion, mention which tests ran.")))
```

Example expansion hook for user-invoked skills:

```emacs-lisp
(add-hook 'mevedel-user-prompt-expansion-functions
          (lambda (event)
            (when (equal (plist-get event :skill-name) "review")
              '(:additional-context
                "For review prompts, prioritize regressions and missing tests."))))
```

Example agent-local hook:

```emacs-lisp
(mevedel-define-agent verifier
  :description "Read-only verification"
  :tools (read)
  :hooks ((Stop
           ((:matcher "*"
             :hooks ((:type elisp :function my-verifier-finished)))))))
```

Hook context injected by `UserPromptSubmit` is shown in the chat view as a
collapsed `◇ hook context added` disclosure. Tool calls blocked by
`PreToolUse` or `PermissionRequest` show the blocking event and reason on the
tool summary line. Hook run details are kept in the session hook log and, once a
session is materialized on disk, appended to `<session>/hook-log.el`.

Useful commands:

| Command                       | Description                                              |
|-------------------------------|----------------------------------------------------------|
| `mevedel-hooks-list`          | Show effective hooks for the current session.            |
| `mevedel-hooks-run-dry`       | Preview which hooks match an event without running them. |
| `mevedel-hooks-trust-project` | Trust the current project's hook files by hash.          |

### Examples

#### Ask tool

![Ask tool 1](.assets/images/ask-tool-1.png)
![Ask tool 2](.assets/images/ask-tool-2.png)

#### Task list

![Task list](.assets/images/todo-tool.png)

### Customization

| Custom Variable                            | Variable Description                                                     |
|--------------------------------------------|--------------------------------------------------------------------------|
| `mevedel-inline-preview-threshold`         | Ratio of chat buffer height to use for inline preview threshold.         |
| `mevedel-deferred-tool-ttl`                | Turns a ToolSearch-loaded deferred tool stays active after last use.     |
| `mevedel-permission-rules`                 | Unified permission rules (path / pattern / domain / name specifiers).    |
| `mevedel-permission-mode`                  | Default permission mode (`default` / `accept-edits` / `trust-all`).       |
| `mevedel-protected-paths`                  | Path globs that always require confirmation.                             |
| `mevedel-bash-dangerous-commands`          | Commands that always require explicit confirmation.                      |
| `mevedel-bash-fail-safe-on-complex-syntax` | When non-nil, always ask for permission when complex syntax is detected. |
| `mevedel-bash-timeout`                     | Seconds before a Bash command is terminated; nil disables timeouts.      |
| `mevedel-permission-guardian`              | Add advisory Bash risk guidance to permission prompts.                   |
| `mevedel-permission-guardian-timeout`      | Seconds to wait for Bash guardian guidance before showing the prompt.    |
| `mevedel-eval-expression-display-limit`    | Lines of an `Eval` expression to show in the confirmation prompt.        |
| `mevedel-model-tiers`                      | Default named provider/effort tier map inherited by session presets.     |
| `mevedel-model-workloads`                  | Default tier/provider/effort policy per model workload.                  |
| `mevedel-preset-extra-tool-specs`          | Add active or deferred tool specs to built-in presets.                   |
| `mevedel-hook-rules`                       | Trusted user-level declarative hook rules.                               |
| `mevedel-hooks-require-project-trust`      | Require explicit trust before project hook files run.                    |
| `mevedel-hooks-command-timeout`            | Default timeout for command hooks.                                       |
| `mevedel-hooks-command-timeout-max`        | Maximum per-hook command timeout.                                        |
| `mevedel-hooks-command-output-max-chars`   | Maximum captured stdout/stderr bytes per command hook.                   |
| `mevedel-hooks-log-limit`                  | Number of hook log entries kept in memory per session.                   |
| `mevedel-hooks-persist-log`                | Append hook logs to persisted session directories.                       |
| `mevedel-hooks-slow-threshold`             | Seconds before a slow hook run is surfaced to the user.                  |
| `mevedel-reminders-edited-file-max-diff-lines` | Maximum diff lines per externally edited file reminder.              |

## Skills

A skill is a reusable prompt package described by a `SKILL.md` file. Skills are
discovered from `.mevedel/skills/`, `.agents/skills/`,
`~/.mevedel/skills/`, `~/.agents/skills/`, and from the directories listed in
`mevedel-skill-dirs`. mevedel ships a few bundled skills under `skills/`
(for example `coordinator`,
`review`, `analyze-log`, and `remember`); name conflicts are exposed with
deterministic visible prefixes.

A skill can:

- Be invoked in the chat input (`$<skill-name>`) or as a
  model-side `Skill` tool call.
- Inline its body into the current request, or fork into a sub-agent (with
  `context: fork` and an optional `agent: <name>`).
- Augment permissions for the duration of the invocation via `allowed-tools`.
- Override the model for that invocation. Skills can also declare effort
  metadata; it is parsed and stored, but currently inert until gptel exposes a
  reasoning-effort control.

User skill invocations may block chat input while async preparation or a foreground
fork completes.

Built-in local slash commands include `/help`, `/clear`, `/tokens`, `/model`,
`/compact`, `/mode`, `/auto`, `/goal`, `/init`, `/review`, and `/verify`.
Project and user skills add `$<skill-name>` invocations by name.

Skill frontmatter can also declare file `paths`, shell commands, hooks, model and
effort metadata, and whether a skill runs inline or in a forked agent. Skill
bodies support argument placeholders and shell/Elisp insertions; see
[`docs/skills.md`](docs/skills.md) for the full format.

| Custom Variable                   | Variable Description                                            |
|-----------------------------------|-----------------------------------------------------------------|
| `mevedel-skill-dirs`              | Directories scanned for `SKILL.md` files.                       |
| `mevedel-skills-include-bundled`  | Whether to scan mevedel's bundled `skills/` directory.          |
| `mevedel-skills-check-for-modifications` | When non-nil, hot-reload skills on file changes.         |
| `mevedel-skills-max-recursion-depth` | Maximum depth for nested skill invocations.                  |
| `mevedel-skills-listing-budget` | Context fraction reserved for the model-visible skill list.       |
| `mevedel-skills-listing-max-entry-chars` | Maximum characters per skill entry in that listing.      |

## Conversation Compaction

![Compact 1](.assets/images/compaction-tool-2.png)

Long chat sessions can accumulate significant token usage. The `mevedel-compact`
command summarizes old conversation history via an LLM call. Persisted sessions
also auto-compact before a request when the estimated context crosses the
configured threshold. During auto-compaction the view spinner shows
`Compacting...`, then the original request continues.

When the session is persisted, compaction rotates segments rather than rewriting
the live buffer: the current segment is finalized on disk, the counter advances
to a new `segment-NNNN.chat.org`, and the new segment starts with an anchored
summary followed by a preserved recent tail. Older segments stay browsable via
`mevedel-rewind`.

![Compact 2](.assets/images/compaction-tool-1.png)

The threshold can be absolute or proportional to usable context. mevedel uses a
local chars/4 estimate before sending requests, then corrects future estimates
from gptel's API-reported token usage when available.

| Command           | Command Description                                        |
|-------------------|------------------------------------------------------------|
| `mevedel-compact` | Summarize old conversation and reduce effective token use. |

See [`docs/compaction.md`](docs/compaction.md) for the full auto-compaction,
summary prompt, and segment-rotation contract.

| Custom Variable                   | Variable Description                                      |
|-----------------------------------|-----------------------------------------------------------|
| `mevedel-compact-auto`            | Whether persisted sessions auto-compact before requests.  |
| `mevedel-compact-context-limit`   | Optional context-window override in tokens.               |
| `mevedel-compact-token-threshold` | Absolute token count or fraction of usable context.       |
| `mevedel-compact-tail-turns`      | Target recent complete turns to preserve verbatim.        |
| `mevedel-compact-tail-budget`     | Fraction of usable context reserved for preserved tail.   |
| `mevedel-compact-reserve-tokens`  | Token headroom reserved below the model context window.   |
| `mevedel-compact-tail-tool-output-max` | Per-tool-result character cap in the preserved tail. |
| `mevedel-compact-body-tool-output-max` | Per-tool-result character cap in compaction input.   |
| `mevedel-compact-file-reference-reminder-limit` | Maximum compacted file refs cited in reminders. |
| `mevedel-compact-warn-on-completion` | Show a one-shot accuracy warning after compaction.    |

## Project Instructions and Memory

mevedel checks the workspace root for an `AGENTS.md` file. If found, its
contents are appended to the system prompt as `## Workspace Configuration`,
enabling per-project LLM behavior customization that can be checked into
version control. `AGENTS.local.md` is loaded after `AGENTS.md` for private
checkout-specific guidance.

In addition, the first 200 lines of each configured memory index are included in
every system prompt. The default memory roots are `.mevedel/memory/`,
`.agents/memory/`, `~/.mevedel/memory/`, and `~/.agents/memory/`.
`MEMORY.md` is an index; durable memory bodies live in linked topic files with
frontmatter that classifies them as user, feedback, project, or reference
memories. The bundled `$remember` skill reviews memory and proposes cleanup or
promotion changes. Memory roots are independent of session sidecars.

| Custom Variable | Variable Description |
|-----------------|----------------------|
| `mevedel-memory-dirs` | Directories scanned for persistent memory indexes. |

## @-Mentions

In chat buffers, mevedel expands `@`-prefixed mentions before sending the
prompt to the LLM. Each mention becomes a compact `[kind:KEY -- STATUS]`
placeholder, with the full content injected as a `<system-reminder>` block
above the user prompt.

- `@ref:N` / `@ref:{tag query}` — instruction reference by ID or tag.
- `@file:path` / `@file:{path with spaces}` — file or directory contents (with
  optional `#L<start>[-<end>]` to pin a line range). Directories are expanded as
  gitignore-filtered recursive listings, and supported media files are attached
  through gptel media when the backend supports them. Goes through the `Read`
  permission check.
- `@agent:name` — asks the main agent to delegate via `Agent(subagent_type=…)`.
- `@mcp:server:uri` — attaches an MCP resource via mcp.el.

Completion at point is provided for valid IDs, file paths, agent names, and
MCP servers/resources.

| Custom Variable                              | Variable Description                                      |
|----------------------------------------------|-----------------------------------------------------------|
| `mevedel-file-mention-directory-max-entries` | Maximum entries included in an `@file` directory listing. |

## Where data is stored

mevedel writes workspace-local state under `.mevedel/` by default. These files
may contain prompts, responses, tool results, file snapshots, memory, and local
workflow configuration. Add `.mevedel/` to `.gitignore` unless you intentionally
want to version a specific project file such as `.mevedel/hooks.json` or a
project skill. Persisted sessions and memory are plaintext unless your filesystem
or external setup encrypts them.

| Path | Purpose |
|------|---------|
| `.mevedel/sessions/` | Persisted chat sessions, segments, sidecars, snapshots, permission logs, hook logs, and agent transcripts. |
| `.mevedel/input-history.el` | Workspace chat input history. |
| `.mevedel/memory/`, `.agents/memory/` | Memory index and topic files. |
| `.mevedel/permissions.el` | Persistent permission decisions. |
| `<session>/tool-results/` | Oversized tool outputs saved outside the transcript. |
| `.mevedel/hooks.el`, `.mevedel/hooks.json` | Project hook configuration. |
| `<session>/goals/<goal-id>/current-plan.md` | Mutable proposal for the current Goal cycle. |
| `<session>/goals/<goal-id>/cycle-NNN-plan.md` | Immutable accepted plan for one Goal cycle. |
| `<session>/goals/<goal-id>/cycles.el` | Lightweight cycle artifact, review, provider/effort, and timestamp index. |

## Documentation

The maintained detail docs live in [`docs/`](docs/):

- [`docs/architecture.md`](docs/architecture.md) — data structures, workspace
  context, gptel integration, memory, and transcript formatting.
- [`docs/view.md`](docs/view.md) — chat view model, zones, transcript rendering,
  and input history.
- [`docs/tools.md`](docs/tools.md) — tool pipeline, renderers, groups, and
  oversized-result persistence.
- [`docs/permissions.md`](docs/permissions.md) — permission decision chain,
  bucket precedence, Bash/Eval handling, and sub-agent propagation.
- [`docs/agents.md`](docs/agents.md) — built-in agents, background execution,
  mailboxes, task status, and review flow.
- [`docs/preview.md`](docs/preview.md) — inline diff preview behavior and
  keybindings.
- [`docs/mentions.md`](docs/mentions.md) — `@ref`, `@file`, `@agent`, and
  `@mcp` expansion.
- [`docs/skills.md`](docs/skills.md) — `SKILL.md` discovery, local slash
  commands, `$skill` invocation, model-side `Skill`, permissions, and forked skills.
- [`docs/hooks.md`](docs/hooks.md) — hook configuration, trust model, events,
  dry runs, and logs.
- [`docs/sessions.md`](docs/sessions.md) — persisted session layout, resume,
  rewind, fork, locking, and cleanup.
- [`docs/compaction.md`](docs/compaction.md) — manual and automatic compaction,
  summaries, and segment rotation.
- [`docs/memory.md`](docs/memory.md) — memory layout and durable-memory rules.
- [`docs/tutor.md`](docs/tutor.md) — Socratic tutoring workflow and hint
  persistence.
- [`docs/reminders.md`](docs/reminders.md) — system-reminder injection.
- [`docs/backlog.md`](docs/backlog.md) — project notes,
  todos, feature ideas, fixes, and deferred work.
- [`docs/commits.md`](docs/commits.md) — commit message format and guidelines.

## Development

Use Eask for tests and byte compilation. Clear stale bytecode before tests so old
`.elc` files cannot shadow edited source files.

```bash
npx @emacs-eask/cli clean elc
npx @emacs-eask/cli test ert test/test-*
npx @emacs-eask/cli test ert test/test-mevedel-compact.el
npx @emacs-eask/cli compile
npx @emacs-eask/cli clean elc
```

With a local Eask install:

```bash
eask test ert test/test-*
```

Test files mirror modules as `test/test-mevedel-MODULE.el`. Keep byte
compilation silent: no free-variable or unknown-function warnings.

## About the use of LLMs

This package was created with the help of AI coding tools such as Claude Code
and later, mevedel itself. Given the focus of this package, this looks like a
natural choice.

Furthermore, the author was interested in the question, if a package like this
can at some point write itself. The answer is so far a resounding *it depends*.

## Acknowledgments

This package would not exist without the foundational work of these developers:

- [daedsidog](https://github.com/daedsidog) for the original
  [evedel](https://github.com/daedsidog/evedel) package
- [Kevin Montag](https://github.com/kmontag) for the
  [macher](https://github.com/kmontag/macher) package, which provided valuable
  inspiration
- [Karthik Chikmagalur](https://github.com/karthink) for the
  [gptel](https://github.com/karthink/gptel) and
  [gptel-agent](https://github.com/karthink/gptel-agent) packages

## License

GPL-3.0-or-later. See [`LICENSE`](LICENSE).
