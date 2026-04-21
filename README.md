# mevedel

mevedel is an Emacs package that adds a visual workflow for interacting with
LLMs during programming. It is built on
[gptel](https://github.com/karthink/gptel), is versatile enough so that it can
be utilized in various types of buffers, and isn't limited to just programming
buffers.

> [!WARNING]
> `mevedel` uses a lot of tokens. Consider using a model which allows caching.

> [!WARNING]
> The package is functional but still work in progress. Expect missing features, unexpected behavior and bugs.

## What does this package do?

This is a fork of the Emacs package
[evedel](https://github.com/daedsidog/evedel) which is unfortunately no longer
maintained.

`mevedel` builds on [evedel](https://github.com/daedsidog/evedel)'s overlay
functionality and integrates it with [gptel](https://github.com/karthink/gptel)
for direct LLM interaction.

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
- Supports specialized agent workflows for focused tasks (codebase analysis,
  research, planning, Emacs introspection) via
  [gptel-agent](https://github.com/karthink/gptel-agent).
- Interactive inline diff previews with approve/reject/edit workflow directly in
  the chat buffer.
- Conversation compaction to summarize old context and reduce token usage.
- Persistent per-project memory and project instruction files (`AGENTS.md` /
  `CLAUDE.md`) for customizing LLM behavior.
- `@ref` mention syntax in chat buffers to include instruction content inline.

[output.webm](https://github.com/user-attachments/assets/738c9f8e-2798-466c-875e-5a77bc166a56)

## Requirements

- [gptel](https://github.com/karthink/gptel) 0.9.0 or higher
- [gptel-agent](https://github.com/karthink/gptel-agent) for multi-agent
  workflows
- Emacs version 30.1 or higher
- [ripgrep](https://github.com/BurntSushi/ripgrep)

## Installation and configuration

The package is not available on MELPA but you can install it directly from
Github using [straight.el](https://github.com/radian-software/straight.el).

``` emacs-lisp
(straight-use-package '(mevedel :host github :repo "FrauH0lle/mevedel" :files ("*.el")))

(use-package mevedel
  :after gptel
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

**Please note:** Requests send from a directive **DO NOT** use the context of
the chat buffer, only what is defined by the directive and its references.


### Overlays

All instructions, references or directives, are highlighted in the buffer via an
overlay. The overlay contains an action menu which can be toggled via
`mevedel-ov-dispatch-key` and grants access to common operations.

![Directive Overlay](/.assets/images/ov-actions-menu.png)

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
prompts the user for confirmation.

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

### Management

| Command                           | Command Description                                                   |
|-----------------------------------|-----------------------------------------------------------------------|
| `mevedel-create-reference`        | Create or resize a reference instruction within a region.             |
| `mevedel-create-directive`        | Create or resize a directive instruction at point or within a region. |
| `mevedel-delete-instructions`     | Remove instructions either at point or within the selected region.    |
| `mevedel-delete-all-instructions` | Delete all mevedel instructions across all buffers.                   |
| `mevedel`                         | Start a chat session in the current project.                          |
| `mevedel-tutoring`                | Start a tutoring chat session in the current project.                 |
| `mevedel-process-directives`      | Process multiple directives sequentially (region, point, or buffer).  |
| `mevedel-abort`                   | Abort any active request in the current buffer.                       |
| `mevedel-version`                 | Show (or insert with prefix arg) the current mevedel version.         |

| Custom Variable               | Variable Description                                         |
|-------------------------------|--------------------------------------------------------------|
| `mevedel-default-chat-preset` | Default preset when `mevedel` is invoked without prefix arg. |

- If the region mark started from outside the reference/directive overlay and a
  part of it is within the selected region, the instruction will be "shrunk" to
  fit the new region boundaries.
- If the region mark started from inside the reference/directive overlay, the
  instruction will grow to include the entire new selection.

Below is an example of scaling existing instruction overlay (in this case, a
reference) by invoking the `mevedel-create-reference` command within a region
that contains one:

[instruction-scaling.webm](https://github.com/user-attachments/assets/7b2f0966-184a-4eda-ad26-8cfc500f9e1d)

### Saving & Loading

| Command                     | Command Description                                    |
|-----------------------------|--------------------------------------------------------|
| `mevedel-save-instructions` | Save current instruction overlays to a specified file. |
| `mevedel-load-instructions` | Load instruction overlays from a specified file.       |
| `mevedel-instruction-count` | Return the number of instructions currently loaded.    |

| Custom Variable                       | Variable Description                                             |
|---------------------------------------|------------------------------------------------------------------|
| `mevedel-patch-outdated-instructions` | Automatically patch instructions when the save file is outdated. |

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

![Ref commentary](/.assets/images/ref-commentary.png)

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

## Tools & Agents

mevedel comes with its own set of tools which are used by the LLM to process the
user's request.

### Available Tools

**File operations:** `Read`, `Write`, `Edit`, `Insert`, `MkDir`, `Glob`, `Grep`

**Code exploration:** `XrefReferences`, `XrefDefinitions`, `Imenu`, `Treesitter`

**User interaction:** `Ask` (ask the user a question with optional file/line
navigation), `RequestAccess` (request directory access outside workspace root),
`CreatePlan` (launch the planner agent for interactive implementation planning),
`PresentPlan` (present a structured implementation plan for interactive review)

**Task tracking:** `TodoWrite`, `TodoRead` (structured task list with statuses)

**Execution:** `Bash` (with permission system, see below), `Eval` (Emacs Lisp
evaluation)

**Web:** `WebSearch`, `WebFetch`, `YouTube` (via
[gptel-agent](https://github.com/karthink/gptel-agent))

**Tutor-specific:** `GetHints`, `RecordHint` (hint history for tutoring)

### Agents

A set of agents (powered by
[gptel-agent](https://github.com/karthink/gptel-agent)) are available to
automatically handle certain tasks. The main agent delegates to these
specialists via the `Agent` tool (or `CreatePlan` for planning):

- `codebase-analyst`: Deep architectural analysis, pattern recognition, and
  dependency mapping. Has access to all read and code exploration tools but no
  web access.
- `researcher`: Online research and documentation discovery. Has access to web
  tools plus `Read`/`Grep` for cross-referencing.
- `planner`: Interactive implementation planning. Launched via the `CreatePlan`
  tool. Gathers context with read tools, then presents a structured plan via
  `PresentPlan` for user review. Users can choose to implement the plan
  directly, implement with clear context (fresh request), provide feedback to
  iterate, or abort. Accepted plans are saved to the workspace's
  `.mevedel/plans/` directory and implementation starts automatically.
- `introspector`: Emacs Lisp and Emacs runtime introspection and debugging. Has
  access to the `Eval` tool for evaluating Emacs Lisp expressions.

### Inline Diff Preview

![Edit tool](/.assets/images/edit-tool.png)

When the LLM proposes file edits via the `Write`, `Edit`, or `Insert` tools, a
diff preview is shown for user approval before any changes are applied. Small
diffs are shown inline in the chat buffer; larger diffs open in a separate
preview buffer (controlled by `mevedel-inline-preview-threshold`).

Keybindings on inline preview overlays:

| Key             | Action                                |
|-----------------|---------------------------------------|
| `C-c C-c` / `a` | Approve and apply the diff            |
| `C-c C-k` / `r` | Reject the diff                       |
| `C-c C-e` / `e` | Edit the diff via ediff before apply  |
| `C-c C-f` / `f` | Provide feedback and reject           |
| `TAB`           | Collapse / expand the preview overlay |
| `n` / `p`       | Navigate to next / previous preview   |

### Bash Permission System

![Bash tool](/.assets/images/bash-permission.png)

The bash tool uses a multi-layer permission system to control which commands the
LLM can execute.

**Permission rules** live in the unified `mevedel-permission-rules` alist. Bash
commands match via the `:pattern` specifier (glob with `*`). Precedence: rules
with a specifier outrank unqualified rules; within each group `deny` > `ask` >
`allow`. Unknown commands always prompt — the safety catchall lives inside
Bash's permission handler, so you do **not** need a `("*" ... :action ask)`
rule.

``` emacs-lisp
(setq mevedel-permission-rules
      '(("Bash" :pattern "echo"     :action allow) ; Bare command
        ("Bash" :pattern "echo *"   :action allow) ; Command with args
        ("Bash" :pattern "ls"       :action allow)
        ("Bash" :pattern "ls *"     :action allow)
        ("Bash" :pattern "git log*" :action allow) ; Trailing wildcard
        ("Bash" :pattern "rm *"     :action deny))) ; Explicitly deny rm
```

Use space-boundary patterns (`"ls"` + `"ls *"`) rather than unbounded globs
like `"ls*"` to avoid accidentally matching unrelated commands such as `lsof`.

**Dangerous commands** (`mevedel-bash-dangerous-commands`): Commands that always
require confirmation regardless of permission rules (e.g., `rm`, `sudo`, `dd`,
`chmod`, `curl`).

**Fail-safe mode** (`mevedel-bash-fail-safe-on-complex-syntax`): When enabled
(the default), commands with unparseable syntax (variable expansion, `eval`,
here-docs, brace expansion) automatically escalate to `ask`.

### Examples

#### Ask tool

![Ask tool 1](/.assets/images/ask-tool-1.png)
![Ask tool 2](/.assets/images/ask-tool-2.png)

#### Todo list tool

![Ref commentary](/.assets/images/todo-tool.png)

### Customization

| Custom Variable                            | Variable Description                                                     |
|--------------------------------------------|--------------------------------------------------------------------------|
| `mevedel-inline-preview-threshold`         | Ratio of chat buffer height to use for inline preview threshold.         |
| `mevedel-permission-rules`                 | Unified permission rules (path / pattern / domain / name specifiers).    |
| `mevedel-bash-dangerous-commands`          | Commands that always require explicit confirmation.                      |
| `mevedel-bash-fail-safe-on-complex-syntax` | When non-nil, always ask for permission when complex syntax is detected. |
| `mevedel-codebase-analyst-tools`           | Tools for the codebase-analyst agent.                                    |
| `mevedel-researcher-tools`                 | Tools for the researcher agent.                                          |
| `mevedel-planner-tools`                    | Tools for the planner agent.                                             |
| `mevedel-plans-directory`                  | Directory where accepted plans are saved (default: `.mevedel/plans/`).  |

## Conversation Compaction

![Compact 1](/.assets/images/compaction-tool-2.png)

Long chat sessions can accumulate significant token usage. The `mevedel-compact`
command summarizes old conversation history via an LLM call, marks the old
content as ignored (dimmed visually), and inserts a folded summary block. This
reduces token usage for future requests while preserving important context.

![Compact 2](/.assets/images/compaction-tool-1.png)

A token estimate is shown in the header line when usage approaches the
configured threshold. Token calculation is done in a naive way (4 chars = 1
token) and thus, the estimated usage should be seen as a rough estimate.
Furthermore, keep in mind that sending directives is independent of the chat
buffer.

Note, that this process needs to be triggered by the user and is not automatic.

| Command           | Command Description                                        |
|-------------------|------------------------------------------------------------|
| `mevedel-compact` | Summarize old conversation and reduce effective token use. |

| Custom Variable                   | Variable Description                                            |
|-----------------------------------|-----------------------------------------------------------------|
| `mevedel-compact-context-limit`   | Estimated maximum context window in tokens.                     |
| `mevedel-compact-token-threshold` | Token count or ratio at which a warning appears in header line. |

## Project Instructions

mevedel checks the workspace root for an `AGENTS.md` or `CLAUDE.md` file. If
found, its contents are appended to the system prompt, enabling per-project LLM
behavior customization that can be checked into version control.

## @ref Mentions

In chat buffers, you can use `@ref` syntax to reference instructions by ID or
tag. These mentions are expanded before sending to the LLM, injecting the
content of referenced instructions inline into the prompt. Completion is
available for valid reference IDs and tags.

Similarly, you can use `@file` to insert the absolute path to the selected file
or directory into the chat buffer.

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
