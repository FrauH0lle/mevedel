# Skills

Skills are reusable prompt packages loaded from `SKILL.md` files.
`mevedel-skills.el` owns discovery, slash completion, model-visible
Skill tool invocation, context activation, hot reload, and skill listing
reminders.

## Discovery

Skills are scanned from configured user/project/managed/plugin dirs plus
bundled skills under `skills/` when `mevedel-skills-include-bundled` is
non-nil. User/project skills override bundled skills by name.

Bundled skills currently include:

- `coordinator` — forked orchestration skill for multi-agent work.
- `analyze-log` — user-invocable gptel HTTP log analysis helper.
- `remember` — user-invocable persistent-memory review and cleanup
  proposal helper.

`remember` is intentionally report-only: it reviews `MEMORY.md`, memory
topic files, and applicable workspace configuration, then proposes
cleanup or promotion changes. It should not edit memory unless the user
explicitly approves the report.

Skill names come from frontmatter `name` when valid, otherwise the
containing directory name. Names must match `[a-z0-9-]+`.

Hot reload marks consuming chat buffers dirty when watched skill
directories change. Completion and reminders rescan on demand when a
buffer is dirty.

## Frontmatter

Current fields include:

- `name`, `display-name`, `description`, `when_to_use` /
  `when-to-use`
- `argument-hint`, `arguments`
- `user-invocable`, `disable-model-invocation`
- `allowed-tools`
- `model`, `effort`
- `context`, `agent`
- `paths`
- `shell`
- `hooks` (skill-scoped hooks active during invocation)

`paths` gates model-listing visibility only. Explicit slash or
model-side invocation by name can still run the skill subject to the
user/model invocation gates.

## Invocation

`mevedel-skills-invoke` is the unified internal entry point for slash,
model-side Skill tool, and internal invocation.

- `context: inline` prepares the body and injects or returns it in the
  current request context.
- `context: fork` dispatches a foreground sub-agent directly. It does
  not ask the model to call `Agent` in prose. If `agent` names a
  registered agent, that agent is used; otherwise mevedel synthesizes an
  agent that inherits the parent context.

Slash invocation may block chat input while async preparation or a
foreground fork completes. Model-side Skill blocks the parent tool call.

Inline slash invocations keep the expanded body in the data buffer, then
append an ignored `<!-- mevedel-render-data -->` side-channel block with
the original skill name and arguments. The view buffer renders that as a
compact slash invocation user turn plus a collapsed `Prompt` section
containing the full expanded body.

User slash skill invocations fire `UserPromptExpansion` after body
preparation and before the expanded prompt reaches the model. This covers
both inline slash skills and foreground `context: fork` slash skills.
Hooks can block the expansion, replace the expanded prompt with
`:updated-input`, or append `<hook-context>` with `:additional-context`.
When a slash expansion is blocked, pending skill-scoped permission/model
and hook context is cleared instead of leaking into the next request.
Model-side Skill calls do not fire this event.

## Allowed Tools

`allowed-tools` is permission augmentation, not tool selection. It never
removes tools from the model and never denies unspecified tools.

Parsed entries become skill-scoped permission rules on the active
request or agent invocation. These buckets outrank session and
persistent rules for allow/ask resolution, while deny remains absolute
across all buckets. Plan mode suppresses skill allow grants for
non-read-only tools.

## Hooks

Skill frontmatter `hooks` uses the same event -> matcher -> handler shape
as `.mevedel/hooks.el` and `.mevedel/hooks.json`. Inline skills install
those rules on the active request while the skill is preparing or running.
Fork skills install them on the sub-agent invocation. For fork skills, a
frontmatter `Stop` declaration is scoped to that child invocation and is
normalized to `SubagentStop`; top-level `Stop` remains a main-turn event.
Successful foreground fork slash skills also complete the parent turn and
fire the top-level `Stop` hook before request-scoped layers are cleared.

## Model And Effort

`model` can name a tier or concrete provider selector. It temporarily
overrides subsequent gptel iterations in the active request/invocation.

`effort` is parsed and stored but currently inert until gptel exposes an
effort/reasoning knob.

## Body Preparation

Argument substitution runs before execution. Body injections support
shell inline `` !`cmd` ``, shell fenced ` ```! ` blocks, elisp inline
`` !el`(emacs-version)` ``, and elisp fenced ` ```!el ` blocks. Inline
elisp is intended for short one-line expressions; fenced elisp is the
supported form for longer expressions.

Shell injections substitute trimmed stdout. Elisp injections substitute
the printed return value, with captured stdout appended in a compact
`STDOUT:` section when present.

Bash and Eval body injections are treated as author-written skill code
and use trusted-literal permission handling. Bash injections require a
covering Bash allow rule; elisp injections require `allowed-tools:
[Eval]`. Explicit deny rules still win.

Only markers written literally in `SKILL.md` are executable body
injections. Markers introduced by argument substitution or produced by
another injection's output remain literal text in the prepared body.
The marker syntax and delimiters must be author-written, but the
command/expression body may contain substituted arguments. For example,
`` !el`(length "$ARGUMENTS")` `` is valid when the skill author controls
the surrounding elisp expression. Substitution is textual, so skill
authors must quote or escape interpolated values correctly before using
them inside shell scripts or elisp strings.

Each invocation records a `mevedel-skill-invocation-record` on the
session so compaction/replay can preserve the prepared body even if the
source `SKILL.md` changes later.
