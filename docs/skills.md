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
- `hooks` (parsed and stored, not executed)

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

## Allowed Tools

`allowed-tools` is permission augmentation, not tool selection. It never
removes tools from the model and never denies unspecified tools.

Parsed entries become skill-scoped permission rules on the active
request or agent invocation. These buckets outrank session and
persistent rules for allow/ask resolution, while deny remains absolute
across all buckets. Plan mode suppresses skill allow grants for
non-read-only tools.

## Model And Effort

`model` can name a tier or concrete provider selector. It temporarily
overrides subsequent gptel iterations in the active request/invocation.

`effort` is parsed and stored but currently inert until gptel exposes an
effort/reasoning knob.

## Body Preparation

Argument substitution runs before execution. Shell injections support
inline `` !`cmd` `` and fenced ` ```! ` blocks. Bash shell expansion is
treated as author-written skill code and uses trusted-literal Bash
permission handling; explicit deny rules still win.

Each invocation records a `mevedel-skill-invocation-record` on the
session so compaction/replay can preserve the prepared body even if the
source `SKILL.md` changes later.
