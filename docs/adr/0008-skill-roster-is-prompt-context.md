# Skill Roster Is Prompt Context

The model-facing skill roster should be rendered as request-time prompt context,
not as an every-turn system reminder. Skills are baseline capabilities like
tools and environment context, while reminders are reserved for runtime nudges
or changes; `ListSkills(query)` remains the escape hatch when the compact
roster is omitted or too narrow. The roster should use the existing Markdown
system-section style rather than a new XML wrapper or `<system-reminder>` block,
and it should replace the old recurring skill roster reminder instead of
running in parallel with it. The same prompt section applies to main sessions
and sub-agents, rendered from the effective skill set for that invocation.

The skill-use contract belongs with the skills prompt section, under a short
`How to use skills` subsection modeled after Codex's trigger rules. It should
say that if the user names a skill with `$skill` syntax or plain text, or the
task clearly matches a listed skill description, the model must call
`Skill(name=...)` for that turn before proceeding. It should also tell the model
to call `ListSkills(query)` with a short search term when unsure which skill
applies, and to choose the minimal applicable skill set and state the order when
multiple skills apply. The model should call the minimal obvious set up front;
when uncertain, it may call `ListSkills(query)` or one skill first and decide
from the result. Skill trigger decisions do not carry across turns unless the
skill is re-mentioned or newly matched. User wording such as `$foo off` or
`don't use foo` is request-scoped instruction, not persistent skill disabling;
persistent disabling stays explicit through `/skills disable` or the skills UI.
Do not copy Codex's file-reading progressive-disclosure instructions: mevedel
models use the `Skill` tool, and the tool owns loading/preparing skill bodies.
The dynamic roster belongs at the
tail of the request-time
prompt, after the more stable dynamic sections, so provider prefix caching keeps
the stable prompt prefix useful even when available skills change. The roster is
rendered fresh from the session's effective skill set on each request rather
than cached independently. Unlike Codex, mevedel's always-on roster should not
include skill source paths; `Skill`, `ListSkills`, and `/skills help` remain the
places to inspect skill details. If there are no active model-invocable skills,
the prompt omits the skills section entirely rather than paying for empty
instructions. Disabled skills are omitted rather than shown as unavailable.
Roster entries use only the canonical invocation name plus description;
`display-name` stays UI-only so the model has one name to invoke. The roster
uses plugin-prefixed names such as `plugin:skill` when that is the canonical
visible invocation name. Entries use raw names, not `$name`, so the model passes
the correct value to `Skill(name=...)`. The roster budget should reuse mevedel's
existing budget
machinery but default to 2% of the context window, matching Codex's known-window
policy. When the roster exceeds budget, shrink descriptions first so skill names
remain visible; omit whole entries only when name-only entries still cannot fit.
In `mevedel-system.el`, register the dynamic skills prompt section after
environment, at order 50, with no cache. The producer reads the current
session's effective skills when the session matches the prompt workspace and
working directory; otherwise it returns nil.

Sub-agents use the parent session's effective skills rather than a separate
agent-specific skill store, but the skills section is rendered only when that
agent's resolved tool set includes `Skill` or `ListSkills`. Agents without skill
tools should not receive a model-facing skill roster. Separately assess which
agents should intentionally receive skill tools.

Initial built-in agent policy: explorer should receive `Skill` and `ListSkills`;
verifier, reviewer, and coordinator should not receive skill tools or a skill
roster. Discovery-only skill access is avoided because a sub-agent that cannot
invoke skills has no useful reason to inspect them.

Main presets should expose skill tools for `discuss`, `implement`, and `tutor`.
The `revise` preset can stay narrow; avoid special-casing it unless existing
tool inheritance already exposes skill tools.

The `Skill` tool remains `read-only-p`: invoking a skill prepares prompt text or
dispatches controlled sub-agent work, while any concrete writes inside that work
still pass through normal permission-gated tools.

Model-side `Skill` invocation does not fire `UserPromptExpansion`; user `$skill`
invocations do. Model invocation is already inside a model turn and should not
be treated as a user prompt expansion event.
Multiple inline attachment-style `$skill` invocations fire
`UserPromptExpansion` once per deduped attached skill in first-occurrence order.
If any hook blocks, the whole send is blocked; this preserves the existing
single-skill hook contract instead of adding an aggregate hook event.
For inline attachments, hook `:updated-input` replaces only that skill's hidden
body; leading command-style invocation keeps the existing whole-prompt rewrite
behavior.

`Skill` tool results remain model-visible as the full prepared body, while the
view keeps them collapsed to avoid transcript noise for the user.

Skill-related reminders may still exist, but only as event-shaped nudges:
roster omitted or truncated, available skills changed, or dormant path-scoped
skills became relevant after file/tool activity. They should point to
`ListSkills(query)` or an exact `Skill(name=...)` when known, not repeat the
full roster.

`ListSkills` with no query should mirror the active roster and stay capped.
`ListSkills(query)` searches all enabled model-invocable skills, including
dormant path-scoped skills absent from the active roster. Query results should
mark dormant path-scoped skills so the model can tell why they were absent from
the active roster. Exact `Skill(name=...)` or `$skill` invocation of a dormant
path-scoped skill runs it once without activating it into the session roster;
persistent activation remains tied to file/tool activity matching `paths`.
One-shot dormant invocations do not emit skill-change reminders because the
active roster did not change.

Available-skill changes should use a session `skills-snapshot`, mirroring the
existing agent roster delta pattern, so the first snapshot is silent and later
changes are reported once. The snapshot uses the same candidate set as the
dynamic roster: enabled, model-invocable, active skills. Added entries include
name and description; removed entries include name only. The snapshot is
persisted with session sidecar state, like the existing agent roster snapshot,
so resumed sessions do not report all known skills as newly added. Manual skill
enable/disable commands use the same change path as plugin, hot reload, and
path activation changes. Dormant path-scoped activation reminders should name
the activated skill and the path that made it relevant when both are known.
They fire on the next prompt after activation even though the skill also appears
in the new roster, because the reminder explains why the roster changed. This
specific path-activation reminder replaces the generic skill-change delta for
that activation so the model does not receive duplicate change messages. If one
path activates multiple dormant skills, list the activated names with the
triggering path, cap the list, and point to `ListSkills(query)` for the rest.
The activation reminder lists at most five skill names before using an
`and N more` suffix, and omits descriptions because the roster already carries
them.

Generic skill-change deltas cap added and removed lists at ten entries each,
then use an `and N more; use ListSkills(query)` suffix.

User-initiated skill invocation uses `$skill` syntax. Slash commands remain
reserved for local mevedel commands such as `/skills`, `/plugin`, and `/review`.
Leading `$foo off` is parsed as command-style invocation of `foo` with argument
`off` when `$` is the first non-whitespace character; it is not parsed as
disabling the skill. Inline `$skill` mentions elsewhere in the prompt are
attachment-style explicit invocations: mevedel prepares the named skill while
preserving the original user prompt as the prompt body; inline invocations
pass empty skill arguments and duplicate mentions of the same canonical skill
are injected once, preserving first occurrence order. Unknown `$foo` text is
sent as a normal prompt rather than rejected, so shell/environment prose such
as `$PATH` remains safe. A `$foo` mention that names a known but disabled
skill blocks the send with guidance to enable it via `/skills enable foo` or
escape it as literal text. Leading and inline explicit user skill invocations
install the same skill-scoped permission rules, hooks, model override, and
parsed effort state; only prompt body handling differs.
Quoted `"$foo"` / `'$foo'`, escaped `\$foo`, and `$foo` inside Markdown inline
code spans or fenced code blocks stay literal text for inline detection.
Inline attachment-style `$skill` is resolved by mevedel itself into additive
hidden skill context before the model request, using the same
`<system-reminder>` prompt-transform path as `@file` and `@ref` mentions. The
scanner is colocated with the mention transform but uses a separate `$skill`
parser. It does not ask the model to call `Skill(name=...)`. The transcript
keeps the user's original prompt text, while the model-visible prompt replaces
recognized inline mentions with compact placeholders such as
`[skill:to-prd -- attached]`. Inline attachment-style invocations persist the
original user text plus render metadata naming the attached skills; transformed
placeholders and hidden reminder bodies are request-time only. Inline
attachment-style invocation only accepts `context: inline` skills. If a known
user-invocable `context: fork` skill appears inline, mevedel blocks the send
with guidance to invoke it as leading `$skill ...` or escape, quote, or
code-span it for literal text. Leading command-style `$skill` can still
dispatch fork skills. Unknown `/foo` remains a strict unknown-command error
because slash is reserved for local commands.
