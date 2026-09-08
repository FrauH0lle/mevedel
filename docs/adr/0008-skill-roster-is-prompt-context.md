# Skill Roster Is Prompt Context

Status: delivery placement and snapshot acknowledgement superseded by
[ADR 0115](0115-retain-delivered-conversation-fragments.md), dynamic-context
extension (2026-09-08). Discovery, canonical names, scope, optionality and
invocation decisions below remain applicable.

The current design keeps skill dispatch policy in the stable system prompt and
delivers a compact catalog through retained current-context updates. Each entry
has its canonical name and first sentence/line (at most 160 characters); full
authored descriptions remain searchable with ListSkills. Trusted observations
actually present in the outgoing payload acknowledge delivery. The separate
session `skills-snapshot` and skill-delta reminder have been removed.

The source audit showed that ordinary catalog changes still changed the system
prefix ahead of all history. Tail placement addressed neither that invalidation
nor frozen worker freshness. Retained updates preserve earlier messages while
reporting additions, removals and empty catalogs. This is the reason for
reversing the placement and acknowledgement decisions recorded below.

## Historical decision and continuing discovery contract

The model-facing skill roster should be rendered as request-time prompt context,
not as an every-turn system reminder. Skills are baseline capabilities like
tools and environment context, while reminders are reserved for runtime nudges
or changes; `ListSkills(query)` remains the escape hatch when the compact
roster is omitted or too narrow. The roster should use the existing Markdown
system-section style rather than a new XML wrapper or `<system-reminder>` block,
and it should replace the old recurring skill roster reminder instead of
running in parallel with it. The same prompt section applies to main sessions
and sub-agents, rendered from the effective skill set for that invocation.

The skill-use contract belongs with the skills prompt section. Explicit user
requests and already prepared dependencies are honored. Other skills are
optional guidance selected for their useful scope and approach; description
and path matches are discovery signals, not compulsory workflow. Reuse relevant
loaded guidance for its task and authored applicability. A new message alone
does not end it, and user direction can extend, end, or replace it. Missing or
changed guidance is retrieved when needed. User wording such as `$foo off`
retains the scope the user gave it; persisted workspace disabling is a distinct
operation through `/skills disable` or the skills UI.

This replaces the former mandatory match-and-invoke, ordering announcement,
and one-turn lifetime rules. Captured system and Skill descriptions repeated
those rules while disagreeing about task versus turn scope. The replacement
reduces those competing obligations without rewriting skill authors' policy.
Skills remain enabled by default; the workspace denylist applies to every
visible project, user, bundled, managed, and enabled-plugin skill without
changing path-scoped catalogue state. `Skill` owns body preparation; models
need not read source files to invoke an already discovered skill.

The roster belongs at the tail of the request-time system prompt, after the
more stable sections. It includes active model-invocable skills without path
restrictions, rendered fresh from the session rather than cached independently.
Path-scoped skills remain outside this system roster regardless of file activity;
their discovery uses later optional notices and ListSkills results. Authored
configuration changes still refresh the roster, including enable/disable changes.

The 2026-09-07 lifecycle measurements found that merely placing a dynamic roster
last in the system prompt was insufficient: Luna and Sol root follow-ups added
an entire Skills section after matching file activity, then fresh restore removed
it. All conversation history followed that changed prefix despite fixed native
tool schemas. Excluding path-scoped entries from the system roster removes this
observed invalidation without freezing configuration or adding persisted state.

Unlike Codex, mevedel's always-on roster should not
include skill source paths; `Skill`, `ListSkills`, and `/skills help` remain the
places to inspect skill details. If there are no eligible skills without paths,
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
In `mevedel-system.el`, the ordered main profile places skills after
environment with no component cache; see ADR 0070. The producer reads the current
session's effective skills when the session matches the prompt workspace and
working directory; otherwise it returns nil.

Sub-agents use the parent session's effective skills rather than a separate
agent-specific skill store, but the skills section is rendered only when that
agent's resolved tool set includes `Skill` or `ListSkills`. Agents without skill
tools should not receive a model-facing skill roster. Separately assess which
agents should intentionally receive skill tools.

Initial built-in agent policy: worker and explorer should receive `Skill` and
`ListSkills`; verifier and reviewer should not receive skill tools or a skill
roster. Discovery-only skill access is avoided because an agent that cannot
invoke skills has no useful reason to inspect them.

Main presets should expose skill tools for `discuss` and `implement`.

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

Skill-related reminders report catalogue changes or optional path matches.
There is no separate budget notice: the stable roster contract advertises
`ListSkills(query)` even when no names fit, and cannot lose that instruction
through cancelled reminder staging. They should point to
`ListSkills(query)` or an exact `Skill(name=...)` when known, not repeat the
full roster.

`ListSkills` with no query lists active model-invocable skills and stays capped,
including active path-scoped skills absent from the system roster.
`ListSkills(query)` searches all enabled model-invocable skills, including
dormant path-scoped skills absent from the default listing. Query results should
mark dormant path-scoped skills so the model can tell why they were absent from
the default listing. Exact `Skill(name=...)` or `$skill` invocation of a dormant
path-scoped skill runs it once without activating its default-listing visibility;
that activation remains tied to file/tool activity matching `paths`.
Neither path activity nor one-shot dormant invocation changes the system roster
or emits its change delta.

Available-skill changes use the root session's `skills-snapshot`. The first
snapshot is silent; its acknowledgement commits only when the request payload
exists. Later additions/removals are reported once, with the same candidate
set as the root roster, and commit by the same delivery mechanism. Persisting
the snapshot prevents cold resume from treating known skills as new.

Path matching changes shared catalogue eligibility, not instructions or
recipient acknowledgement. Each conversation with callable Skill access gets
an optional notice for matching skill facts it has not been shown. Per-skill
event keys preserve discoveries across multiple paths and tool calls. Delivery
commits a buffer-local fact (name, source, description, paths), so cancelling
before dispatch permits a later matching observation to retry. An agent does
not acknowledge for its parent or siblings. Path notice delivery never mutates
the system-roster snapshot: its entries exclude path-scoped skills, so there is
no corresponding delta to suppress.

This replaces immediate shared snapshot mutation: a child could previously
suppress the parent's discovery, and a cancelled notice could suppress its
own retry. Frozen agent prompts also made the claim that their roster had
changed false. Notices now describe optional relevance and offer Skill or
ListSkills; they make no invocation or pre-action enforcement claim. Delivery
is once per live conversation and matching fact; a changed fact can rearm it.
Compaction alone does not repeat optional discovery. Cold resume starts a new
buffer throttle. The always-callable ListSkills contract remains the fallback
when a notice has left context; required skill-body fidelity is a separate
compaction responsibility.

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
escape it as literal text. A leading command contributes command-scoped
permissions and hooks. Exactly one leading command may also own the next
request's model and effort; a command stack retains session policy. Embedded
instruction mentions attach prepared skill context but do not activate command
permissions, hooks, agents, model, or effort.
Quoted `"$foo"` / `'$foo'`, escaped `\$foo`, and `$foo` inside Markdown inline
code spans or fenced code blocks stay literal text for inline detection.
Inline attachment-style `$skill` is resolved by mevedel itself from atomic
source bindings into additive hidden skill context before the model request.
It does not ask the model to call `Skill(name=...)`. The transcript keeps the
user's original prompt text, while the model-visible prompt replaces recognized
inline mentions with compact placeholders such as
`[skill:to-prd -- attached]`. Inline attachment-style invocations persist the
original user text plus render metadata containing the prepared root body and
attached-skill names and bodies. Leading command expansion also leaves its
provider-facing prompt, including required-attachment reminder wrappers, in the
canonical user run; the view uses the structured metadata instead of reparsing
those wrappers. Transformed inline-attachment placeholders are request-time
only. Inline
attachment-style invocation treats even a `context: fork` skill as a non-forking
instruction. Only a leading fork command dispatches a child. Unknown `/foo`
remains a strict unknown-command error
because slash is reserved for local commands.

Amendment: a skill author may declare recursive required instruction
attachments with literal `!$skill` syntax in `SKILL.md`. This is a deliberate,
narrow exception to the rule that skill bodies are not recursively interpreted.
The declaration attaches the dependency as context; it does not execute the
skill, invoke a tool, or dispatch an agent. Dependencies resolve and validate as
one graph, prepare dependency first in authored sibling order, and preserve the
root's user or model origin. A model-origin graph requires every node to permit
model invocation, so a parent cannot reach a model-disabled descendant. A
user-origin graph requires only its root to be user-invocable; a dependency may
be attachment-only and hidden from direct user invocation. Required children
contribute instructions only and do not activate their command/fork behavior,
agent, model, effort, hooks, or request permissions.

Amendment: inline command render metadata now stores the prepared root body and
each required attachment as structured data. The previous single flattened
prompt made wrapper recovery ambiguous when an authored attachment itself
contained a literal `<system-reminder>` example: the inner closing tag could be
mistaken for the generated outer close and split the transcript. Structured
metadata keeps presentation independent of provider wire syntax, while the
canonical transcript scanner balances nested reminder delimiters.

The exception depends on literal source provenance rather than text alone.
Escaped and Markdown-code forms, plus markers introduced by argument
substitution, injections, hooks, prepared bodies, child prompts or results, and
model output remain inert. Without that provenance boundary, generated or
untrusted text could turn into executable dependency structure and bypass the
model-invocation gate. Full-line dependency arguments may use ordinary parent
argument substitution, but the substituted result remains non-author text for
the same reason.

Amendment: Tutor mode was removed. It required the user to summon it before
knowing they needed teaching, then refused to answer what was asked, so the
chat buffer answered the same questions better without it. Its pedagogical
angle now reaches the user through Buddy notes, which arrive unasked and cost
nothing to ignore. Every tutor profile, component, preset, and tool named above
is gone; the surrounding mechanism is unchanged.
