# Hooks

Hooks run configured automation at mevedel lifecycle and tool boundaries.
Use them for policy checks, formatting, linting, context injection, and
notifications. Command handlers receive JSON; native Elisp handlers receive
a mevedel event plist. The [architectural rationale](adr/0066-hooks-follow-lifecycle-boundaries.md)
explains the event boundary and ordering choices.

## Hook execution flow

```mermaid
flowchart TD
    A[Stable mevedel lifecycle event] --> B[Collect configured hook layers in precedence order]
    B --> C[Match event and target]
    C --> D[Run handlers in order]
    D --> E{Decision returned?}
    E -- No --> F[Continue]
    E -- Block or deny --> G[Surface reason and stop supported operation]
    E -- Context or rewrite --> H[Apply allowed mutation]
    H --> F
    F --> I[Record hook log]
    G --> I
```

## Event set

Supported events and their control effects:

| Event | Fires | Matcher | Control |
| --- | --- | --- | --- |
| `SessionStart` | root context epoch begins | source (`startup`, `resume`, `clear`, `compact`, `rewind`, `restore`, `fork`) | add context only |
| `UserPromptSubmit` | before a root or retained-agent task input is sent | none | block, add context, rewrite prompt |
| `UserPromptExpansion` | before a user `$skill` expansion reaches the model | none | block, add context, rewrite prompt |
| `PreToolUse` | after validation, before permission | tool name | deny, ask, add context, rewrite args |
| `PermissionRequest` | once before any permission card enters the shared queue | tool name | allow, deny, ask |
| `PermissionDenied` | once after a final tool denial | tool name | add feedback/context only |
| `PostToolUse` | after handler and result shaping | tool name | add context, replace result, mark feedback |
| `PostToolUseFailure` | after a handler reports or signals failure | tool name | add context, replace result |
| `PreCompact` | before manual/automatic compaction | trigger (`manual`, `auto`) | block, add context |
| `PostCompact` | after compaction completes | trigger | notification/logging |
| `SubagentStart` | once before a retained-agent identity is published | agent role | block, add context |
| `SubagentStop` | after each retained-agent turn reaches terminal status | agent role | notification/logging |
| `Stop` | after a successful top-level assistant turn | none | notification/logging |
| `StopFailure` | after an errored or aborted top-level assistant turn | none | notification/logging |
| `SessionEnd` | buffer kill/session teardown | reason | notification only |

## Config shape

Persistent hook configuration accepts Lisp data and JSON.  Lisp is
idiomatic for Emacs users and can name Elisp functions naturally; JSON is
natural for shell-heavy hook configs and easier to share with other agent
tools.  When both files are present in the same layer, load and merge both
additively in documented order.

Lisp shape:

```elisp
((PreToolUse
  ((:matcher "Bash"
    :hooks ((:type command
             :command ".mevedel/hooks/block-rm.sh"
             :timeout 10
             :description "Block destructive rm")
            (:type elisp
             :function my-mevedel-bash-policy)))))
 (PostToolUse
  ((:matcher "ApplyPatch"
    :hooks ((:type command
             :command ".mevedel/hooks/format-changed-file"
             :timeout 30))))))
```

Configuration sources:

- `mevedel-hook-rules`: user defcustom, Emacs-local.
- `~/.agents/hooks.el`, `~/.agents/hooks.json`,
  `~/.mevedel/hooks.el`, and `~/.mevedel/hooks.json`: user files,
  shareable across projects on one machine.
- Activated plugin manifests may contribute hooks only after mevedel has
  shown a concise consent summary for the plugin's executable hooks.
  Mevedel reads the Codex default `./hooks/hooks.json` when the manifest
  omits `hooks`, or the manifest `hooks` field when present. That field
  must be a single path.
- `<workspace>/.agents/hooks.el`, `<workspace>/.agents/hooks.json`,
  `<workspace>/.mevedel/hooks.el`, and
  `<workspace>/.mevedel/hooks.json`: project hooks, trusted per
  workspace.
- Skill frontmatter `hooks`: scoped to a command invocation. Project skill
  declarations require exact workspace/path/content trust. Instruction
  preparation ignores this field. In fork commands, a local `Stop`
  declaration is normalized to `SubagentStop`.
- Agent definition `:hooks`: scoped to invocations of that registered
  agent.  A local `Stop` declaration is normalized to `SubagentStop`.
- Session/request/invocation hook lists: transient programmatic layers.

Hook handlers run from lower-precedence roots to higher-precedence roots
so later rewrite fields preserve the package-wide resource precedence.
Layers merge additively in this order: `mevedel-hook-rules`, user
`.agents/hooks.el`, user `.agents/hooks.json`, user
`.mevedel/hooks.el`, user `.mevedel/hooks.json`, enabled plugin hooks,
trusted project `.agents/hooks.el`, trusted project `.agents/hooks.json`,
trusted project `.mevedel/hooks.el`, trusted project
`.mevedel/hooks.json`, session, request, and agent invocation. Command-skill
hooks are folded into the owned request or agent invocation. Deny decisions
remain restrictive across all layers;
allow decisions do not override existing explicit permission denies.

Within a file layer, `.el` runs before `.json` when both exist.  Ordering only
matters for hooks that rewrite tool input or results, so this order is kept
deterministic.

JSON shape:

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": ".mevedel/hooks/block-rm.sh",
            "timeout": 10,
            "failClosed": true
          }
        ]
      }
    ]
  }
}
```

## Matching

`matcher` is event-specific.  For tool events it matches the mevedel tool
name (`Bash`, `Read`, `ApplyPatch`, `Agent`, MCP names when wrapped).
For agent events it matches agent role.  For compaction it matches
`manual` or `auto`.

Matcher rules:

- nil, empty string, or `"*"` matches all.
- strings containing only letters, digits, `_`, `-`, and `|` are exact
  names or pipe-separated exact alternatives.
- any other string is an Emacs regexp matched case-sensitively.
- a regexp that does not compile is dropped with a warning naming its event,
  so a typo cannot abort the lifecycle event the group was configured under.

Handler-level `:if` predicates are unsupported.

## Handler types

`command` runs a shell command with JSON input on stdin and captures
stdout/stderr.  Execution follows the configuration resource's origin:

- trusted project-file commands run on the session target from the workspace
  root through the target's Emacs file handler;
- user defcustom commands run locally from `user-emacs-directory`, user-file
  commands run locally from the directory containing the user hook file; and
- plugin commands discovered inside the project run on the session target from
  the plugin root; client-local plugin commands remain local there.

Commands executed on a remote session target inherit that target's shell
environment.  Mevedel does not forward the client's `process-environment`;
project plugins receive only their deliberate compatibility variables in
addition to the target environment.  Client-local user and plugin commands
continue to inherit the client environment.

Loaded handlers retain `:source-file` and `:source-root` metadata so dispatch
does not infer origin from the event cwd or copy executable code between
machines.  Project-relative commands such as
`.mevedel/hooks/block-rm.sh` therefore resolve consistently regardless of the
session cwd.  A command handler without trusted project, user, or plugin
provenance is refused before launch.  Default timeout is 30 seconds with a
global cap, armed as soon as the child exists rather than after its stdin is
written; whatever settles a command handler — exit, timeout, request
cancellation, or a failed stdin write — leaves no child running and writes
one log entry.  A cancellation logs status `cancelled` and closes the
event's telemetry span without running the remaining handlers or the
caller's continuation, because the request that owned them is being torn
down.  Each
stdout/stderr stream is capped by `mevedel-hooks-command-output-max-chars`
before parsing decisions or writing log previews, so noisy hooks cannot
inject unbounded output through `updated_result` or block reasons.

Plugin command hooks receive compatibility environment variables:

- `PLUGIN_ROOT`, `CLAUDE_PLUGIN_ROOT`, and `MEVEDEL_PLUGIN_ROOT` point at
  the plugin root.
- `PLUGIN_DATA`, `CLAUDE_PLUGIN_DATA`, and `MEVEDEL_PLUGIN_DATA` point at
  `<workspace>/.mevedel/plugin-data/<plugin-name>` and are created before
  the command starts.

For a project plugin on a remote target, all six values use target-native
paths.  A plugin root naming another execution target is refused before the
command process starts.

A client-local plugin in a remote session receives the three root variables
but not the three data variables: its workspace data is authoritative on the
target, and a local command cannot consume a TRAMP file name.  Target identity
and target-native workspace facts remain available in the JSON event input.

Superpowers is treated specially when its hooks are enabled: mevedel
installs a native `SessionStart` Elisp hook that loads the bundled
`using-superpowers` skill plus a mevedel tool mapping, and skips the
plugin's manifest hooks. Non-Superpowers plugin hooks keep their manifest
behavior.

Codex plugin `apps` and `mcpServers` manifest fields are not loaded by
the hook subsystem. They are unsupported plugin components.

`elisp` calls a function with one event plist argument.  Functions may
return nil or a decision plist.  Elisp hooks are trusted in-process code
and should usually be configured by the user, packages, or skills rather
than accepted blindly from untrusted project files.

Native abnormal `*-functions` hooks and declarative `elisp` handlers enter
one ordered handler engine. Native functions are represented as Elisp handler
records ahead of the declarative layers while retaining Emacs buffer-local,
global-inheritance, and hook-depth ordering. The shared engine owns decision
normalization, one log record per handler, serial payload mutation, decision
merging, error policy, and event-specific terminal short-circuiting. Normal
zero-argument `mevedel-session-start-hook` and `mevedel-session-end-hook`
remain notification hooks outside this decision engine.

Only `command` and `elisp` handlers are supported.

## Input and output

Every event plist includes these keys; values can be nil when no session or
turn is available:

- `:hook-event-name`
- `:session-id`
- `:transcript-path`
- `:cwd`
- `:workspace-root`
- `:model`
- `:turn-id`
- `:origin` (`/root` or a canonical retained agent path)

Tool events add:

- `:tool-name`
- `:tool-use-id`
- `:tool-input`
- `:raw-result` for post events, before persistence/truncation and
  render-data shaping
- `:result` / `:tool-response` for post events, containing the current
  result with UI-only render data removed and media represented for hooks.
  Provider output persistence/truncation happens afterward.  Both names are provided; `:tool-response` is the documented
  hook payload field, while `:result` is kept as an Elisp convenience.
- `:error` for failure events

Prompt events add:

- `:prompt`: the user prompt about to be sent
- `:display-text`: optional view-facing text used when the actual prompt is
  generated from another source, such as an inline skill invocation
- `:skill-name` and `:arguments` for `UserPromptExpansion` when the prompt
  came from a `$skill` invocation

Compaction events add:

- `:trigger`: `"manual"` or `"auto"`
- `:tokens-before`
- `:aggressive`
- `:instructions` for `PreCompact`
- `:summary` and `:tokens-after` for `PostCompact`

Sub-agent events add:

- `:agent-path`
- `:role`
- `:description`
- `:transcript-relative-path`
- `:prompt` for `SubagentStart`
- `:status` and `:terminal-reason` for `SubagentStop`

Top-level terminal events add:

- `:status`, currently `completed`, `error`, or `aborted`
- `:terminal-reason` for `StopFailure`

Command handlers receive the same data encoded as JSON with snake_case
keys.  Their `cwd`, `workspace_root`, and `transcript_path` values are
target-native paths.  Remote file names nested in `tool_input` are target-native
too, while ordinary strings such as Bash command text are unchanged.
`execution_target` contains the session's structured target identity.  This
lets a local user or plugin hook reason about a remote event without receiving
a client-specific TRAMP prefix or pretending to have a remote process cwd.
Elisp handlers receive the plist directly, with Emacs-qualified paths and
`:hook-handler` holding the normalized handler metadata for declarative
handlers.

Decision plist fields:

- `:continue nil`: stop processing where supported.
- `:stop-reason`: user-facing reason.
- `:system-message`: user-visible warning/status.
- `:additional-context`: developer/model context to inject into the next
  request or current tool feedback, depending on event.
  For `PreToolUse`, delivery is in the eventual tool result, after the current
  attempt. Use deny/ask/input rewriting when the current action must change;
  a textual instruction cannot retroactively constrain execution.
- `:permission-decision`: `allow`, `deny`, or `ask` for pre-tool and
  permission events.
- `:permission-reason`: model-facing reason for deny/ask feedback.
- `:updated-input`: replacement prompt text for `UserPromptSubmit` and
  `UserPromptExpansion`, or replacement tool args for `PreToolUse`.
  Prompt rewrites create a hook audit surface attached to the submitted
  user turn.  The audit surface records the hook event, optional
  system-message/reason detail, and original versus submitted text;
  it does not need inline diff review UI.  Tool argument rewrites create
  a hook audit surface attached to the affected tool attempt, recording
  original and updated args.
- `:updated-result`: replacement result for post-tool events.
- `:suppress-output`: unsupported; returning this field produces a hook error.

Command exit code handling:

- exit 0 with empty stdout: success, no decision.
- exit 0 with JSON stdout: parse as decision.
- exit 2: deny/block/continue according to event, using stderr as reason.
- other non-zero: hook failure.  Log it and fail open unless the event is
  explicitly configured as fail-closed.

Handlers may set `:fail-closed t`.  For those handlers, timeout,
unparseable required output, or non-zero failure blocks the triggering
operation with a hook-failure reason.  The default remains fail-open so a
broken formatter or notification script does not strand normal work.

## Pipeline integration

The tool pipeline shape is:

```
validate
-> PreToolUse hooks
-> normalize paths and prepare addressed resources
-> permission / PermissionRequest hooks
-> capture mutation coverage and optional snapshot
-> handler and render transform
-> PostToolUse / PostToolUseFailure hooks
-> provider projection: hook/repair context, oversized-output persistence,
   Goal warnings, and render/media attachment
```

Running `PreToolUse` after validation means hooks see normalized args and
do not need to duplicate schema checks.  Running it before permission lets
project policy deny early and lets `PermissionRequest` hooks participate
in every permission-card path.  Generic, Bash, Eval, and sandbox-authority
requests all fire it once before shared queue admission.  Hook allow or deny
settles the pending request without creating a card; displaying or
re-evaluating an admitted card does not rerun the hook.

`PostToolUse` runs after the handler and render transform, before provider
projection, and receives `:raw-result`, `:tool-response`, and `:result`.
The latter two contain the current feedback; subsequent provider projection
can add context or persist oversized output. `:raw-result` is available
for audit, formatting, redaction, or repair hooks that need the handler's
original output.  Post-tool hooks cannot block already-completed tool side
effects; they may replace feedback with `:updated-result` or add context.
Only a handler execution reaches a post-use hook: canonical success emits
`PostToolUse`, while explicit or signaled handler failure emits
`PostToolUseFailure`.  Validation, permission, and aborted-interaction
failures emit neither.
`PostToolUse` and `PostToolUseFailure :additional-context` attach their
hook audit surface to the affected tool transcript result because that is
the feedback they modify.  The hidden audit block is stripped at
`gptel--parse-tool-results` before any model-bound tool result is built.

Hook steps must read session/workspace/default-directory from the pipeline
context, matching the existing rule for all post-handler steps.

## Lifecycle integration

`UserPromptSubmit` runs in the data buffer context after any deterministic
skill plan has fully prepared its bodies, placeholders, and hidden instruction
context, but before the view renders or forwards the prompt to `gptel-send`.
Queueing runs no prompt hook; a queued entry fires this event once when it
becomes its own turn. A blocking decision stops the send without
inserting a user turn. Its additional context remains pending on the root
session and is consumed once by the next accepted root input. For ordinary
prompts, `:updated-input` replaces the
prompt text.  For a deterministic skill plan, the replacement is accepted
only when it retains the complete prepared prompt unchanged as a substring;
otherwise the rewrite and its audit record are ignored.  This lets a hook add
prefix or suffix policy without deleting a prepared body, reminder, or
placeholder.  Only an explicit blocking decision cancels the whole plan.
`:additional-context` is appended to the model-visible prompt inside a
`<hook-context>` block while staying out of the view-facing user message
body.  The view shows a generic collapsed `◇ hook context added`
disclosure that can be expanded to see the contributing event names and
injected text.  Multiple hook context contributions consumed by the same
prompt share one combined disclosure, preserving contribution order in the
expanded details. Pending context is cleared at the transcript commit boundary,
before request startup, so a later dispatch error cannot duplicate context that
is already stored.  An error before transcript insertion leaves it pending for
retry.  If a prepared WaitAgent steering attempt loses its waiter race, mevedel
transfers the approved context into the queued prompt. Earlier FIFO entries
cannot consume it, draining does not rerun `UserPromptSubmit`, and editing or
clearing the queue restores the context for the next user submission.
Model-visible and persisted `<hook-context>` blocks contain ordered
`<hook-event name="...">` entries. When known, their `source`, `file`, and
`plugin` attributes identify the contributing handler's configuration source.
Attribution comes from the host runner, not handler-returned claims. A decision
whose context was changed after the runner returned keeps only its event label,
so replacement text does not inherit stale attribution. Attribute values and
bodies are escaped; authored obligations and repeated contributions remain
intact rather than being summarized or silently deduplicated. These source
labels support conflict assessment; they introduce no new precedence rule.

Resume, rewind, and full rerender retain the recorded context:

```xml
<hook-context>
<hook-event name="SessionStart" source="plugin" plugin="project-workflow">
Project workflow context.
</hook-event>
<hook-event name="UserPromptSubmit" source="project-file" file="/project/.agents/hooks.el">
Project-specific prompt policy.
</hook-event>
</hook-context>
```

Retained-agent initial tasks and idle-agent follow-ups also fire this event
once before their model request. Mailbox delivery, compaction, guardians,
response items, and automatic continuations do not. If an agent task is
blocked, its additional context remains pending on that retained identity and
is consumed once by its next accepted task; it cannot enter the root or a
sibling conversation. Internal root flows that construct their own requests,
such as directive processing and plan execution, do not currently fire this
event.

`SessionStart :additional-context` is pending context for the next request,
not an immediate transcript event. Its hook audit surface is attached to the
first user turn that consumes it, so the visible record appears where the
context affects model input. A fresh buffer uses source `startup`; restoring a
saved session uses `resume`; successful `/clear` uses `clear`; and successful
root compaction uses `compact`. Restoring an already-live buffer does not
reinitialize it. Clear and compaction begin context epochs inside the same
live session epoch and therefore do not emit `SessionEnd`.

Context-changing audit surfaces represent an event once and list its
contributing handlers in execution order.  Each handler retains its own
source, identity, reason, and context bodies where that audit surface exposes
them.  Parent sub-agent rows omit the injected bodies.

`UserPromptExpansion` runs after each unique user-invoked skill body is
prepared. Commands run first from left to right, followed by deduplicated
instructions in first-occurrence order. `:updated-input` replaces that skill's
prepared contribution; `:additional-context` is appended inside a
`<hook-context>` block. A blocking decision stops the complete plan before any
request or child dispatch. After all expansions settle, `UserPromptSubmit`
sees the complete inert prompt without receiving earlier context in its event
payload. Model-side Skill calls do not fire this event. Expansion context is
merged afterward with start and prompt-submit context, in lifecycle order, and
persisted in the accepted turn's durable disclosure.

`PreCompact` runs after the compaction range and prompt have been prepared
but before the compaction request is sent.  A blocking decision stops the
compaction.  `:additional-context` is supplied as labelled untrusted evidence
beneath the fixed context-summary system contract, so hooks can provide local
retention hints without changing summary purpose, authority, or structure.
Its hook audit surface belongs on the compaction event/summary, not a
user turn, because the summarizer request is the model call it changes.
For automatic compaction, a block is treated as compaction failure and the
pending user request is not sent. Each provider retry reruns `PreCompact` with
a fresh decision rather than reusing the previous attempt's hook result.

`PostCompact` runs after a successful summary has been applied. It receives
the summary and before/after token estimates. Decisions are currently logged
but not injected anywhere. A successful root compaction then runs
`SessionStart(compact)` before final view completion. Manual compaction leaves
that context pending for the next accepted input; automatic compaction adds it
to the already-approved pending request without rerunning `UserPromptSubmit`.
Retained-agent compaction emits `PreCompact` and `PostCompact` only.

`SubagentStart` runs once when a retained identity is created, before
`UserPromptSubmit` and before the identity is published. Follow-ups and
retained-agent compaction do not rerun it. A blocking decision stops the Agent
tool without leaving an addressable identity or partial conversation.
`:additional-context` is appended to the sub-agent prompt inside a
`<hook-context>` block. This hook is awaited before the atomic spawn commits;
after commit the Agent tool returns the retained canonical path immediately.
Its hook audit surface is split across parent and child:
the parent Agent tool row records that the spawned agent received hook
context, while the child transcript attaches the full hook context to the
child's initial prompt.

`SubagentStop` runs once after every invocation reaches `completed`, `error`,
or `aborted` and after transcript status/sidecar updates have been written.
The retained identity remains addressable for later follow-ups.
Decisions are currently logged but do not change terminal status or parent
feedback.

`Stop` runs after a successful top-level assistant turn, before the
request-scoped hook layers are cleared. `Stop` and `StopFailure` retain the
ending request's hook rules and event context, but their handlers are not
registered as request cancellers: turn teardown must not kill them before
they settle. Each command handler remains bounded by its own timeout.
This includes awaited fork user skill
completions, which finalize the parent turn without a
normal gptel DONE transition.  `StopFailure` runs for top-level error and
abort terminals and includes `:terminal-reason` when available.  Both
events are observational: blocking decisions are logged but do not change
terminal state.

## Trust and permissions

Shell hooks are arbitrary code in their resource environment.  Project-file
commands run on the target and must not run unless the workspace hook config
is trusted. By default:

- user defcustom and user `.agents/hooks.*` / `.mevedel/hooks.*` files are trusted.
- project `.agents/hooks.*`, `.mevedel/hooks.*`, and `SKILL.md` hook declarations
  are ignored until the user trusts them for that workspace.
- trust state lives under user state, keyed by workspace id and project
  hook file hashes.
- changed project hook files or skill manifests require re-trust.
- resolution reads each project config once, checks that snapshot's hash, and
  parses the same bytes; replacing the pathname after the check cannot
  substitute executable rules.
- trusting a project refreshes that workspace's trust entries to the current
  hook files and skill manifests only, so removed sources are no longer
  trusted.

Permission decisions are audit-surfaced by outcome.  `deny` and forced
`ask` decisions are visible on the affected tool attempt because they
change control flow.  `allow` decisions remain in hook logs unless they
suppress a permission prompt that would otherwise have been shown.

Project Lisp configuration is read as data with reader evaluation disabled;
loading it does not evaluate arbitrary forms. A trusted `:function` declaration
can invoke an existing Elisp function, which then runs with full in-process
authority. Trust the handler implementation as well as its configuration.

Hooks may tighten policy.  They should not silently weaken explicit
permission denies.  A `PreToolUse` or `PermissionRequest` allow can skip a
prompt only when the normal permission resolver would not return an
explicit deny.  An `ask` still emits `PermissionRequest` once before a
`PreToolUse` allow suppresses queue admission.  Every final policy, user,
`PermissionRequest`, or
`PreToolUse` denial emits one `PermissionDenied` payload carrying the
original `:permission-provenance`.

## Emacs API

Notification hooks:

- `mevedel-session-start-hook`
- `mevedel-session-end-hook`

Control/argument hooks:

- `mevedel-user-prompt-submit-functions`
- `mevedel-user-prompt-expansion-functions`
- `mevedel-pre-tool-use-functions`
- `mevedel-permission-request-functions`
- `mevedel-permission-denied-functions`
- `mevedel-post-tool-use-functions`
- `mevedel-pre-compact-functions`
- `mevedel-post-compact-functions`
- `mevedel-subagent-start-functions`
- `mevedel-subagent-stop-functions`
- `mevedel-stop-functions`
- `mevedel-stop-failure-functions`

These support normal `add-hook` usage, including buffer-local hooks
with LOCAL non-nil. Programmatic hooks use the same decision plist
as declarative `elisp` handlers.

## Debugging and UI

The hook runner keeps a per-session in-memory hook log with event,
handler, status, elapsed time for command hooks, stdout/stderr previews,
parsed decision, and failure details.  `SessionStart` entries also record
`:event-source` as `"startup"`, `"resume"`, `"clear"`, or `"compact"`,
separately from the handler's configuration source.

Raw command-hook stdout/stderr never appears in the view by default.
Only structured decision fields are eligible for user-facing hook audit
surfaces.  Hook authors who want user-visible text must return fields such
as `:system-message`, `:stop-reason`, `:permission-reason`,
`:additional-context`, or `:updated-input`.

`:system-message` by itself is a transient user notification: it is shown
through `message` and recorded in the hook log, but it does not create a
transcript/view audit item.  When the same decision also changes
model-visible context, submitted content, control flow, or permissions,
the hook audit surface includes the system message as supporting detail.

When the session has been materialized on disk, the same entries are also
appended to `<session>/hook-log.el` as one sanitized plist per line.  Local
sessions append when each entry is recorded; remote process callbacks enqueue
and the next successful session settlement publishes the append atomically.
Bounded entries created before the session has a save path are backfilled when
it first materializes.  Non-readable runtime values such as closures are
converted to printable strings before writing.  The in-memory log remains
capped by `mevedel-hooks-log-limit`; the persistent file is append-only for the
session.  A failed append warns, remains queued, and retries after the next
successful session save.

Raw hook stdout/stderr should stay out of the model transcript by default.
Only explicit structured fields such as `:additional-context`,
`:permission-reason`, `:updated-result`, or `:tool-response` may enter
model-visible content.  Random script output belongs in the hook log, with
important failures surfaced to the user in the view or messages.

Slow hook runs are surfaced after `mevedel-hooks-slow-threshold` seconds.
If the view already has an active spinner, its status changes to show the
running hook event; otherwise the user sees a `message`.  The temporary
status has a per-run ownership token.  It replaces only the progress state
captured when the hook was dispatched, restores that state on completion,
and cannot overwrite or restore across a newer request or agent status.
Events with no matching handlers do not create progress activity.  Blocking
decisions are always surfaced with the event name and hook-provided reason.
For tool calls blocked by `PreToolUse` or `PermissionRequest`, the compact
tool line includes a second line such as `blocked by PreToolUse: reason`.

Useful commands:

- `mevedel-hooks-list`: show effective hooks for the current session.
- `mevedel-hooks-trust-project`: trust the current project's hook files and
  skill manifests.
- `mevedel-hooks-run-dry`: show which native and declarative hooks would
  run for an event/matcher target without executing Elisp or shell
  handlers.
- `mevedel-hooks-reload`: forget the memoized hook configuration so the
  next event re-reads every config file.  The user, plugin, and project
  layers are remembered per workspace for the process — re-resolving
  them costs several target round trips per event on a remote workspace
  — so an edit to a hook file needs this command (or trusting the
  project, or toggling a plugin, each of which invalidates) to take
  effect.

Quiet successful hooks do not clutter the normal workflow.
