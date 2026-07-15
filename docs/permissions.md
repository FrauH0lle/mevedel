# Permission system

## Decision flow

```mermaid
flowchart TD
    A[Tool call] --> B[Extract specifiers]
    B --> C{Any deny rule?}
    C -- Yes --> Z[Deny]
    C -- No --> D{Goal planning or review<br/>and mutating tool?}
    D -- Yes --> Z
    D -- No --> E{Command checker?}
    E -- Yes --> F{Checker or rules<br/>authorize command?}
    F -- Deny or ask --> Q[Command decision]
    F -- Allow --> G{Path supplied?}
    E -- No --> N{Protected path without<br/>an exact grant?}
    N -- Yes --> X[Ask for resource authority]
    N -- No --> H{Matching native rule?}
    H -- Deny or ask --> Q
    H -- Allow --> Y
    H -- None --> J{Allowed root, exact path,<br/>or exact resource grant?}
    J -- No --> X
    J -- Yes --> K{Mode authorizes tool?}
    K -- Yes --> Y
    K -- No --> Q
    G -- No --> Y[Allow]
    G -- Yes --> I{Protected path without<br/>an exact grant?}
    I -- Yes --> X[Ask for resource authority]
    I -- No --> L{Allowed root, exact path,<br/>or exact resource grant?}
    L -- Yes --> Y
    L -- No --> X
```

Single decision function `mevedel-check-permission`. Decision chain:

1. Extract specifier values via `get-path` / `get-pattern` / `get-domain` /
   `get-name` slots
2. Deny rules (across all buckets — see bucket precedence below)
3. Active Goal planning or review with a non-read-only tool → deny
4. Tool's own `check-permission` slot decides command authority
5. Allow/ask rules (innermost-bucket-first — see bucket precedence below)
6. Permission-mode hard deny, if any
7. For a path not directly covered by a native path rule, resolve an allowed
   root, exact allowed path, or exact resource grant
8. A protected or outside-root path without that authority → ask
9. Permission-mode fallback when no earlier policy decides; satisfied resource
   authority does not itself authorize a mutating operation

For a tool with a command checker, command authority and filesystem resource
authority are layered: both must allow. A command rule cannot authorize its
path, and a resource grant cannot authorize its command. Native `:path` rules
remain direct tool authorization, but cannot bypass a protected path's exact
resource grant.

Hook integration sits around this chain:

- `PreToolUse` runs before the chain. A hook `deny` is final. A hook
  `ask` can tighten an allow into a prompt. A hook `allow` can only skip
  a prompt when the normal resolver would have returned `ask`; explicit
  denies still win.
- `PermissionRequest` runs after the chain returns `ask` and before the
  generic queued prompt is shown. It can allow, deny, or leave the prompt
  in place. Tools with specialized permission queues, currently Bash and
  Eval, make their prompt decision inside the tool permission slot instead
  of returning a generic `ask`.
- `PermissionDenied` runs after any final denial. It can adjust the
  reason/context shown to the model, but it cannot turn the denial into an
  allow.

Permission invocation context is normalized in the permission module before
callers enter the decision chain. That context centralizes specifier
extraction, rule buckets, mode, allowed roots, exact resource grants,
missing-session fallback warnings, and the prompt rule shape used for
outside-root approvals.

The synchronous and asynchronous decision entry points then share one pure
preflight. It normalizes decision facts, resolves absolute deny rules, and
records protected-path and resource-boundary facts exactly once. Both paths
use the same synchronous tool slot adapter and decision tail; only a tool that
supplies an asynchronous permission callback introduces an asynchronous
branch.

## Bucket precedence

Steps 2 and 5 consume rules from multiple buckets, in this order:
invocation `skill-permission-rules`, request `skill-permission-rules`,
session rules, persistent rules, defcustom `mevedel-permission-rules`.

- Step 2 (deny) is absolute — any bucket's `deny` wins.
- Step 5 (allow/ask) is innermost-first — the first bucket yielding any
  decision wins.
- Goal planning and review deny non-read-only tools before allow rules are
  considered. This restriction is independent of the session permission mode,
  so skill, session, persistent, and default allow rules cannot bypass it.
- Automatic Goal guardians receive no tools at all. Guardian approval changes
  lifecycle state only; it never raises the session permission mode. Fully
  unattended implementation therefore still requires an explicit `full-auto`
  selection.

## Rule format

Rules live on `mevedel-permission-rules` with form
`(TOOL-NAME &key SPECIFIER VALUE :action ACTION)`. One specifier per rule:

| Key        | Matches                | Used by                           |
|------------|------------------------|-----------------------------------|
| `:path`    | path (glob, `~` exp.)  | Read, Edit, Write, Glob, Grep, ...|
| `:pattern` | command string (glob)  | Bash                              |
| `:domain`  | host name (glob)       | WebFetch, YouTube                 |
| `:name`    | free-form name (glob)  | Agent (subagent_type)             |

Precedence: specifier rules outrank generic; within a group
`deny > ask > allow`. Protected paths prompt unless an exact resource grant
with sufficient access already exists.

`mevedel-protected-paths` is an alist from glob to `read-only` or
`inaccessible`. The default `.git` glob is read-only; the default SSH and GnuPG
credential globs are inaccessible. String-only entries are invalid by design.

The three canonical modes are `ask`, `auto`, and `full-auto`:

- `ask` allows recognized inspection and prompts for edits and uncertain
  Bash or Eval execution.
- `auto` additionally applies native edits inside allowed roots, but grants no
  blanket Bash or Eval authority.
- `full-auto` bypasses heuristic Bash and Eval prompts, including live Eval,
  while explicit denies and protected resources without exact authority still
  win.

Configuration and persisted sessions accept only these canonical values. The
interactive `/mode` command additionally accepts `edit` as a user-facing alias
for `auto`; the alias is normalized before it reaches session state.

The prompt offers allow/deny choices for the invocation, session, or persistent
workspace scope. `.mevedel/permissions.el` stores a plist containing both
`:rules` and `:resource-grants`.

Default allowed roots are the workspace root, the system temporary directory,
configured memory roots, and manually configured additional roots. A native
filesystem operation outside those roots prompts for exact `read` or `write`
authority. A session grant is stored on the session; an always grant is stored
only in the workspace permission file. Write authority covers reading the
same exact path, but read authority does not cover writes. These grants do not
cover siblings or descendants, add workspace roots, or authorize Bash/Eval
code. Revoking the grant restores the underlying workspace/protected-path
restriction. Invocation-only authority is consumed by the approved call and is
not stored.

Files dropped into the view buffer can add exact, session-scoped `Read`
grants when the next sent prompt still mentions the same path. These
grants are in-memory only, do not grant the containing directory, do not
apply to write tools, and are still lower precedence than explicit deny or
ask rules and protected-path resource checks.

Local slash commands may own deterministic workflows outside the model tool
pipeline. `/worktree status` and `/worktree create` run argv-safe local
Git commands directly because the user explicitly typed the command in the
mevedel UI. That does not grant the model any Bash permission. When the
model uses the `git-worktree` skill and falls back to creating a worktree
itself, creation happens through the normal Bash tool and this permission
chain.

## Prompt queues

Permission prompts are queued on the session, not displayed as
independent blocking overlays. `mevedel-permission-queue.el` owns a
heterogeneous FIFO with four entry kinds:

- `generic` for pipeline permission asks
- `bash` for Bash command confirmation
- `eval` for Eval expression confirmation
- `sandbox` for additive network or exact filesystem authority

Only the head is visible in the view interaction zone. The permission UI
registers that head with `mevedel-view-interaction.el`, which owns ordering,
callback overlays, and redraw. Rule-creating outcomes (`allow-session`,
`deny-session`, `always-allow`) can coalesce
queued siblings by re-running the decision chain. The queue is transient
runtime state and is not written to the session sidecar; unfinished
prompts are aborted on request/session teardown.

`mevedel-permission-prompt.el` is the focused UI owner for all four entry
kinds. It owns generic permission controls, agent attribution, Bash guardian
and dangerous-command presentation, and Eval presentation. The queue retains
ordering and outcome semantics; the shared interaction primitive retains
overlay settlement and request cancellation.

Permission diagnostics are persisted to `permission-log.el` in the session
directory when `mevedel-permission-log-enabled` is non-nil. The log is
diagnostic only: resume never replays it into live permission state.
Entries recorded before first materialization are buffered transiently and
flushed when the session directory is created.

Each tool invocation that reaches permission checking records a sanitized
`permission-decision` event with fields such as tool name, origin, mode,
outcome, specifier, protected-path flag, resolver path, and rule bucket. It
does not include raw Write/Edit content, arbitrary tool args, or extra raw
Bash/Eval payloads. Prompt lifecycle events remain separate: queue
enqueue/resolve/abort/coalesce events describe prompt handling without raw
Bash commands or Eval expressions.

## Bash specifics

Bash analysis returns a normalized command class, structured argument vectors,
parser source, literal resources, and human-readable reasons.  It uses a
normally configured Bash Tree-sitter grammar when available and a conservative
scanner otherwise.  Redirections, substitutions, expansions, assignments,
subshells, here-documents, control flow, parse errors, and unsupported operators
are complex.  A dangerous component takes precedence in a compound request.
Read-only classification uses argument-aware built-in policies.  Git status,
log, diff, show, and query-only branch forms are recognized only without
output, configuration, pager, helper-execution, or mutation options.  Find,
ripgrep, base64, sed, and awk likewise reject deletion, helper execution,
output-file options, and unrecognized programs.  Safe forms need no broad
default allow patterns; variants outside these narrow policies remain unknown.
Bash does not use the
pipeline's generic permission prompt or `PermissionRequest` hook path;
when it needs a decision it enqueues a Bash-specific permission entry.
Under `full-auto`, unknown, dangerous, and complex Bash commands are
allowed without a prompt after explicit deny rules and literal protected
path tokens have been checked. Outside `full-auto`, unknown commands
default to ask. Direct user-authored session, persistent, and defcustom
patterns may authorize dangerous or complex forms. Invocation- and
request-scoped delegated patterns may not. Explicit denies always win.

### Child confinement

Bash and batch Eval share the guarded child launcher and, independently of the
permission mode, consult `mevedel-sandbox-mode`. On Linux, `auto` resolves
`bwrap` with `executable-find` and caches one real probe of the core mount,
user, process, and network namespaces. A successful profile mounts `/`
read-only, rebinds the workspace, temporary directory, memory roots, manually
configured roots, and session working directory writable, installs a fresh
`/proc`, and changes to the canonical working directory. The default profile
also isolates the network. A justified additive network request prompts in
`ask` and `auto`, proceeds automatically in `full-auto` after command
authorization, and changes only network isolation for that invocation. The
namespace and mount boundary is inherited by descendants.

A justified additive filesystem request names exact absolute paths and marks
each as read or write. Ungranted paths prompt in every permission mode;
invocation, session, and persistent approvals use the same exact resource-grant
store as native filesystem tools. Approved paths are rebound at only the
requested access level after protected masks are installed. Inaccessible
parents expose traversal only far enough to reach the named mount, so their
contents and sibling resources remain hidden. Command or Eval authorization is
resolved independently and is never supplied by the resource grant. Explicit
path denies remain final. Network and filesystem additions may be combined
without changing any unrequested confinement boundary.

`auto` executes directly when the initial probe is unavailable. For the narrow
race where a later Bubblewrap launch fails, a marker emitted immediately before
`exec` proves whether the requested process started: only a missing marker
permits one direct fallback. A command failure, signal, or timeout after the
marker is returned exactly once and is never replayed. `required` returns an
execution error instead of falling back, while `off` selects direct execution
deliberately. Direct execution always reports `filesystem: unrestricted` and
`network: unrestricted`; confined and direct child results both append their
active sandbox facts for the model and audit trail.
Trusted skill substitutions keep those facts out of the substituted literal;
an unrestricted substitution instead emits a user-visible warning while the
active facts remain recorded.

Protected restrictions are layered after writable roots. Existing glob matches
and canonical targets become concrete mounts; `.git` pointer files also protect
their Git directory target. Read-only paths remain visible but immutable, while
inaccessible directories are replaced by empty read-only mounts. Determinable
missing directory roots receive identity-checked temporary mount targets that
are removed after settlement. A protected path crossing a symlink that the
child could rewrite fails closed instead of relying on a racy canonical-path
snapshot.

### Bash guardian guidance

`mevedel-permission-guardian` can add model-reviewed risk guidance to
Bash prompts. Outside `full-auto`, it is advisory only: the normal
permission chain still decides `allow` / `ask` / `deny`, explicit deny
rules still win, Goal phase restrictions and protected-path policy are
unchanged, and
the user remains authoritative. In `full-auto`, the guardian is
deny-only for commands that the normal classifier would have treated as
suspicious; deny recommendations veto, while timeouts, failures, invalid
output, and non-deny recommendations allow by default.

Guardian guidance runs only after Bash resolves to `ask`. The permission
prompt is shown immediately with:

```text
Guardian guidance
Status: Analyzing command risk...
```

When guidance arrives, the same queued prompt is redrawn with risk,
recommendation, and reason. If the reviewer times out, fails, or returns
unparseable output, the section stays visible as:

```text
Guardian guidance
Unavailable
```

Set `mevedel-permission-guardian` to `t` to use the `guardian` workload
policy from the current session's `mevedel-model-workloads`, or to a custom
`(lambda (command context callback) ...)` classifier for tests or local
policy. `mevedel-permission-guardian-timeout` controls the wait for
reviewer output; the default is 20 seconds. The model prompt lives in
`prompts/permissions/bash-guardian.md`.

## Eval

Eval asks through the same session permission queue's Eval-specific
entry type unless the effective permission mode is `full-auto`. Like
Bash, it does not use the generic `PermissionRequest` hook path. The
expression shown in the prompt is subject to
`mevedel-eval-expression-display-limit`.  The prompt also shows the
requested execution mode and, for live Eval, whether UI preservation is
enabled.

Eval supports two execution modes.  `live` is the default and evaluates
inside the current Emacs process so the expression can inspect live
buffers, variables, windows, timers, advice, and package state.  Live
Eval restores the selected frame's window configuration by default;
callers can pass `preserve_ui: false` only when intentional UI
manipulation is desired.  `batch` starts a child `emacs --batch -Q`
process with the current `load-path` and the session working directory.
Batch Eval protects the interactive Emacs session from UI/global-state
mutation and uses the same optional child confinement as Bash. When
confinement is unavailable or disabled, it still runs as the same OS user and
the result explicitly reports unrestricted filesystem and network access.

Skill body elisp injections (`!el` inline and ` ```!el ` fenced blocks)
are the exception: they pass a trusted-literal flag because the
expression is author-written SKILL.md content, not model-generated Eval
input. A trusted elisp injection may bypass the prompt only when an
active unqualified `Eval` allow rule covers it, typically from the
skill's `allowed-tools: [Eval]`. Eval deny rules still win absolutely,
and Goal planning/review suppress mutating Eval calls. Markers introduced
by argument substitution are not trusted literals and are left as text.
Literal markers may still contain substituted text in their expression
body; only the marker syntax and delimiters carry the trusted-literal
provenance.

## Sub-agent permission propagation

Sub-agent buffers carry `mevedel--session` set buffer-locally to the
**parent's session struct, by reference** (allocated in
`mevedel-agent-exec--allocate-agent-buffer`). The pipeline reads
`mevedel--session` from the current buffer at tool-dispatch entry, so a
tool dispatched inside a sub-agent observes the parent's
`permission-rules` and `permission-mode` slots, and any
"allow-session" / "deny-session" outcome accepted inside the sub-agent's
prompt is written via `setf` on the same struct -- so the new rule
applies immediately to the main agent and to every other live sub-agent.
This is a deliberate sharing contract; agents that should not be able to
mutate the shared state are constrained today by their tool list (e.g.
the verifier ships read-only tools, so its calls never reach the prompt
step).

All queued permission prompts render in the parent session's interactive
view buffer, not inside the sub-agent transcript buffer or a read-only
transcript inspection view. Queue entries carry an origin (`"main"` or
the canonical agent id), and request teardown only aborts entries owned
by the ending request. This keeps a background agent's visible prompt
open across parent-view rerenders and parent request cleanup until the
user explicitly resolves it or aborts the session/agent.

If the permission step ever runs without a session in context,
`mevedel-pipeline--step-permission` emits a `display-warning`
("Permission step for ... ran with no session in context"); that
fallback would silently consult only the defcustom-scoped global
defaults, which is the actual hazard. The warning surfaces it.

## Bash permission example

```elisp
(setq mevedel-permission-rules
      '(("Bash" :action ask)                       ; default ask
        ("Bash" :pattern "echo"     :action allow)
        ("Bash" :pattern "echo *"   :action allow)
        ("Bash" :pattern "ls"       :action allow)
        ("Bash" :pattern "ls *"     :action allow)
        ("Bash" :pattern "git log*" :action allow)
        ("Bash" :pattern "rm *"     :action deny)))

(setq mevedel-bash-dangerous-commands
      '("rm" "sudo" "dd" "chmod" "curl" "wget" "ssh"))
```

Use space-boundary patterns (`"ls"` + `"ls *"`) rather than `"ls*"` to
avoid matching `lsof`. Supported plain syntax is commands joined by
`&&`, `||`, `;`, or `|`. Dynamic shell forms are complex and require either
an interactive decision, `full-auto`, or a deliberately authored direct-user
pattern.
