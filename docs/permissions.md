# Permission system

The subsystem has four owners. `mevedel-permission-mode.el` owns mode
normalization and session-scoped transitions; `mevedel-permission-rules.el`
owns rule matching, precedence buckets, protected-path policy, and resource
grants; `mevedel-permission-persistence.el` owns authority-store validation and
target-aware I/O. `mevedel-permissions.el` is the decision facade that combines
those facts with tool policy.

## Decision flow

```mermaid
flowchart TD
    A[Tool call] --> B[Extract specifiers]
    B --> C{Any deny rule?}
    C -- Yes --> Z[Deny]
    C -- No --> D{Planning/review<br/>and forbidden mutation?}
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
3. Active standalone Plan with a native edit tool other than an all-local
   `ApplyPatch` or with `Eval`, active directive Planning with any native edit
   tool or `Eval`, an `ApplyPatch` containing an ordinary, non-local, or bare
   endpoint, or active Goal planning/review with a native edit tool -> deny
   regardless of allow rules or permission mode
4. Tool's own `check-permission` slot decides command authority
5. Allow/ask rules (innermost-bucket-first — see bucket precedence below)
6. Permission-mode hard deny, if any
7. For a path not directly covered by a native path rule, resolve an allowed
   root, exact allowed path, or exact resource grant
8. A protected or outside-root path without that authority → ask
9. Permission-mode fallback when no earlier policy decides; satisfied resource
   authority does not itself authorize a mutating operation

ApplyPatch is the deliberate exception to step 9. Its reviewed-edit capability
is allowed because `ask` mode already supplies mandatory per-file/hunk review
before any write. Allowed-root patches therefore proceed directly to that
review. Protected or outside-root paths still require resource authority before
the review appears, and explicit allow/ask/deny rules retain their precedence.

For a tool with a command checker, command authority and filesystem resource
authority are layered: both must allow. A command rule cannot authorize its
path, and a resource grant cannot authorize its command. Native `:path` rules
remain direct tool authorization, but cannot bypass a protected path's exact
resource grant.

Hook integration sits around this chain:

- `PreToolUse` runs before the chain. A hook `deny` is final. A hook
  `ask` can tighten an allow into a prompt. A hook `allow` can only skip
  a prompt when the normal resolver would have returned `ask`; explicit
  denies still win. The resulting `ask` still crosses `PermissionRequest`
  once before that earlier allow suppresses queue admission.
- `PermissionRequest` runs whenever generic, Bash, Eval, or sandbox authority
  resolution reaches `ask`, before the corresponding entry enters the shared
  queue. It can allow, deny, or leave the prompt in place. Queue display and
  rule-driven re-evaluation do not rerun it.
- `PermissionDenied` runs after any final denial. It can adjust the
  reason/context shown to the model, but it cannot turn the denial into an
  allow. Its payload identifies the original policy, user, `PreToolUse`, or
  `PermissionRequest` provenance.

Permission invocation context is normalized in the decision facade before
callers enter the decision chain. That context centralizes specifier
extraction, rule buckets, mode, allowed roots, exact resource grants,
missing-session fallback warnings, and the prompt rule shape used for
outside-root approvals.

## Resource-address authority

A resource address identifies a target; it is not a permission grant. After
repair and final validation, resource preparation resolves an opaque attempt
and only the logical authority facts needed here, without reading content.
Permission then applies the ordinary operation policy before the authorized
handler consumes that attempt. Malformed or unsupported addresses, traversal,
and containment failures stop before permission and post-use hooks; a valid but
missing, disconnected, stale, or unreadable resource follows ordinary handler
failure handling.

Read-only `artifact://`, `skill://`, `agent://`, `history://`, and `memory://`
resources keep their intrinsic read capability and current freshness rules.
`local://` mutation uses ordinary `ApplyPatch` permission and patch review;
recognizing an address does not broaden roots or create a grant. Skill and
memory addresses retain client-local origin, while MCP authority remains with
the current configured connection. Standalone/sticky Plan mode allows all-local
`ApplyPatch`, including requests from retained agents, but denies any proposal
containing an ordinary, non-local, or bare endpoint before local materialization
or ordinary mutation. Mixed local/ordinary and ordinary-only proposals
therefore fail tree-wide; permission modes and allow rules cannot reopen that
boundary. See
[`address-to-resource.md`](address-to-resource.md#shared-resolution-and-permission-seam).

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
- Directive discussion installs request-scoped denies for every registered
  mutation-capable tool and for mutation-capable delegation. These denies are
  a hard capability ceiling: broader session, workspace, or global allows
  cannot enable mutation during that discussion request.
- Goals use the same tool permission policy as ordinary root conversation
  turns. Goal lifecycle state never raises the session permission mode.
- Standalone Plan approval applies the selected Ask, Edits, or Full Auto mode to
  the session that performs Direct implementation. Here changes the source
  session; Worktree leaves the source mode unchanged and applies the selection
  only after the target session and accepted artifact are prepared. Summary
  generation does not raise either session's permission mode.

## Rule format

Rules live on `mevedel-permission-rules` with form
`(TOOL-NAME &key SPECIFIER VALUE :network BOOL :file-system GRANTS
:sandbox-permissions LEVEL :action ACTION)`.
One specifier per rule:

| Key        | Matches                | Used by                           |
|------------|------------------------|-----------------------------------|
| `:path`    | path (glob, `~` exp.)  | Read, ApplyPatch, Glob, Grep, ... |
| `:pattern` | command/expression glob | Bash; full-escalation Eval rules |
| `:domain`  | host name (glob)       | WebFetch, YouTube                 |
| `:name`    | free-form name (glob)  | Agent (`role`)                    |

Precedence: specifier rules outrank generic; within a group
`deny > ask > allow`. Protected paths prompt unless an exact resource grant
with sufficient access already exists.

`:sandbox-permissions` is an execution-level qualifier, not a request to raise
authority. A rule carrying `require-escalated` is considered only after Bash or
batch Eval has explicitly requested that level. Ordinary command allows cannot
authorize full escalation. Only direct user-authored session, persistent, and
defcustom rules may allow it; delegated invocation/request rules cannot. A
pattern scopes authority to the matching Bash command or Eval expression, and
omitting the pattern deliberately authorizes every expression for that tool at
that execution level. Qualified and ordinary explicit denies remain final.

`:network` and `:file-system` form an execution permission profile on a
matching Bash or batch-Eval allow rule. The profile records the additive child
authority approved with that operation; it is not a workload classifier.
Single-segment Bash approvals may use the recognized safe command pattern.
Compound commands keep generalized operation rules for their segments but
store the profile against the complete compound command, preventing one
segment from inheriting another segment's capability.
Filesystem entries are exact `(:path PATH :access read-or-write)`
requirements. `PATH` is either absolute or home-relative with `~`/`~/`; stored
home-relative paths expand in the execution target. Project and session stores
encode exact paths and absolute path rules without a client-specific TRAMP
prefix, then qualify them through the currently opened matching target. A
foreign target prefix invalidates the stored authority instead of being
reinterpreted. Entries become effective only while an equally strong direct
resource grant still exists. Multiple matching profiles are unioned, with
write dominating read for the same path. Only session, persistent, and
defcustom rules can contribute a reusable profile. Invocation- and
request-scoped delegated rules may still authorize ordinary operations but
never broaden the child sandbox.

`mevedel-protected-paths` is an alist from glob to `read-only` or
`inaccessible`. The default `.git` glob is read-only; the default SSH, GnuPG,
AWS, Azure, Google Cloud, and Kubernetes credential globs are inaccessible. On
a trailing `/**`, policy covers both the directory and its descendants. On a
remote session, leading `~` uses the target user's probed home; a
client-absolute custom pattern stays in the client path domain and therefore
does not become a remote protection rule.
String-only entries are invalid by design.

The three canonical modes are `ask`, `edits`, and `full-auto`:

- `ask` allows recognized inspection, sends ApplyPatch directly to its
  mandatory review when its paths are authorized, and prompts for other edits
  and uncertain Bash or Eval execution.
- `edits` additionally applies native edits inside allowed roots, but grants no
  blanket Bash or Eval authority.
- `full-auto` bypasses heuristic Bash and Eval prompts, including live Eval,
  while explicit denies and protected resources without exact authority still
  win.

Configuration, interactive commands, and persisted sessions accept only these
canonical values.

The prompt offers allow/deny choices for the invocation, session, or persistent
workspace scope. When one Bash or batch-Eval call needs both operation and
additive authority, one card presents the complete request. Session and
workspace approval default to remembering the complete selected profile;
command, network, and individual path toggles can narrow it before approval.
The current invocation always receives the complete approved request.
`.mevedel/permissions.el` stores a plist containing both `:rules` and
`:resource-grants`. It lives with the project on its execution target and stores
target-native paths, so reopening the same workspace through an equivalent
TRAMP alias reuses its authority. The user-global `permissions.el` remains in
the client-local `mevedel-user-dir`; it is merged at runtime and is never copied
to the target or into a portable session. Project-store replacement is atomic
and descriptor-pinned; a symlink in the state path or at `permissions.el`
fails closed before any linked target is changed.

Global and project stores are validated when a fresh, resumed, or forked
session initializes. Invalid files contribute no authority and produce one
actionable warning per file version. Persistent permission changes refuse to
overwrite an invalid file; fix it to the current
`(:rules (...) :resource-grants (...))` shape first. Missing files are valid
empty stores.

Default allowed roots are the workspace root, the system temporary directory,
configured memory roots, and manually configured additional roots. A native
filesystem operation outside those roots prompts for exact `read` or `write`
authority. A session grant is stored in the durable session sidecar and
survives save/resume of that same session; an always grant is stored only in
the workspace permission file. Neither enters an unrelated session. ApplyPatch
authority covers reading the same exact path, but read authority does not cover
writes. These grants do not cover siblings or descendants, add workspace
roots, or authorize Bash/Eval code. Revoking the grant restores the underlying
workspace/protected-path restriction. Session-side paths use the same
target-native codec as workspace authority, while another client's global
rules are recomputed locally on resume. Invocation-only authority is consumed
by the approved call and is not stored.

A conversation or worktree session fork copies the source session's permission
mode, sandbox mode, session permission rules, and resource grants at the fork
point. Subsequent authority changes are local to the source or fork unless they
modify the shared workspace permission store.

An ephemeral `/btw` side also copies that policy, but every side request carries
an immutable one-shot mutation boundary. Analyzer-proven read-only Bash and
read-only tools remain automatic when the ordinary policy allows them. Other
mutations ask even under `full-auto` or an inherited allow rule, and the prompt
offers only allow-once, deny-once, or feedback; defensive outcome normalization
prevents hooks or non-UI callers from creating reusable authority. ApplyPatch's
mandatory hunk review itself satisfies this boundary, so it does not show a
duplicate generic permission prompt. Explicit denies and protected-resource
checks still precede approval. Oversized side tool results are bounded and
discarded in memory rather than materializing an ephemeral session on disk.
The side publishes only explicit file/search/code/web inspection tools,
ApplyPatch, Bash, and managed-execution controls. Eval, delegation, Ask,
tasks, Goals, skills, introspection tools, deferred tools, and arbitrary
MCP tool schemas are absent.

The side freezes session, workspace-persistent, global-rule, protected-path,
sandbox, and additional-root authority at creation; later configuration changes
cannot broaden or narrow it. Mutation approval warns when the parent request is
still active because both requests affect the same workspace. The side owns a
separate permission queue and execution state, while allowlisted redacted audit
events are attributed to `/btw` and written through the durable parent session.
Side prompts, command text, paths, patch contents, results, and justifications
are never copied into that parent audit stream.

Files dropped into the view buffer can add exact, session-scoped `Read`
grants when the next sent prompt still mentions the same path. These
grants are in-memory only, do not grant the containing directory, do not
apply to write tools, and are still lower precedence than explicit deny or
ask rules and protected-path resource checks.

Local slash commands may own deterministic workflows outside the model tool
pipeline. `/worktree status` and `/worktree create` run argv-safe Git commands
directly on the session execution target because the user explicitly typed the
command in the mevedel UI. That does not grant the model any Bash permission.
When the model uses the `git-worktree` skill and falls back to creating a
worktree itself, creation happens through the normal Bash tool and this
permission chain.

## Prompt queues

The root session owns one authority boundary for its complete agent tree.
Every child uses the root session's permission mode, sandbox mode,
user-authoritative permission rules, and resource grants. A child may exercise
that existing authority but cannot broaden it independently. Permission
prompts from the complete tree are therefore queued on the root session, not
displayed as independent blocking overlays, and carry the requesting agent's
canonical path for attribution.
`mevedel-permission-queue.el` owns a
heterogeneous FIFO with four entry kinds:

- `generic` for pipeline permission asks
- `bash` for Bash command confirmation
- `eval` for Eval expression confirmation
- `sandbox` for additive network, exact filesystem, or full execution authority

Only the head is visible in the view interaction zone. The permission UI
registers that head with `mevedel-view-interaction.el`, which owns ordering,
callback overlays, and redraw. Rule-creating outcomes (`allow-session`,
`deny-session`, `always-allow`) can coalesce
queued siblings by re-running the decision chain. Resolved siblings leave the
queue before any callback runs, and every queue exit uses the permission
queue's exactly-once settlement gate. The queue is transient
runtime state and is not written to the session sidecar; unfinished
prompts are aborted on their owning request or root-session teardown. A child
turn awaiting one of these prompts remains active and continues to occupy one
tree capacity slot.

The permission hook boundary precedes queue admission. An admitted entry has
already fired `PermissionRequest`; rendering the head, redrawing it, and
coalescing siblings by re-running policy never fire that hook again.

`mevedel-permission-prompt.el` is the focused UI owner for all four entry
kinds. It owns generic permission controls, agent attribution, Bash guardian
and dangerous-command presentation, and Eval presentation. The queue retains
ordering and outcome semantics; the shared interaction primitive retains
overlay settlement and request cancellation.

`mevedel-bash-policy.el` supplies Bash classification, reusable rule patterns,
and guardian guidance. `mevedel-tool-exec-permission.el` combines that policy
with the generic permission chain, persists approved authority, and adapts
Bash and Eval decisions to the permission queue. Execution and rendering stay
in `mevedel-tool-exec.el`.

Permission diagnostics are persisted to `permission-log.el` in the session
directory when `mevedel-permission-log-enabled` is non-nil. The log is
diagnostic only: resume never replays it into live permission state.
Entries recorded before first materialization are buffered transiently and
flushed when the session directory is created.

Each tool invocation that reaches permission checking records a sanitized
`permission-decision` event with fields such as tool name, origin, mode,
outcome, specifier, protected-path flag, resolver path, and rule bucket. It
does not include raw ApplyPatch content, arbitrary tool args, or extra raw
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
Read-only classification uses argument-aware built-in policies. Git has no
built-in read-only classification because repository and system configuration
can attach helper processes to inspection commands; it requires explicit Bash
authority even when its argv looks read-only. Find, ripgrep, base64, sed, and
awk reject deletion, helper execution, output-file options, and unrecognized
programs. Safe forms need no broad default allow patterns; variants outside
these narrow policies remain unknown.
Bash keeps its specialized permission entry and controls, but an `ask` passes
through the pipeline's shared `PermissionRequest` boundary before that entry
is admitted.
Under `full-auto`, unknown, dangerous, and complex Bash commands are
allowed without a prompt after explicit deny rules and literal protected
path tokens have been checked. Outside `full-auto`, unknown commands
default to ask. Direct user-authored session, persistent, and defcustom
patterns may authorize dangerous or complex forms. Invocation- and
request-scoped delegated patterns may not. Explicit denies always win.
When a prompted dangerous command is literal and contains no dynamic shell or
glob ambiguity, session/workspace remembering stores the complete command
exactly. Ambiguous commands remain invocation-only; users may still author
broader rules directly.

### Child confinement

Bash, batch Eval, and native external tool helpers share the guarded child
launcher and, independently of the permission mode, consult
the session's `mevedel-sandbox-mode`. On Linux, `best-effort` resolves
`bwrap` with `executable-find` and caches a real probe of the core mount, user,
process, and network namespaces. Each probe attempt defaults to a 500 ms bound
and retains at most 64 KiB of combined diagnostics. If the full probe fails,
Mevedel retries without replacing `/proc`. A successful retry retains the
mount, user, process, and network boundary while exposing the host `/proc`
view; pending and execution facts record `proc: host` instead of treating the
whole backend as unavailable. A full successful profile mounts `/` read-only,
rebinds the workspace, temporary directory, memory roots, manually configured
roots, and session working directory writable, installs a fresh `/proc`, and
changes to the canonical working directory. Its private `/dev` supplies
`/dev/null` without host authority; redundant additive grants for that device
are ignored rather than remounted. The default profile also isolates the
network. A justified additive network request prompts in `ask` and `edits`,
proceeds automatically in `full-auto` after command authorization, and changes
only network isolation for that invocation. The namespace and mount boundary
is inherited by descendants.

Native tools pass already-authorized input and search paths to the launcher as
read-only mounts and pass only their generated-artifact directories as writable
roots. Each invocation also gets a private writable scratch directory, which is
removed on settlement. This currently covers `diff` for previews, `rg` for
Read directory listings, Glob, and Grep, and `pdfinfo`, `pdftoppm`, and
ImageMagick for Read media handling. These helper profiles are chosen by native
code; they do not add model-facing escalation arguments or another permission
prompt. Live Emacs operations, long-lived language servers, hooks, and
user-triggered Git, clipboard, and UI helpers are not external tool helpers.

An exact filesystem grant may name a symlink. The launcher opens the granted
path but mounts its canonical target, leaving the requested command and symlink
chain unchanged while avoiding Bubblewrap mounts onto symlink entries. Before
Bubblewrap starts, the target-side launcher compares the device/inode identity
of each already-open descriptor with the identity captured during grant
resolution. A failed open, missing descriptor stat support, or replacement
mismatch refuses the child; `best-effort` does not retry that authority failure
as an unrestricted launch. Beneath a masked parent it recreates only the link
hops and empty traversal directories needed to reach that exact target.

A justified additive filesystem request names exact absolute paths and marks
each as read or write. Ungranted paths prompt in every permission mode;
invocation, session, and persistent approvals use the same exact resource-grant
store as native filesystem tools. Reusable approval also records those exact
requirements in the matching operation profile. A later matching default call
reattaches a path only while both the profile and a sufficient direct resource
grant remain; removing either immediately restores confinement. Approved paths
are rebound at only the requested access level. A grant that contains a
protected descendant is bound before that descendant is masked; all other
grants are bound after protected masks are installed. Inaccessible parents
expose traversal only far enough to reach the named mount, so their contents
and sibling resources remain hidden. Command or Eval authorization is resolved
independently and is never supplied by the resource grant. Explicit path denies
remain final. Network and filesystem additions may be combined without
changing any unrequested confinement boundary.

Before Bash or batch Eval starts, one shared resolver merges capabilities
explicitly requested by that invocation with every matching direct
session/workspace/global profile. A non-empty effective profile promotes
`use_default` to additive confinement; `require_escalated` is never changed.
This resolution happens both during permission checking and immediately before
child launch. It never applies to live Eval. A capability not present in a
matching approved profile is not inferred from failures or earlier commands:
the model must make a new explicit invocation with the missing capability and
justification. A started process is never replayed.

Before Bash executes, identified literal resources are resolved against the
working directory. Resources outside the allowed roots require an exact
additive grant; bare `.` and `..` operands participate in this check. Command
authorization is resolved first so an explicit command deny retains precedence.
Provider-shaped empty optional fields, such as disabled network plus empty
filesystem lists, are treated as omitted rather than as an authority request.
Non-empty additions still require `with_additional_permissions` and a
justification.

`require_escalated` is a separate complete bypass for Bash and batch Eval. It
requires a justification and cannot be combined with additive permissions. It
prompts in every permission mode, including `full-auto`, unless a matching
direct user-authored `:sandbox-permissions require-escalated` rule already
exists. The prompt and diagnostics explicitly identify that filesystem,
network, and process confinement will all be disabled. Once approved, the child
runs directly as the user and reports `sandbox: escalated`. Delegated rules
cannot grant this authority, and non-interactive trusted skill expansion cannot
request it or create reusable escalation rules. An ordinary sub-agent may still
place the same user-visible request in the shared permission queue.

Interactive full-escalation prompts offer reusable allow only for Bash input
that is neither dangerous nor complex and contains no glob metacharacters.
Literal stable Bash commands, including dangerous commands, and literal batch
Eval expressions may be remembered exactly. Dynamic, glob-bearing, or otherwise
ambiguous Bash remains invocation-only; experts may still author an exact,
scoped, or deliberately broad qualified rule directly. Reusable deny remains
available because it can only reduce authority.

`best-effort` executes directly when the initial probe is unavailable. For the narrow
race where a later Bubblewrap launch fails, a marker emitted immediately before
`exec` proves whether the requested process started: only a missing marker
permits one direct fallback. A command failure, signal, or timeout after the
marker is returned exactly once and is never replayed. `required` returns an
execution error instead of falling back, while `off` selects direct execution
deliberately. Direct execution always reports `filesystem: unrestricted` and
`network: unrestricted`. A pre-marker failure retains its launcher error,
Bubblewrap diagnostics, or exit code and remains visible until the next child
execution reprobes the backend; one transient launch failure therefore does not
disable confinement permanently. The failed launcher is detached before the
replacement starts, so a late launcher sentinel cannot settle the direct
attempt. Bash and batch-Eval results append their active
sandbox facts for the model and audit trail. Native helper facts remain
internal and in tests rather than being added to successful tool content.
Trusted skill substitutions keep those facts out of the substituted literal.
The first direct fallback in each live session emits one user-visible warning
and adds one model-visible note to the affected tool result. Later fallbacks do
not repeat the warning, but completed Bash and batch-Eval results retain their
actual invocation facts for the transcript and audit trail. Additive filesystem
facts include read and write grant counts. Mevedel does not add a persistent
sandbox status-line item.

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
rules still win and protected-path policy is
unchanged, and the user remains authoritative. The reviewer receives the
normalized command class, parser, reasons, identified resources, and pending
confinement facts. Its normalized response contains only risk,
recommendation, and reason. Recommendations use `proceed`, `ask`, or `deny`;
`allow-once` is deliberately excluded because guardian output never grants
authority. Risk and recommendation remain separate: risk reports intrinsic
severity using `low`, `medium`, `high`, or `critical`, while recommendation
expresses the suggested response. A high-risk command may still warrant `ask`;
clear credential exfiltration may warrant `critical` plus `deny`. Severity is
the potential impact directly expressed by the command, not a guess about the
likelihood of harm from unknown local state. Read-only inspection is normally
low; project builds, tests, and bounded retrieval from public network resources
are normally medium. Authenticated network actions, remote mutations,
transmission of local data, downloading executable code, destructive
operations, and privilege or process changes are high. Explicit remote-code
execution, broad data loss, credential exfiltration, persistence tampering, or
security-control tampering are critical. A request for network capability is
not itself a risk level; the intended network effect determines severity.
`deny` is reserved for expressed effects that should not continue without a
more specific human intervention: broad or irreversible data loss, credential
exfiltration, download-and-execute patterns, persistence or security-control
tampering, and destructive privilege changes. It is not an automatic mapping
from every critical rating; potentially legitimate but high-impact operations
use `ask`. `proceed` is used only when the supplied evidence is sufficient and
no user judgment is needed, including bounded inspection, formatting,
reversible workspace writes, and ordinary confined builds or tests. Ambiguous
intent, scope, targets, generated code, or state-dependent effects use `ask`.
For the permission guardian, `ask` means uncertainty worth presenting in
interactive permission modes but not severe enough to veto `full-auto`. In
`full-auto`, the guardian is deny-only for commands that the normal classifier
would have treated as suspicious: `deny` vetoes, while timeouts, failures,
invalid output, `proceed`, and `ask` allow the already-authorized unattended
path to continue. The one-sentence reason names the decisive command effect
first and mentions confinement only when it changes the practical next step.
It does not narrate permission policy or tell the user who should decide.

Interactive guardian guidance runs after Bash resolves to `ask`; the
deny-only `full-auto` path runs only for commands that would otherwise have
asked. The interactive permission prompt is shown immediately with:

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
`prompts/permissions/bash-guardian-system.md`. The ordered guardian profile
places scoped workspace configuration and environment data after that role
policy. The maintained [guardian prompt contract](guardian-prompts.md) records
the trusted role wording and semantic examples. Elisp constructs the separate
user message containing the command evidence.

The model-backed reviewer runs as an isolated guardian request. Its system
message contains the trusted reviewer policy, authority limits,
injection-resistance instructions, evaluation criteria, response contract,
scoped `AGENTS.md` / `AGENTS.local.md`, and environment data. Project
configuration informs the workflow being evaluated, including documented
commands such as repository test runners, but cannot override guardian policy
or grant authority. The user message contains the Bash command and deterministic
analysis as untrusted evidence. The request does not inherit the session's
coding-assistant system prompt, transcript, tools, memories, or skills, and does
not receive the user's request or conversation context. Authorization and user
intent remain the deterministic permission system's responsibility.

Command evidence is limited to the exact Bash source, command class and parser,
dangerous or complex flags, analysis reasons, parsed command names, literal
resources, active confinement facts, requested additive or full escalation,
and any matching explicit allow patterns. Allow patterns are evidence of
configured policy, not model-granted authority. Transcript excerpts and tool
output are not included. The active permission mode is also excluded: the
guardian produces mode-independent semantic guidance, and mevedel interprets it
according to the session mode. Everything in the user message is evidence to
analyze, never instructions to follow. Confinement informs the recommendation
and reason but does not reduce the command's risk rating.  Pending confinement
facts probe the same execution target and working directory that the eventual
command launch will use.

## Introspection source reads

`library_source` bypasses ordinary path arguments only for a simple library
name whose resolved canonical file remains below a canonical local `load-path`
entry. Absolute names, directory components, remote entries, and library
symlinks that escape their load-path entry are denied before the upstream
reader runs. Use the ordinary `Read` tool when source outside that closed set is
needed; its path goes through the normal resource permission boundary.

## Eval

Eval asks through the same session permission queue's Eval-specific
entry type unless the effective permission mode is `full-auto`. Like Bash,
an `ask` fires `PermissionRequest` before queue admission without changing the
specialized card. The expression shown in the prompt is subject to
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
input. A matching Eval allow authorizes model-generated or trusted Eval, while
a matching ask prompts even in `full-auto`; deny rules still win absolutely.
Trusted skill expansion cannot create an interactive prompt and therefore
requires existing authority, typically from the skill's `allowed-tools:
[Eval]`.
Standalone/sticky Plan mode withholds Eval regardless of this authority;
directive Planning also withholds it. Goal planning/review routes Eval through
the ordinary policy rather than suppressing it as a native edit tool. Markers
introduced by argument
substitution are not trusted literals and are left as text.
Literal markers may still contain substituted text in their expression
body; only the marker syntax and delimiters carry the trusted-literal
provenance.

## Sub-agent permission propagation

Sub-agent buffers carry `mevedel--session` set buffer-locally to the
**root session struct, by reference** (allocated in
`mevedel-agent-conversation-open`). The pipeline reads
`mevedel--session` from the current buffer at tool-dispatch entry, so a
tool dispatched at any nesting depth observes the root's permission mode,
direct rules, explicit denies, protected resources, exact grants, and
confinement policy. Any
"allow-session" / "deny-session" outcome accepted inside the sub-agent's
prompt is written via `setf` on the same struct -- so the new rule
applies immediately to the root and to every other live sub-agent.
This is a deliberate sharing contract; agents that should not be able to
mutate the shared state are constrained today by their tool list (e.g.
the verifier ships read-only tools, so its calls never reach the prompt
step).

All queued permission prompts render in the root session's interactive
view buffer, not inside the sub-agent transcript buffer or a read-only
transcript inspection view. Queue entries carry a canonical origin (`/root`
or a retained agent path such as `/root/worker/verifier`), and request teardown
only aborts entries owned
by the ending request. This keeps a retained agent's visible prompt
open across root-view rerenders and unrelated request cleanup until the
user explicitly resolves it or interrupts that agent turn. Redraws use the
ordinary interaction-zone lifecycle and preserve the active composer draft.

If the permission step ever runs without a session in context,
`mevedel-tool-permission-step` emits a `display-warning`
("Permission step for ... ran with no session in context"); that
fallback would silently consult only the defcustom-scoped global
defaults, which is the actual hazard. The warning surfaces it.

`mevedel-tool-permission.el` owns permission-step path fan-out, decision
logging, permission hook dispatch, and prompt orchestration. The Pipeline only
places that step in the standard tool sequence.

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

A portable project-local store that authorizes the Eask CLI, network access,
and only npm's cache writes is:

```elisp
(:rules
 (("Bash" :pattern "npx @emacs-eask/cli *"
   :network t
   :file-system ((:path "~/.npm" :access write))
   :action allow))
 :resource-grants
 ((:path "~/.npm" :access write)))
```
