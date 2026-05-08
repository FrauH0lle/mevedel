# Permission system

Single decision function `mevedel-check-permission`. Nine-step chain:

1. Extract specifier values via `get-path` / `get-pattern` / `get-domain` /
   `get-name` slots
2. Deny rules (across all buckets — see bucket precedence below)
3. Protected paths (`.git/`, `.ssh/`, `.gnupg/`) → ask
4. Tool's own `check-permission` slot
5. Allow/ask rules (innermost-bucket-first — see bucket precedence below)
6. Inside workspace → allow (implicit)
7. Outside workspace with no covering rule → ask
8. Permission mode
9. Default: ask

## Bucket precedence

Steps 2 and 5 consume rules from multiple buckets, in this order:
invocation `skill-permission-rules`, request `skill-permission-rules`,
session rules, persistent rules, defcustom `mevedel-permission-rules`.

- Step 2 (deny) is absolute — any bucket's `deny` wins.
- Step 5 (allow/ask) is innermost-first — the first bucket yielding any
  decision wins.
- Plan-mode exception: under `mode = plan`, the skill buckets are
  suppressed from step 5 for non-read-only tools (skill grants cannot
  bypass plan mode).

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
`deny > ask > allow`; protected paths always prompt.

Modes: `default` / `accept-edits` / `plan` / `trust-all`.

Prompt offers 5 choices (allow/deny × once/session/always). Persisted
rules live in `.mevedel/permissions.el`.

## Prompt queues

Permission prompts are queued on the session, not displayed as
independent blocking overlays. `mevedel-permission-queue.el` owns a
heterogeneous FIFO with three entry kinds:

- `generic` for pipeline permission asks
- `bash` for Bash command confirmation
- `eval` for Eval expression confirmation

Only the head is visible in the view interaction zone. Rule-creating
outcomes (`allow-session`, `deny-session`, `always-allow`) can coalesce
queued siblings by re-running the decision chain. The queue is transient
runtime state and is not written to the session sidecar; unfinished
prompts are aborted on request/session teardown.

## Bash specifics

Bash has domain logic in `check-permission`: parses commands, enforces
`mevedel-bash-dangerous-commands` blocklist, fails safe under
`mevedel-bash-fail-safe-on-complex-syntax` on variable expansion /
`eval` / `exec` / here-docs / brace expansion. Bash does not use the
pipeline's generic permission prompt; when it needs a decision it
enqueues a Bash-specific permission entry. Unknown commands default to
ask even under `trust-all`. The dangerous blocklist only downgrades
`allow` to `ask`; explicit `deny`/`ask` wins.

Skill body shell expansion passes a trusted-literal flag for
author-written commands so the dangerous-command and complex-syntax
heuristics do not fire. Explicit deny rules still win.

### Bash guardian guidance

`mevedel-permission-guardian` can add model-reviewed risk guidance to
Bash prompts. It is advisory only: the normal permission chain still
decides `allow` / `ask` / `deny`, explicit deny rules still win, plan
mode and protected-path policy are unchanged, and the user remains
authoritative.

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

Set `mevedel-permission-guardian` to `t` to use the current gptel model,
or to a custom `(lambda (command context callback) ...)` classifier for
tests or local policy. `mevedel-permission-guardian-timeout` controls the
wait for reviewer output; the default is 20 seconds. The model prompt
lives in `prompts/permissions/bash-guardian.md`.

## Eval

Eval always asks unconditionally through the same session permission
queue. The expression shown in the prompt is subject to
`mevedel-eval-expression-display-limit`.

Skill body elisp injections (`!el` inline and ` ```!el ` fenced blocks)
are the exception: they pass a trusted-literal flag because the
expression is author-written SKILL.md content, not model-generated Eval
input. A trusted elisp injection may bypass the prompt only when an
active unqualified `Eval` allow rule covers it, typically from the
skill's `allowed-tools: [Eval]`. Eval deny rules still win absolutely,
and plan mode suppresses skill-bucket Eval allows. Markers introduced
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
avoid matching `lsof`. Parseable: simple commands, chains (`&&`/`||`/`;`),
pipes, command substitutions (incl. nested), sudo/env/nice prefixes.
Fails safe: variable expansion (`$VAR`), `eval`/`exec`, here-docs, brace
expansion, unbalanced quotes.
