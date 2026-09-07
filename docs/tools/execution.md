# Bash and Eval execution

The tool descriptions give ordinary Bash and Eval contracts. This manual
covers exact grants, batch evaluation, and interactive command control; it
grants no authority. The permission fields below apply to Bash and batch Eval,
not live Eval.

## Additive permission requests

Use the tool's `sandbox_permissions`, `additional_permissions`, and
`justification` fields so the host can present the actual request. A separate
prose permission question does not create a grant. Request all capabilities
already known to be necessary together; do not copy unrelated authority from
an earlier command. Matching approved session/workspace profiles are reapplied
by the host.

For required network access:

```text
Bash(command="git fetch origin",
     sandbox_permissions="with_additional_permissions",
     additional_permissions={"network":true},
     justification="Fetch the requested remote branch updates.")
```

For exact protected filesystem resources, use absolute paths:

```text
additional_permissions={"file_system":{"read":["/exact/path"]}}
additional_permissions={"file_system":{"write":["/exact/path"]}}
```

Read and write are separate grants; write permits reading that same resource.
Exact approval does not grant its parent or siblings. The user can explicitly
select a containing directory tree in the permission card; the displayed scope
then covers its descendants. This does not grant other protected paths, network,
or unrestricted processes. Filesystem approval also does not authorize the
command itself; command policy is checked independently. Network and filesystem
grants can be combined in one `additional_permissions` object when both are
needed.

After a likely sandbox/network failure, confirm that the operation is still
needed and request the specific missing capability in a new call. A new call
is not an automatic replay of the failed command. Account for any completed
effects before repeating an operation.

## Full confinement bypass

If a relevant confined attempt failed and additive grants cannot support the
operation, `sandbox_permissions="require_escalated"` requests a full bypass,
with a concise justification. This removes filesystem, network, and process
confinement and runs directly as the user; command approval still applies.
Do not request it merely to avoid a command permission prompt.

When confinement is unavailable, the result discloses unrestricted execution.
That disclosure does not authorize unrelated operations or expand user scope.

## Yielding, polling, and input

Bash waits for `yield_time_ms`, then returns an `execution_id` if still running.
Use WriteStdin with empty `chars` to wait for new unread output. Poll the same
execution until it completes; a quiet interval does not establish completion.
ListExecutions lists your managed commands and StopExecution stops one.
There is no automatic command timeout; a native `timeout` command can enforce
a deadline. Shell backgrounding (`&`) bypasses this managed lifetime and should
be replaced with a small yield interval.

Pipe-mode stdin is closed. `tty=true` provides a PTY whose stdin stays writable
for prompts, REPLs, and tools requiring a terminal. WriteStdin sends ordinary
input only to PTY executions. A single Ctrl-C character interrupts the process
group in either PTY or pipe mode. Do not send input to a finished execution or
start a replacement merely because no new output arrived.

## Eval process and state

Live Eval runs in the host Emacs. Restoring the window configuration does not
undo variable assignments, buffer edits, files written, timers, or other side
effects. Use it only for effects within the authorized task.

Batch Eval starts a separate `emacs --batch -Q`. Its load path and working
directory are supplied, but it does not copy loaded packages or live values.
Load the code whose behavior you need to check in the expression itself:

```text
Eval(expression="(and (require 'cl-lib) (cl-every #'numberp '(1 2 3)))",
     mode="batch")
```

For a batch check requiring protected files or network, use the additive
permission fields above with `mode="batch"` and the Elisp `expression` in
place of a Bash `command`. Full escalation removes child confinement; it
does not copy live Emacs state into the child. A failed expression can already
have changed state before the error, so inspect relevant effects before retrying.

Only one top-level form is read. Compound forms can sequence related operations
and share lexical bindings. `print`, `prin1`, and `princ` contribute STDOUT;
`message` goes to Emacs diagnostics instead. Unprintable objects appear as
`#<...>` and cannot be reconstructed by reading their printed representation.
Batch Eval settles through its callback; the Bash polling controls above do
not apply to it.

## Shell composition and outcomes

Quote paths/arguments for the shell. Double quotes preserve spaces but still
allow variable and command substitution; use literal-safe quoting for text
containing backticks or `$()`. Use `&&` when a later command requires success
of an earlier one. Independent commands can use separate calls; conflicting
mutations must remain sequential.

Raw exit codes are retained. A simple grep/rg no-match, diff difference, or
test/[ false is an expected command outcome with its own classification. These
are distinct from launch errors, permission denial, cancellation, and commands
that actually failed. Inspect output and the settled status before reporting
completion.
