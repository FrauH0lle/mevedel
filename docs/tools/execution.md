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

Edits refuses execution when confinement is unavailable. Ask may select direct
execution under its configured best-effort preference. Full Access already
authorizes unrestricted execution without a separate escalation request. Direct
results disclose that boundary; it does not expand the user's task scope.

## Yielding, polling, and input

Bash waits for `yield_time_ms`, then returns an `execution_id` if still running.
Use WriteStdin with empty `chars` to wait for new unread output. Poll the same
execution until it completes; a quiet interval does not establish completion.
ListExecutions lists yielded commands owned by the calling agent, and
StopExecution stops one of those commands.
There is no automatic command timeout; a native `timeout` command can enforce
a deadline. Shell backgrounding (`&`) bypasses this managed lifetime and should
be replaced with a small yield interval.

Pipe-mode stdin is closed. `tty=true` provides a PTY whose stdin stays writable
for prompts, REPLs, and tools requiring a terminal. WriteStdin sends ordinary
input only to PTY executions. A single Ctrl-C character interrupts the process
group in either PTY or pipe mode. Do not send input to a finished execution or
start a replacement merely because no new output arrived.

### Transcript presentation

The initiating Bash row owns the command, current state, and output. Its compact
header shows the command, status, and elapsed time; expanded details contain the
execution ID, working directory, output counts, routine exit facts, the full
command, and bounded output. Output is collapsed by default, including while
the command runs. Explicitly opened sections stay open through progress and
settlement. Consequential sandbox warnings and truncation remain visible, and
retained output remains accessible where available. A collapsed row marks a
truncated whole-execution preview even when each individual poll returned its
entire unread chunk; observation omission counts remain unchanged.
Live progress displays the execution owner's bounded output tail without an
additional line cap, and retains material sandbox disclosures as the row updates.
Launch failures retain their diagnostic separately from stdout: expanding the
original Bash row (including a ToolCall child) or its read-only fallback shows
the failure cause even when the command produced no output. Errors after launch
remain visible without being mislabeled as startup failures.
When the original row is gone, read-only evidence retains material sandbox
disclosures and marks a truncated preview unless a readable retained output
artifact accounts for the execution's reported output bytes. A readable older
snapshot warns that later output may be missing; if the byte count is unknown,
a truncated preview remains marked as such. Forwarded completion facts retain
the omitted-byte count for mailbox-only result links. An unsolicited process
signal is labeled
`signaled`, and its number is a signal, not an exit code. It is not presented
as a requested interruption; an INT requested through managed input or user
control is `interrupted` only when the process actually terminates from SIGINT.
Its signal number is likewise not an exit code. A requested stop may settle
after TERM or after an ordinary exit, so its ambiguous numeric status is not
presented as either a signal or an exit code.
An ignored interrupt followed by normal exit or another signal keeps its
observed terminal classification. Requested stops remain distinct.
An execution stopped by the output limit retains a truncation warning even
when its bounded spool is readable; that artifact does not contain output
beyond the limit.

Successful empty-input WriteStdin observations are still returned to the model,
but do not create separate transcript rows, even when they collect new output
or the terminal result. That output belongs to the initiating Bash row. Actual
input and stop actions remain compact linked interactions; submitted input is
available on disclosure rather than in the collapsed label. Their `Show result`
link opens the original Bash row, including a ToolCall child, or shows retained
read-only evidence (or an explicit absence) if the row is unavailable. Failed control
operations, such as a permission denial or invalid execution handle, remain
visible. A command's nonzero exit is recorded on the command, not presented as
a failed input or polling operation that successfully observed it. The model
still receives the original failure status, command outcome, and output.
Retained tool records remain inspectable through
the execution-history disclosure. These rules also apply within ToolCall.

Only a command that actually yielded receives a completion breadcrumb. Whether
completion was polled or delivered independently, the receiving transcript
shows one linked line, such as `↳ Finished: ./run-tests  [Show result]` or
`↳ Failed: ./run-tests · exit 1  [Show result]`. Inside an activity group the
line folds into the group and is counted there, as in
`ran 2 commands, 1 command finished, 1 failed`. Foreground commands have no
breadcrumb. The breadcrumb repeats neither output nor a metadata summary;
`Show result` opens the original execution output, including from an older
segment or an agent transcript. If the original row is unavailable, navigation
uses retained read-only evidence or states that evidence is missing. Agent
answers and execution completions remain separate. Hiding these redundant
presentation rows does not remove the model-visible results or change polling,
delivery, execution ownership, or process lifecycle.

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
