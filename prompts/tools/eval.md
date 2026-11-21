Evaluate one Emacs Lisp form for editor inspection or Lisp computation; return
its value and output.

### When to use `Eval`

- Use live mode when the result depends on the user's running Emacs state.
- Use fresh mode for isolated Emacs Lisp checks that do not need live state.
- Use sandbox mode for isolated, stateful Lisp define/call/redefine work.

### When NOT to use `Eval`

- Use Bash for shell commands. Do not wrap them in `shell-command-to-string`,
  `call-process`, or similar merely to run them through Eval. Appropriate Emacs
  APIs are fine.
- Ordinary file reads, searches, and source edits: prefer Read, Glob, Grep, and
  ApplyPatch.
- A Bash confinement or permission failure: use Bash's recovery path, not live
  Eval to escape that boundary. A permission denial is not a tool-routing hint.
- Untrusted code: live Eval is unrestricted; confinement does not make code safe.

### How to use `Eval`

- Supply one top-level form in `expression`; later forms are ignored. Use `let`
  or `progn` for related operations. All modes use the owner's working directory.
- `mode="live"` (default) runs on Emacs's main thread without confinement. Only
  the user's `C-g` can interrupt it. `preserve_ui=true` (default) restores the
  selected frame's window configuration, not other side effects; use false only
  for intentional window changes.
- `mode="fresh"` starts a clean `emacs -Q --batch` child with the current
  `load-path`, not live state or loaded packages. Require needed libraries in
  the form. Fresh Eval is refused for remote execution targets.
- `mode="sandbox"` reuses a child per session/owner. Earlier definitions
  apply; results report started/reused and feature count. `reset=true` discards
  them. Busy calls refuse, including the current call's ID only after yield.
  Sandbox is automatic in Ask/Edits/Full Access but always
  confined (workspace writes, no network or grants); remote targets or unavailable
  confinement refuse it. Use fresh for per-call authority.
- Fresh and sandbox `yield_time_ms` defaults to 10000 (range 250–30000). If still running,
  Eval returns an `execution_id`: poll with WriteStdin, list with ListExecutions,
  or kill the child with StopExecution. Non-empty input is refused. Root-owned
  completion wakes an idle root; polling just to keep work moving is unnecessary.
  No automatic timeout. Live runs inline and ignores `yield_time_ms`.
- `Result:` prints the value with `%S`; `STDOUT:` holds `print`/`prin1`/`princ`
  output; `MESSAGES:` holds `message` text, still visible to the user in live
  mode. Errors retain prior output and include a bounded `BACKTRACE:`. Large
  results have a bounded preview and retrieval address, except sandbox sections,
  which truncate in the child with a marker.
- In Ask/Edits, live Eval goes to the selected permission reviewer. Automatic
  review typically approves task-relevant observational calls without involving
  the user; mutations, unknown effects, and unclear scope go to the user.
  Optional `justification` explains a live call to its approver.
- In Edits, fresh Eval is automatic with required confinement and no network
  by default. Ask uses the configured sandbox preference and expression policy.
  Full Access runs live/fresh automatically without confinement. Explicit denies
  and Plan limits remain.
- Fresh-only authority fields: `sandbox_permissions="with_additional_permissions"`
  and `additional_permissions` request network or exact absolute read/write paths;
  `require_escalated` requests a full confinement bypass. Include `justification`.
  For grant shapes, escalation prerequisites, and recovery, read
  `niwa://tools/execution.md`; if unavailable, do not guess. Unavailable
  confinement is disclosed; approval is not a retry.

### Examples of good usage

<example>
Eval(mode="live", expression="(list :emacs-version emacs-version :gptel-loaded (featurep 'gptel))")
Returns bounded runtime metadata from the current Emacs without changing it.
</example>

<example>
Eval(mode="fresh", expression="(progn (require 'cl-lib) (message \"checking\") (princ \"sum\") (cl-reduce #'+ '(1 2)))")
Returns value `3`, STDOUT `sum`, and MESSAGES `checking`.
</example>

### Examples of bad usage

<example>
Eval(expression="(shell-command-to-string \"git status --short\")")
Use Bash(command="git status --short", description="Inspect workspace change status")
instead; this command does not need live Emacs state.
</example>

<example>
Eval(expression="(+ 1 2) (* 3 4)")
Only `(+ 1 2)` runs; use one compound form if both operations belong together.
</example>
