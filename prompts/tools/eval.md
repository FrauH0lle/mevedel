Evaluate one Elisp form and return its value and printed output.

### When to use `Eval`

- Inspect live Emacs state or evaluate an Elisp hypothesis.
- Use batch mode for a check that can run in a separate Emacs process.

### When NOT to use `Eval`

- Applying source edits: ApplyPatch provides a reviewable patch.
- Running untrusted code. Live Eval has unrestricted access to the Emacs process;
  batch confinement does not make the expression trustworthy.

### How to use `Eval`

- Only the first form in `expression` is read and evaluated. A compound form such
  as `let` or `progn` can contain related operations; separate top-level forms
  after it are ignored.
- `mode="live"` (default) sees live buffers, variables, advice, and package state.
  It runs without child-process confinement. `preserve_ui=true` restores the
  selected frame's window configuration, not other side effects; set it false
  for intentional window manipulation.
- `mode="batch"` runs a child `emacs --batch -Q` with the current `load-path` and
  session working directory. It does not inherit live buffers, variable values,
  or loaded packages. Require libraries needed by the check.
- Evaluation uses the session working directory. Values print with `%S`; `print`,
  `prin1`, and `princ` output is returned as STDOUT. `message` is not captured.
  Errors can include output produced before failure. Large results have a bounded
  preview and retrieval address.
- Batch confinement, when available, restricts filesystem/process access and
  defaults to no network. Non-default permission requests apply only to batch:
  `with_additional_permissions` requests network and/or exact absolute read/write
  paths; `require_escalated` requests unrestricted child execution. Put the reason
  in `justification` on the new tool call. Expression authorization is separate;
  approval is not an automatic retry. Results disclose unavailable confinement.
- For exact grant semantics, failure recovery, and batch evaluation details,
  read `mevedel://tools/execution.md` before using those features.

### Examples of good usage

<example>
- Check a library operation without relying on the live Emacs's loaded packages:
Eval(mode="batch", expression="(progn (require 'cl-lib) (let ((values (cl-remove-if-not #'numberp '(1 skip 2)))) (princ values) (apply #'+ values)))")
The returned value is 3; STDOUT contains (1 2). The library is required inside
the single evaluated form because batch mode does not inherit live loaded state.
</example>

### Examples of bad usage

<example>
Eval(expression="(+ 1 2) (* 3 4)") expecting both forms to run
<reasoning>
Only the first form is evaluated. Use one compound form when the operations
belong together, or separate calls when the second depends on inspecting the first.
</reasoning>
</example>
