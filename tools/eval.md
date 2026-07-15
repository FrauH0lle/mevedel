Evaluate one Elisp expression and return the result and any printed
output.

`expression` can be anything to evaluate: a function call, a variable,
a quasi-quoted expression. Only the first sexp is read and evaluated.
If you need multiple expressions, make one call per expression. Do not
combine with `progn` --- go expression by expression.

`mode` controls where the expression runs:

- `live` (default): evaluate in the current Emacs process. This can see
  live buffers, variables, windows, advice, timers, and package state.
- `batch`: evaluate in a child `emacs --batch -Q` process with the
  current `load-path` and the session working directory. On supported systems,
  the child runs with filesystem and process confinement and without network
  by default.

`preserve_ui` applies to `mode=live`. It defaults to true and restores
the current window configuration after evaluation. Set it to false only
when intentional window/frame manipulation is the point of the call.

Instead of saying "I can't calculate that", use this tool to evaluate
the result.

The return value is formatted using `%S`, so strings appear escaped
and literal forms are `read`-compatible where possible. Objects
without a printed representation show as `#<hash-notation>`.

Expressions evaluate with `default-directory` set to the session working
directory.

If an important batch expression fails because it needs network access, make a
new batch Eval call with
`sandbox_permissions="with_additional_permissions"`,
`additional_permissions={"network":true}`, and a concise user-facing
`justification`. Request approval in the tool call rather than asking in prose.
The retry is a distinct invocation; it retains filesystem, protected-path, and
process confinement. Additional sandbox permissions do not apply to live Eval.

Output from `print`, `prin1`, and `princ` is captured and returned as
STDOUT. Use `print` for diagnostic output, not `message` (which goes
to `*Messages*` and is not captured).

### When to use `Eval`

- Testing elisp code snippets or expressions
- Verifying code changes work correctly
- Checking variable values or function behavior
- Demonstrating elisp functionality to users
- Calculating results
- Exploring live Emacs state or testing short hypotheses with `mode=live`
- Running longer or UI-risky Elisp checks with `mode=batch`

### When NOT to use `Eval`

- Multi-expression evaluations -> one call per expression (no progn)
- File modifications -> use `Edit`
- Shell operations -> use `Bash`
- Test commands -> use `Bash`
- Untrusted code -> live Eval is unrestricted code execution, and batch
  confinement is not a substitute for trusting the expression

### Examples of good usage

<example>
- Calculate sum
Eval(expression="(+ 1 2 3 4)")
</example>

<example>
- Check current buffers
Eval(expression="(buffer-list)")
</example>

<example>
- Change setting
Eval(expression="(setq tab-width 4)", mode="live")
</example>

<example>
- Run an isolated package-level check
Eval(expression="(and (require 'mevedel-view) (fboundp 'mevedel-view--setup))", mode="batch")
</example>

### Examples of bad usage

<example>
Eval(expression="(progn (message \"hello\") (message \"world\"))")
<reasoning>
Should make two separate Eval calls instead of using progn.
</reasoning>
</example>

<example>
Eval(expression="(find-file \"/path/to/file.txt\")")
<reasoning>
Use Edit tool for file modifications, not Eval.
</reasoning>
</example>
