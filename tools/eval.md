Evaluate an Elisp expression and return the result and any printed
output.

`expression` can be anything to evaluate: a function call, a variable,
a quasi-quoted expression. Only the first sexp is read and evaluated.
If you need multiple expressions, make one call per expression. Do not
combine with `progn` --- go expression by expression.

Instead of saying "I can't calculate that", use this tool to evaluate
the result.

The return value is formatted using `%S`, so strings appear escaped
and literal forms are `read`-compatible where possible. Objects
without a printed representation show as `#<hash-notation>`.

Expressions evaluate with `default-directory` set to the session working
directory.

Output from `print`, `prin1`, and `princ` is captured and returned as
STDOUT. Use `print` for diagnostic output, not `message` (which goes
to `*Messages*` and is not captured).

### When to use `Eval`

- Testing elisp code snippets or expressions
- Verifying code changes work correctly
- Checking variable values or function behavior
- Demonstrating elisp functionality to users
- Calculating results
- Quickly changing user settings or checking configuration
- Exploring Emacs state or testing hypotheses

### When NOT to use `Eval`

- Multi-expression evaluations -> one call per expression (no progn)
- File modifications -> use `Edit`
- Shell operations -> use `Bash`

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
Eval(expression="(setq tab-width 4)")
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
