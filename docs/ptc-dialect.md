# ToolScript dialect manual

ToolScript lets a model make a data-dependent sequence of ordinary mevedel
tool calls inside one model turn. The script is interpreted by mevedel; it is
never evaluated as Emacs Lisp. Every nested tool call still goes
through normal validation, hooks, permissions, snapshots, telemetry, and
rendering.

Use ToolScript for orchestration and light result shaping. Use the ordinary
tool directly for one call, and use Grep or another purpose-built tool for bulk
text processing.

## Values and evaluation

The dialect supports numbers, strings, symbols, keywords, lists, lexical
bindings, and lambdas. `nil` is false; every other value is true. A script may
contain multiple top-level forms, which are evaluated as an implicit `progn`.
Lambdas are internal callables: neither a tool argument nor the script's final
value may contain one.

The evaluator resolves a call's operator before evaluating its arguments. An
unknown function therefore fails without running tool calls hidden in its
arguments.

Operators additionally preflight before execution: the whole parsed script is
walked once, and every operator outside the closed tables is reported in one
error with nearest-match suggestions, before any nested tool runs. Because
operators never resolve through the lexical environment, this static check is
exact for evaluated positions; unknown names in never-taken branches are
rejected too. Quoted data, binding names, and lambda parameter lists are
skipped. Literal regexp arguments to the regexp primitives are validated in
the same pass.

`let` evaluates all initializer expressions in the surrounding environment:

```elisp
(let ((paths (Glob :pattern "*.el"))
      (count (length paths))) ; wrong: paths is not bound here
  count)
```

Use `let*` when a later initializer depends on an earlier binding:

```elisp
(let* ((paths (Glob :pattern "*.el"))
       (count (length paths)))
  count)
```

`setq` changes an existing lexical binding only. It does not create globals.

## Syntax

Special forms:

```text
quote  if  progn  cond  and  or  let  let*  setq  while
lambda  funcall  apply  mapcar  parallel  parallel-map
```

Closed, macro-shaped conveniences:

```elisp
(when CONDITION BODY...)
(unless CONDITION BODY...)
(push VALUE VARIABLE)
(dolist (VARIABLE LIST) BODY...)
(dotimes (VARIABLE COUNT) BODY...)
```

`push` accepts only a plain bound variable as its place. `dolist` and
`dotimes` accept only the two-element specification; there is no result form.
The dialect has no `macrolet`, `cl-loop`, generalized places, or
guest-visible macro expansion.

The ToolScript tool description generates exact signatures for every pure data
primitive from the interpreter's closed table. Beyond those, only a script's
own top-level definitions are callable. The
table includes the syntactic path helpers (`file-name-nondirectory`,
`file-name-directory`, `file-name-concat`, `file-name-extension`,
`file-name-sans-extension`, `file-name-base` — file-name handlers are disabled
so they never touch remote state), `take`, and a fixed-comparator `sort` that
copies its list and orders ascending with `value<`. Guest closures cannot be
comparators.

## Definitions

A script may open with top-level `defun` and `defmacro` forms:

```elisp
(defun read-or-nil (path)
  (let ((r (Read :file_path path)))
    (if (plist-get r :error) nil r)))

(defmacro with-lines (var call &rest body)
  (let ((text (gensym)))
    `(let* ((,text ,call)
            (,var (split-string ,text "\n" t)))
       ,@body)))

(with-lines lines (Grep :pattern "TODO" :output_mode "content")
  (length lines))
```

Definitions are legal only at the script's top level and are hoisted before
the body runs, so they may reference one another regardless of order. A
definition name must not collide with a special form, convenience, pure
primitive, tool, or earlier definition; nothing is ever shadowed. At least
one non-definition body form must remain.

Parameter lists accept plain names, `&optional`, and one trailing
`&rest NAME`. Other markers such as `&key` are rejected, arity is checked on
every call, and duplicate parameters are errors. The same rules apply to
`lambda`.

Named functions may recurse; a self-call in tail position runs at constant
stack depth, and non-tail recursion is bounded by the stack budget. A
function name is also accepted where a callable is expected, as in
`(mapcar 'name list)`. A macro name is not a value.

One-level backquote builds list templates: `` `(a ,x ,@items) `` expands to
`quote`/`list`/`append` calls. Nested backquote, an unquote outside a
backquote, and the dotted `(a . ,b)` reader convention are rejected.

A macro receives its argument forms unevaluated and runs its body inside the
same closed evaluator, where it may call pure primitives and tools like any
other code. The returned expansion is validated against the reader's contract
and size budgets, then evaluates in the caller's environment. `(gensym)`
returns a fresh uninterned symbol for hygienic expansions. A macro call
inside a loop re-expands on every iteration; hoist it out of hot loops when
that matters.

## Strings and regexps

Strings use normal Lisp escapes. In script source:

```elisp
"\n"                 ; one newline character
"\\"                ; one backslash character
(split-string text "\n" t)
```

Regexp primitives use Emacs regexp syntax. Parentheses are literal unless
escaped, so this matches a line beginning with `(defcustom`:

```elisp
(string-match-p "^(defcustom" line)
```

Escaped parentheses create a capture group. Because the backslash is inside a
Lisp string, it is doubled in script source:

```elisp
(string-match-p "\\(foo\\)" text)
```

These rules apply to guest regexp primitives, not nested tool arguments. Grep
uses ripgrep syntax; for example, `^[(]defcustom` matches a literal opening
parenthesis without adding another layer of backslash escaping.

Use `regexp-quote` when matching model- or tool-produced text literally.
The guest regexp subset is deliberately bounded. `*`, `+`, `?`, and `\{n,m\}`
are allowed on a single atom — one literal, one escaped character, `.`, or one
bracket class — so `"^[0-9]+$"` and `"^[ \t]*[0-9]+"` work. Rejected before
Emacs's backtracking matcher runs: a quantified group, a quantifier stacked on
another quantifier, more than eight quantified atoms, alternation,
backreferences, and group extensions. Adjacent single-atom quantifiers can
still backtrack polynomially; the quantifier cap and the regexp work budget
bound that residual. Literals, anchors, bracket classes, and ordinary capture
groups remain available. The atomic-work estimate accounts conservatively for
polynomial backtracking by raising input size to the number of quantified
atoms; a pattern that is cheap on short text can therefore be rejected on a
larger input before Emacs's matcher runs. `split-string` also rejects an empty separator and a
split whose maximum output cannot fit the guest value budget. The same checks
apply to `string-trim` and `split-string` trim regexps. An omitted split
separator uses a fixed whitespace regexp; it never reads Emacs configuration.

## Tool calls

The bottom of each ToolScript tool description lists the tools available for
that request and their exact keyword arguments:

```elisp
(Read :file_path "mevedel.el" :offset 1 :limit 80)
(Grep :pattern "TODO" :path "." :output_mode "files_with_matches")
```

A successful nested call returns its canonical result, normally a string —
exactly what that tool returns in conversation, including its documented
formatting. `Read` output carries `cat -n` style line-number prefixes and may
end with a truncation notice; `Grep` count mode returns absolute `path:count`
lines; `Glob` returns newline-separated absolute paths. Session-level Read
duplicate suppression does not apply to nested calls: a script always receives
file content, and its reads do not poison the conversation's own
duplicate-read state. A failed nested call returns a guest plist:

```elisp
(:error "message")
```

Scripts can inspect that value and continue:

```elisp
(let ((result (Read :file_path path)))
  (if (plist-get result :error)
      (list path :unreadable)
    (list path (length result))))
```

Permission denial is different: it aborts the whole ToolScript call. Completed
nested calls remain visible in the audit, but the script cannot catch the
denial.

## Parallel calls

Use `parallel` when a fixed set of calls is independent:

```elisp
(parallel
  (Read :file_path "a.el")
  (Read :file_path "b.el"))
```

Use `parallel-map` for the same call shape over a list:

```elisp
(parallel-map
  (lambda (path) (Read :file_path path))
  paths)
```

Results stay in source or input order even when calls finish out of order. The
host controls maximum concurrency. Every `parallel` entry must be one direct
tool call. The `parallel-map` lambda takes one argument and contains exactly
one direct tool call. Argument expressions may use pure data primitives, but
may not make nested tool calls of their own.

Use sequential forms when a later call depends on an earlier result.

## Limits and performance

ToolScript bounds script bytes, syntax nodes, nesting depth, evaluation steps,
wall time, nested-call count, recursion depth, transformed syntax size, regexp
work, numeric size, individual values, and cumulative retained values. Errors name the
exceeded budget. Final rendering counts repeated references at their serialized
size, so a small shared object cannot expand into an oversized result while
printing.

These limits are intentionally generous for orchestration and restrictive for
bulk computation. Prefer:

```elisp
(Grep :pattern "^(defcustom" :path "." :output_mode "count")
```

over reading every file and scanning every line in the interpreter. Narrow
wide searches before mapping over their results.

The interpreter yields between short computation slices, so Emacs remains
interactive. Scripts in flight are runtime state: if Emacs exits or the session
is recovered, the ToolScript call settles as interrupted and does not resume.
For root-session scripts, only the envelope call and its bounded ordered child
audit are checkpointed, and the checkpoint is written durably twice: once
before the first nested call and once at settlement. Retained-agent scripts
skip this checkpoint because their own interrupted-turn handling settles them.
Between the root-session writes, child audit progress is journaled in memory
only (an unrelated autosave captures it
opportunistically) — per-child sidecar writes dominated script runtime,
serialized parallel batches, and cost one remote round-trip each on TRAMP
targets. A crash mid-script therefore recovers the child audit as of the last
autosave, not the last child. Recovery turns the surviving checkpoint into an
ordinary ToolScript tool row, marks queued or running children interrupted,
and consumes the checkpoint with the repaired segment; no lexical environment,
stack, timer, or continuation is serialized.

## Security boundary

Guest identifiers are read into a private symbol table. Calls resolve against
closed tables of special forms, audited syntax transformers, pure primitives,
the script's own top-level definitions, and the request's ToolScript tool
roster. Unknown names fail closed.

The guest cannot evaluate host Lisp or access buffers, processes, files,
environment variables, user identity, time, or package state directly. It can
reach external state only through nested tools, where normal mevedel authority
and audit rules apply.

Host `macroexpand-all` is never run on guest text. The closed syntax
transformers are dispatched only when their names occur in evaluated operator
position, so quoted data and binding positions remain opaque. Guest `defmacro`
bodies run inside the same closed evaluator with the same tables and budgets,
so a user macro is a power feature, not a widening of this boundary.
