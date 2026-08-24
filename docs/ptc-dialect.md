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
```

`push` accepts only a plain bound variable as its place. `dolist` accepts only
the two-element `(VARIABLE LIST)` specification; there is no result form. The
dialect has no user-defined macros, `macrolet`, backquote, `cl-loop`,
generalized places, or guest-visible macro expansion.

The ToolScript tool description generates exact signatures for every pure data
primitive from the interpreter's closed table. Nothing else is callable.

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
The guest regexp subset is deliberately linear: repetition operators,
alternation, backreferences, and group extensions are rejected before Emacs's
regexp matcher runs. Literals, anchors, bracket classes, and ordinary capture
groups remain available. `split-string` also rejects an empty separator and a
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

A successful nested call returns its canonical result, normally a string. A
failed nested call returns a guest plist:

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
wall time, nested-call count, transformed syntax size, regexp work, numeric
size, individual values, and cumulative retained values. Errors name the
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
Only the envelope call and its bounded ordered child audit are checkpointed.
Recovery turns that checkpoint into an ordinary ToolScript tool row, marks
queued or running children interrupted, and consumes the checkpoint with the
repaired segment; no lexical environment, stack, timer, or continuation is
serialized.

## Security boundary

Guest identifiers are read into a private symbol table. Calls resolve against
closed tables of special forms, audited syntax transformers, pure primitives,
and the request's ToolScript tool roster. Unknown names fail closed.

The guest cannot evaluate host Lisp or access buffers, processes, files,
environment variables, user identity, time, or package state directly. It can
reach external state only through nested tools, where normal mevedel authority
and audit rules apply.

Host `macroexpand-all` is never run on guest text. The closed syntax
transformers are dispatched only when their names occur in evaluated operator
position, so quoted data and binding positions remain opaque.
