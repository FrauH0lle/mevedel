# ToolCall dialect manual

ToolCall lets a model make a data-dependent sequence of ordinary mevedel
tool calls inside one model turn. The script is interpreted by mevedel; it is
never evaluated as Emacs Lisp. Every nested tool call still goes
through normal validation, hooks, permissions, snapshots, telemetry, and
rendering.

Pass one call or a composed program in ToolCall's `expression` argument.
Retrieve specialist contracts with ToolSearch. Native core tools can also be
called directly. Use Grep or another purpose-built tool for bulk processing.

Tools marked standalone-only must be the entire expression, without nested
calls in their arguments or surrounding forms. This includes Ask, Skill,
WaitAgent, UpdateGoal, and wrapped tools not explicitly admitted for
composition. Direct-call arguments may be literals, quoted data or direct pure
primitive applications. Other argument expressions use the composed path.
Read-only Elisp introspection tools support composition.

A direct call returns the underlying tool's result and supported media, and
renders as that tool in the view. Composed expressions return the last value
and retain their expandable script/child-call display. Hook context and repair
feedback are delivered even when an intermediate result is discarded.

For object arguments use keyword plists, arrays use vectors or lists, JSON
false uses `:json-false` (or nil for a boolean argument), and JSON null uses
`:null`. Declared object keys are converted to the native adapter's keys.
Wrapped tools use the qualified `category/name` shown by ToolSearch; this
prevents collisions across servers. The installed MCP text adapter exposes
text content only. ToolCall cannot recover media or error metadata discarded
upstream by that adapter.

## Values and evaluation

The dialect supports numbers, strings, symbols, keywords, lists, lexical
bindings, and lambdas. `nil` is false; every other value is true. A script may
contain multiple top-level forms, which are evaluated as an implicit `progn`.
Lambdas are internal callables: neither a tool argument nor the script's final
value may contain one.

The evaluator resolves a call's operator before evaluating its arguments. An
unknown function therefore fails without running tool calls hidden in its
arguments.

Static operators additionally preflight before execution, including literal
indirect references to standalone-only tools. Unknown names in never-taken
branches are rejected too. Quoted data, binding names, lambda parameter lists
and unexpanded user-macro arguments are skipped. Literal regexps are checked
in the same pass. Runtime-generated calls are checked against the same closed
roster before dispatch; they cannot always be rejected before earlier tools
run. Earlier authorized effects remain recorded and are not rolled back.

`let` evaluates all initializer expressions in the surrounding environment:

```elisp
(let ((paths (list "a.el" "b.el"))
      (count (length paths))) ; wrong: paths is not bound here
  count)
```

Use `let*` when a later initializer depends on an earlier binding:

```elisp
(let* ((paths (list "a.el" "b.el"))
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

## Pure data operations

The closed primitive table supports these signatures. `&optional` marks the
remaining optional arguments, and `&rest` marks repeated arguments. Beyond
these operations, calls resolve only to discovered tools or the program's own
definitions. Path helpers are syntactic and do not access the filesystem.
`sort` copies its list and uses a fixed ascending comparator.

```text
(car list)
(cdr list)
(caar x)
(cadr x)
(cdar x)
(cddr x)
(cons car cdr)
(list &rest objects)
(append &rest sequences)
(length sequence)
(nth n list)
(nthcdr n list)
(last list &optional n)
(take n list)
(reverse seq)
(sort list)
(member elt list)
(memq elt list)
(assoc key alist &optional testfn)
(assq key alist)
(plist-get plist prop &optional predicate)
(plist-member plist prop &optional predicate)
(concat &rest sequences)
(format string &rest objects)
(substring string &optional from to)
(split-string string &optional separators omit-nulls trim)
(string-join strings &optional separator)
(file-name-nondirectory filename)
(file-name-directory filename)
(file-name-concat directory &rest components)
(file-name-extension filename &optional period)
(file-name-sans-extension filename)
(file-name-base &optional filename)
(string-trim string &optional trim-left trim-right)
(string-prefix-p prefix string &optional ignore-case)
(string-suffix-p suffix string &optional ignore-case)
(string-match-p regexp string &optional start)
(string-search needle haystack &optional start-pos)
(regexp-quote string)
(upcase obj)
(downcase obj)
(capitalize obj)
(number-to-string number)
(string-to-number string &optional base)
(+ &rest numbers-or-markers)
(- &optional number-or-marker &rest more-numbers-or-markers)
(* &rest numbers-or-markers)
(/ number &rest divisors)
(% x y)
(mod x y)
(abs arg)
(max number-or-marker &rest numbers-or-markers)
(min number-or-marker &rest numbers-or-markers)
(float arg)
(truncate arg &optional divisor)
(round arg &optional divisor)
(floor arg &optional divisor)
(ceiling arg &optional divisor)
(= number-or-marker &rest numbers-or-markers)
(/= num1 num2)
(< number-or-marker &rest numbers-or-markers)
(> number-or-marker &rest numbers-or-markers)
(<= number-or-marker &rest numbers-or-markers)
(>= number-or-marker &rest numbers-or-markers)
(eq obj1 obj2)
(eql obj1 obj2)
(equal o1 o2)
(null object)
(not object)
(atom object)
(consp object)
(listp object)
(stringp object)
(numberp object)
(integerp object)
(floatp object)
(symbolp object)
(sequencep object)
(identity argument)
(gensym &optional prefix)
```

## Definitions

A script may open with top-level `defun` and `defmacro` forms:

```elisp
(defun read-or-nil (path)
  (let ((r (Read :file_path path)))
    (if (and (listp r) (plist-get r :error)) nil r)))

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

ToolSearch returns current tool contracts and their exact keyword arguments:

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
  (if (and (listp result) (plist-get result :error))
      (list path :unreadable)
    (list path (length result))))
```

Permission denial is different: it aborts the whole ToolCall call. Completed
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

ToolCall bounds script bytes, syntax nodes, nesting depth, evaluation steps,
wall time, nested-call count, recursion depth, transformed syntax size, regexp
work, numeric size, individual values, and cumulative retained values. Errors name the
exceeded budget. Final rendering counts repeated references at their serialized
size, so a small shared object cannot expand into an oversized result while
printing.

These limits are intentionally generous for orchestration and restrictive for
bulk computation. Prefer:

```elisp
(Grep :pattern "^[(]defcustom" :path "." :output_mode "count")
```

over reading every file and scanning every line in the interpreter. Narrow
wide searches before mapping over their results.

The interpreter yields between short computation slices, so Emacs remains
interactive. Scripts in flight are runtime state: if Emacs exits or the session
is recovered, the ToolCall call settles as interrupted and does not resume.
For root-session scripts, only the envelope call and its bounded ordered child
audit are checkpointed, and the checkpoint is written durably twice: once
before the first nested call and once at settlement. Retained-agent scripts
skip this checkpoint because their own interrupted-turn handling settles them.
Between the root-session writes, child audit progress is journaled in memory
only; an unrelated autosave captures it opportunistically. The performance
rationale is in [ADR 0111](adr/0111-run-programmatic-tool-calls-in-a-closed-machine.md#decision-history). A crash mid-script therefore recovers the child audit as of the last
autosave, not the last child. Recovery turns the surviving checkpoint into an
ordinary ToolCall tool row, marks queued or running children interrupted,
and consumes the checkpoint with the repaired segment; no lexical environment,
stack, timer, or continuation is serialized.

## Security boundary

Guest identifiers are read into a private symbol table. Calls resolve against
closed tables of special forms, audited syntax transformers, pure primitives,
the script's own top-level definitions, and the request's ToolCall tool
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
