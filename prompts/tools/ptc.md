Run one small Lisp orchestration script that calls other tools.

Only the script's final value returns to you. Intermediate results stay inside
the script. Lambdas are internal callables and cannot be passed to tools or
returned as the final value. If the user asks for one ToolScript call, call
ToolScript directly. Do not inspect mevedel's ToolScript implementation first.

This is a closed dialect, not Emacs Lisp. This quick reference is sufficient
to write scripts. If you need more examples or exact semantics, first use Read
on the installed manual at `{{PTC_DIALECT_MANUAL_PATH}}`.

### When to use `ToolScript`

- Later tool calls depend on earlier results and the intermediate data
  does not need to enter the conversation
- Many independent calls can run as one batch with `parallel` or
  `parallel-map`
- Light reshaping of tool output (filtering lines, collecting names)
  where only the final value matters

### When NOT to use `ToolScript`

- A single ordinary tool call: call the tool directly
- Bulk text processing: search with `Grep` instead of reading many
  files and scanning their lines in the interpreter
- Anything needing `eval`, macros, buffers, processes, filesystem
  access, or ambient Emacs state; the dialect has none of these

### How to use `ToolScript`

#### Quick reference

Special forms:

    quote  if  progn  cond  and  or  let  let*  setq  while
    lambda  funcall  apply  mapcar  parallel  parallel-map

Accepted macro-shaped forms:

    (when CONDITION BODY...)
    (unless CONDITION BODY...)
    (push VALUE VARIABLE)          ; VARIABLE is a plain bound variable
    (dolist (VAR LIST) BODY...)    ; no RESULT form
    (dotimes (VAR COUNT) BODY...)  ; no RESULT form

There is no `eval`, macro definition, backquote, `cl-loop`, generalized place,
buffer/process/filesystem access, or ambient Emacs state.

Every operator is checked before anything runs: a script naming a function
outside the closed tables is rejected up front with the full list of unknown
names and nearest-match suggestions. `seq-*` and `cl-*` helpers do not exist;
use the listed primitives (`take` replaces `seq-take`, `mapcar` replaces
`cl-mapcar`). Path strings have dedicated pure primitives
(`file-name-nondirectory`, `file-name-directory`, `file-name-concat`, ...) —
do not reimplement them with `split-string`. `sort` copies its list and always
orders ascending with a fixed comparator.

`let` evaluates every initializer in the outer environment. Use `let*` when a
later initializer needs a variable bound earlier in the same form. `setq` only
changes an existing binding.

Strings use normal Lisp escapes: `"\n"` is a newline. Guest regexp primitives
such as `string-match-p` use Emacs regexp syntax. A literal opening parenthesis
needs no escape there, so a line beginning with `(defcustom` is matched by
`"^(defcustom"`; a capture group uses `"\\(...\\)"` in script source. Tool
arguments follow that tool's contract instead: Grep uses ripgrep syntax, where
`"^[(]defcustom"` safely matches the same literal text.
The guest regexp subset allows `*`, `+`, `?`, and `\{n,m\}` on a single atom:
one literal, one escaped character, `.`, or one bracket class, so `"^[0-9]+$"`
works. Quantified groups, a quantifier stacked on another quantifier,
alternation, backreferences, and group extensions are rejected. Ordinary
capture groups and `regexp-quote` remain available. Polynomial backtracking is
charged conservatively from input size and quantified-atom count before Emacs's
matcher runs. Literal regexps are validated up front, before any nested tool
runs.

The generated **Pure data operations** section below gives the exact signature
of every callable data primitive. Brackets mean optional and `name...` means
zero or more arguments.

#### Calling tools

Call a listed tool by name with its generated keyword arguments:

    (Grep :pattern "^(defcustom" :path "." :output_mode "count")

Successful calls normally return strings. A failed nested call returns
`(:error "message")`, so scripts can branch on `(plist-get result :error)`.
Permission denial aborts the whole script and cannot be caught.

A nested call returns exactly the string that tool returns in conversation,
including any formatting the tool documents. `Read` output carries `cat -n`
style line-number prefixes (`1\tfirst line`) and may end with a truncation
notice; `Grep` count mode returns absolute `path:count` lines; `Glob` returns
newline-separated absolute paths. Parse accordingly.

Use parallel forms only for independent calls. Results preserve input order.
Each `parallel` entry is one direct tool call. `parallel-map` takes a
one-argument lambda whose only body form is one direct tool call. Argument
expressions may reshape data but may not call another tool.

#### Keep scripts small

Use ToolScript for orchestration and light shaping, not bulk text processing.
Search with Grep instead of reading hundreds of files and scanning their lines
in the interpreter. Scope broad globs before looping. If a budget is exceeded,
narrow the work instead of retrying the same script.

### Examples of good usage

<example>
- Dependent calls; only the shaped final value returns:
(let* ((counts (Grep :pattern "^(defcustom"
                     :path "."
                     :output_mode "count"))
       (lines (split-string counts "\n" t))
       (out nil))
  (dolist (line lines)
    (when (string-search ":" line)
      (push line out)))
  (reverse out))
</example>

<example>
- Independent reads as one ordered batch:
(parallel
  (Read :file_path file-a)
  (Read :file_path file-b))
</example>

<example>
- One direct tool call per list element:
(parallel-map
  (lambda (file) (Read :file_path file))
  files)
</example>

### Examples of bad usage

<example>
(Grep :pattern "TODO" :path "src")
as the entire script
<reasoning>
A single ordinary tool call. Call Grep directly instead of wrapping it
in ToolScript.
</reasoning>
</example>

<example>
(seq-filter (lambda (l) (string-match-p "err" l)) lines)
<reasoning>
seq-* and cl-* helpers do not exist in the closed dialect; the script
is rejected up front. Use the listed primitives, e.g. dolist with push.
</reasoning>
</example>

<example>
(parallel-map (lambda (f) (length (split-string (Read :file_path f) "\n")))
              (split-string (Glob :pattern "**/*.el") "\n" t))
<reasoning>
Bulk text processing over hundreds of files inside the interpreter.
The parallel-map lambda body must also be one direct tool call. Use
Grep with output_mode "count" to let the search tool do the work.
</reasoning>
</example>
