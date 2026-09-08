Call one or more tools using the calling expressions supplied by ToolSearch.

### When to use `ToolCall`

- Invoke a specialist found through ToolSearch.
- Compose dependent calls, parallel batches, or light result processing.

### When NOT to use `ToolCall`

- Native core tools can be called directly.
- The expression language is closed: it has no ambient Emacs state or host eval.

### How to use `ToolCall`

- `expression` is a Lisp call with keyword arguments, or multiple forms whose
  last value is returned. Use the tool name and contract supplied by ToolSearch.
- Tools marked standalone-only must be the entire expression. Their result
  needs a model turn before further work.
- Composed calls keep intermediate results inside the expression. Tool failures
  return `(:error "message")`; permission denial aborts execution. Completed
  changes are not rolled back.
- Read `mevedel://ptc-dialect.md` for control forms, pure operations, parallel
  calls, definitions, regular expressions and execution budgets.

### Examples of good usage

<example>
ToolCall(expression="(Imenu :file_path \"src/main.el\")")
Returns the file's symbol index.
</example>

### Examples of bad usage

<example>
ToolCall(expression="(progn (Skill :name \"frontend\") (Bash :command \"build\"))")
Skill is standalone-only: invoke it separately and consume its instructions
before choosing further actions.
</example>
