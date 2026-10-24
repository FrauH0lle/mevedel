Call one or more tools using the shapes in the tool listing or ToolSearch contracts.

Invoke listed specialists or compose calls and light result processing.
Call native tools directly. The language is closed: no ambient Emacs state or
host eval.

- `expression` is a Lisp call with keyword arguments, or multiple forms whose
  last value is returned. Use the tool name and argument shape from the tool
  listing or a ToolSearch contract.
- Tools marked standalone-only must be the entire expression. Their result
  needs a model turn before further work.
- Composed calls keep intermediate results inside the expression. Tool failures
  return `(:error "message")`; permission denial aborts execution. Completed
  changes are not rolled back.
- Read `mevedel://ptc-dialect.md` for control forms, pure operations, parallel
  calls, definitions, regular expressions and execution budgets.

ToolCall(expression="(Imenu :file_path \"src/main.el\")") returns the symbol index.
