# Auto confinement falls back only before execution

Sandbox mode `auto` treats execution confinement as best-effort: a harmless
capability probe marks an unavailable backend and permits direct execution with
persistent disclosure, while `required` refuses execution.  Once the requested
sandbox process has started, Mevedel never replays that command directly,
because it cannot prove that the child produced no partial effects; the single
sandboxed result is returned instead.  This preserves optional cross-platform
operation without turning a sandbox failure into duplicate or unexpectedly
broader execution.  The model may respond to the returned failure with a new
tool invocation requesting additional permission; that invocation is checked
and executed independently rather than treated as an executor retry.
