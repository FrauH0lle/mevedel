# Yield Bash through session-owned executions

Status: accepted

## Current decision

The session owns managed executions; the initiating root or canonical agent is
its model-facing owner. Bash waits through `yield_time_ms`, then returns unread
output and an opaque handle if still running. Yield detaches the process from
request abort while retaining its owner, working directory, permission grant,
and confinement. Input, observation, and stop reuse that captured authority;
explicit denies and hooks remain authoritative.

The [execution manual](../tools/execution.md) owns tool arguments and result
handling; [managed Bash](../tools.md#managed-bash-execution) describes scheduling,
progress, settlement, and recovery. The `mevedel-execution.el` facade exposes
operations and immutable facts while `mevedel-execution-process.el` owns child
processes and spools. Callers do not inspect process records or timers.

## Rationale and consequences

One owner absorbs process groups, output bounds, cancellation, and cleanup for
Bash, batch Eval, and one-shot helpers. A shell-native background process would
escape that lifecycle, so background operators are refused. Request-owned
foreground work stops on abort; yielded work survives until completion, stop,
owner/session teardown, or Emacs exit. Resumed sessions mark stale rows lost
rather than reattaching unproven PIDs. Rewind refuses live executions and Fork
never copies them.

Output is spooled with a configurable 64 MiB default limit and bounded head/tail
observations. Remote live spools stay client-local; omitted output is staged as
session-owned evidence. Exceeding the cap terminates execution rather than
silently losing bytes. Read-only commands may overlap; other commands take a
fair exclusive lane, released on yield so long-running work does not block
admission. Managed Bash imposes no automatic timeout.

Structured execution facts keep exit and outcome separate from command output.
UI progress is transient; completion updates durable presentation and reaches
its captured owner without starting a model request. User controls can inspect
all session owners, while model controls remain owner-scoped. Deterministic
UTF-8, terminal, color, pager, and `MEVEDEL_EXECUTION=1` defaults make child output
consistent without changing user-authorized command semantics.
