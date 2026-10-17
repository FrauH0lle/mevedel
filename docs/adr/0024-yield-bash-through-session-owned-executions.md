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

The initiating Bash row owns output and execution status. Successful empty-input
polls do not add visible rows; actual input, stops, and failed control operations
remain visible, and all tool results remain model-visible and inspectable in
execution history. An execution that actually yielded gets one durable
completion breadcrumb at its original row, independent of whether completion
was collected by polling or delivered through a mailbox. It is the one visible
completion line, worded like a notice (`● Background command "Run tests"
completed (exit code 0)  [Show result]`, `● Monitor "ticker" stopped · 2
events`); the transcript's own delivery of the same completion renders nothing.
A transcript receiving another owner's completion, such as a child agent's
forwarded record, has no local breadcrumb and shows a one-line notice built
from the mailbox record's structured opening-tag attributes (never payload
text). Both use completed, failed, stopped, and timed out. A stop is
neutral in the view while the outcome fact stays `failure`.
The breadcrumb links to the original row or retained evidence and does not
duplicate output. Inside an activity run it folds into that run's group as a
counted completion rather than a line of its own: standalone breadcrumbs
stacked up beside the groups they belonged to, several per turn, while the
original rows already showed each outcome. The record stays durable, because
it marks when the model learned of a completion it may react to. While the
receiving transcript's turn runs, the record goes where that turn's
response currently ends, so the turn's later output follows it; an idle
transcript receives it at its end. Appending at the end during a running turn
put every completion behind the turn's final answer: one session stacked
twelve completions, emitted over eighty minutes, just before the next prompt,
where no activity group could count them. Running
background commands and monitors are listed transiently in the view's
background-work panel, together with active agents; finished items leave it.
Activating a row expands one local bounded inspector; transcript navigation,
full output, stopping, and opening the background-work cockpit are explicit
actions.
The inspector projects existing execution snapshots and passive events without
claiming output, and retains only the selected item's final presentation until
collapsed or replaced. It adds no process registry or execution timer.
Its identity is scoped to session, owner, and execution, not
command text. Execution IDs carry a fresh per-state prefix so resumed sessions
cannot reuse the earlier state's counter in durable records or output paths.
Foreground completion never produces a breadcrumb. Execution
presentation records in `mevedel-execution-transcript.el` keep these facts
reconstructable across redraw, reload, and compaction; they do not create a
second process registry or alter delivery to the model or agent.

Completion publication resolves project transcripts through the session's
artifact store, not the existence of a fixed transcript file. Portable sessions
do not keep such files; using that check silently left completions unpublished.
Publication updates only saved content, never an unsent draft. A mid-turn
insertion keeps the live buffer modified so its response-position ordering and
any flushed stream text are included in the next checkpoint.

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
its captured owner's mailbox. A root-owned yielded command that ends on its own
also wakes an idle root model: the harness starts one
"Background event" turn whose fixed prompt says it is not a user message (the
view hides the heading and prompt and starts with the notice lines), and
the result reaches the model through the ordinary mailbox delivery. A stop by
the user or the model does not wake it, a result the model already read does
not wake it again, and agent-owned completions stay with their invocation.
The wake mark is runtime state and is not persisted. The same scheduler admits
agent-originated MAIL and ordinary RESULT delivery to `/root`, under
[ADR 0035](0035-separate-agent-messages-from-follow-up-work.md); a child's
forwarded EXECUTION record does not itself gain wake eligibility. User controls
can inspect all session owners, while model controls remain owner-scoped. Deterministic
UTF-8, terminal, color, pager, and `MEVEDEL_EXECUTION=1` defaults make child output
consistent without changing user-authorized command semantics.

Keeping output on the original row, and marking accepted input or observation
controls successful **in the view** even when they collect a failed exit, avoids
mistaking a failed process for a failed control operation. Model-visible results
and nested ToolCall error values keep their original status and structured
process facts. A single yield-based breadcrumb
replaces duplicate completion cards and terminal poll summaries without relying
on elapsed time or on whether a view happened to be open. Distinct executions
of the same command remain independently navigable.

## Decision history

- **One completion line (2026-10).** The breadcrumb (`↳ Completed: …  [Show
  result]`) and the notice of the transcript's own delivery
  (`● Background command "…" completed`) sat next to each other for every
  in-turn completion; the user read them as duplication. The breadcrumb is
  recorded when the work ends, before delivery, so it stays as the one line in
  notice wording, and the own delivery no longer renders. Forwarded child
  completions keep their notice because their transcript has no breadcrumb.
- **One background-work panel with agents (2026-10).** Commands and monitors
  had their own status block, with an `All…` link to an executions-only
  cockpit, while agents had a separate block with a `[-]` toggle and handle
  rows; the footer repeated agent counts. A live test with an agent, a command
  and a monitor showed three presentations of one concept. The user chose one
  panel with one row grammar, counts in its rule only, overflow expanded in
  place, and a `[Manage]` row control instead of `All…` in the rule: `All…`
  opened a list that no longer matched the panel and crowded the header. The
  executions cockpit became the background-work cockpit listing agents too,
  and agents gained a user Interrupt.
- **Subject-first notices from structured attributes (2026-10).** Deliveries
  were shown as `✉ Monitor: … [1 line collapsed]` cards under a visible
  "Background event" heading, and the view recovered facts by parsing payload
  text, which quoted output could spoof; a user Stop also showed as a red ×.
  The producer now emits one `MONITOR` record per event and `MONITOR_TERMINAL`
  and `EXECUTION` openers with structural attributes, and the view renders
  short notices (`● Monitor event: …`, `● Background command "…" completed
  (exit code 0)`) without the wake heading, following Claude Code's background
  notices. Stops render neutral.
- **Share the wake path with agent reports (2026-10).** Late root-directed
  agent messages and results were stranded until the user's next turn even
  though background Bash and Monitor already woke the idle root. They now use
  that same scheduler, holds, and current permissions rather than a parallel
  agent scheduler. Successful root turns reoffer eligible late unread mail;
  failure or abort does not automatically retry it. Wake marks remain
  runtime-only, and restart/recovery remains non-waking.
- **Local inspection (2026-10).** Status rows previously jumped to their
  originating transcript entry, while their margins inherited an action opening
  the execution cockpit. The user found a named row unexpectedly opening a
  management screen and chose expansion in place over navigation. Commands and
  monitors now share one local-inspection interaction; the original transcript
  entry remains available through an explicitly named action. This preserves
  conversational context and keeps an open inspector visible through settlement.
- **Completion wake (2026-10).** Originally a yielded completion started no
  model request: the root saw it only at its next turn, so a model waiting for
  a build or test run had to poll with `WriteStdin` or ask the user to nudge
  it. The user wanted to start background work, end the turn, and be woken by
  the result instead of polling, following Claude Code's background-Bash
  contract, where a finished background command notifies the model. The wake
  is always on, as there; Bash gained no parameter. It is admitted through the
  same idle-work order as queued follow-ups and Goal continuation (follow-ups,
  then background events, then Goal) and waits behind a composer draft,
  pending interactions, and the follow-up drain's holds, so it never
  interrupts the user. The same path carries [Monitor](../tools.md#monitor)
  events.
