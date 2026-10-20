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

An independent terminal result or monitor batch reaches its owner's mailbox
through an acknowledged sink: the execution hands over its frozen observation
and unread range and is told, exactly once, whether the record is durable. On
a warm local session the acknowledgement may arrive after several event-loop
turns ([sessions](../sessions.md#warm-local-asynchronous-mailbox-publication));
elsewhere it arrives before the call returns. Until then the record is
delivery-pending: its output is not consumed or retired, a poll observes the
range without claiming the terminal result, and a monitor keeps its in-flight
lines and starts no second batch (lines that arrive meanwhile join the next
one). Only success consumes the range, retires the record, emits the
delivered event and then offers the root its wake; a failure restores the
record unconsumed so it can be delivered again. A root wake never turns a
secured delivery into a failure, and none is offered once the session is
stopping.

When an agent has already answered but still owns yielded commands, a non-last
completion keeps the existing held-result semantics.  Its last completion
instead waits for terminal settlement to publish the RESULT, forwarded output,
and transcript.  A busy publication gate queues that settlement, not its
acknowledgement: failure acknowledges failure and removes only that completion's
tentative held entry, so the producer retains output for a duplicate-free retry.
Post-commit notification failure cannot undo the successful acknowledgement.
Agent owner finalization leaves a finished execution with an in-flight mailbox
acknowledgement registered, with its unread spool intact: success retires it and
failure leaves it pollable or retryable.  Actual session teardown discards the
record and makes a later acknowledgement inert, without resurrecting it.

A breadcrumb and the settled tool row are written to the live transcript and
reach disk with its next checkpoint: the in-flight checkpoint while a request
runs, including the final one when the last request settles, or one queued
for that transcript when none runs. Their crash window is the same as the
running turn's own streamed output. Nested ToolCall and archived completion
records still publish at once; that path resolves project transcripts
through the session's artifact store, not the existence of a fixed transcript
file (portable sessions do not keep such files, and that check silently left
completions unpublished), and updates only saved content, never an unsent
draft.

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

Retired local spools from 64 KiB through 64 MiB use gzip level 1 only when it is
smaller. A separate Python worker compresses after the 60-second terminal
polling window; the owner retires unchanged raw output only after successful
publication and active reader leases have ended. Live output and terminal
polling are unaffected, and failures keep raw evidence. Compression is local;
consumption also supports TRAMP takeover. Save As retains the compressed
representation, Fork still excludes execution history, and transfer does not
copy or rewrite it. Historical sessions are not proactively transformed.

`mevedel-execution-spool.el` owns physical selection and asynchronous,
operation-lifetime logical views behind stable artifact addresses. Read,
Grep, Glob and Full output share that seam rather than individual gzip
fallbacks. Views use logical names, bounded inflation, raw preference during
coexistence, and disposal on settlement or cancellation. No persistent
inflated cache trades the saved disk space back for read convenience.

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

- **Delivery waited for the commit inside the producer (2026-10-20).** A
  completion handed to the mailbox returned only after its synchronous
  publication, about 120 ms of target waiting on a local session while the
  user typed. The sink contract changed from "return true when secured" to an
  acknowledgement callback so the wait can happen between event-loop turns;
  the record therefore needed an explicit delivery-pending state, since
  clearing the pending flag when the sink returned would let a poll claim the
  same terminal result mid-commit. Root wake moved from the sink's return to
  its success. On a compiled in-process rerun after checkpoint preparation
  changes (one 4000-character result per sample, six samples, synthetic local
  portable session of 6000 saved turns), the producer returned in
  0.196-0.460 ms and its acknowledgement arrived 105.028-115.540 ms later,
  against 63.907-70.177 ms of blocked editor for the synchronous sink; the
  largest heartbeat gap was 6.533-7.159 ms. Earlier async ranges were
  8.2-10.2 ms gap and 75.4-120.1 ms ACK, so no blocking regression was observed
  on this workload, not an earlier completion. The original recipe explicitly
  collects garbage before each sample; none contained GC. Possible independent
  verifier load was not excluded. Not a live or graphical measurement, nor a
  hard bound on arbitrary filesystem latency, sidecar size or GC.

- **Breadcrumbs checkpoint with the transcript (2026-10).** Each yielded
  completion used to re-read, re-parse and republish the whole saved
  transcript to append one record without publishing unsent live content.
  Live profiling on a 1.3 MB session measured a 225 ms main-thread stall and
  10.7 MB of allocation per completion, felt as scroll and typing lag. The
  in-flight checkpoint already publishes live content every 30 seconds, so
  immediate publication bought only a shorter crash window than the
  surrounding turn text had. The breadcrumb now waits for that checkpoint;
  without one running, the transcript queues its own, since an idle editor
  may receive no input and so no auto-save.

- **Lossless retired-spool storage (2026-10).** Session inspection found
  385,324,439 bytes of execution logs but only 421,924 duplicate bytes;
  deduplication would save little. Two 64 MiB samples compressed with gzip-1
  to 390,317 and 3,649,871 bytes. Reducing the output cap or deleting finished
  logs would discard postmortem evidence. Lossless off-thread compression
  instead preserves it. A resolver-only `.gz` fallback was rejected because
  it breaks directory Grep and logical `.log` Glob matching; operation-scoped
  logical views preserve those interfaces without a persistent inflated cache.
  On a reflink copy of session `2026-10-19T07-07-cdd7de4f55be`, the implemented
  owner compressed 20 eligible retained logs from 1,697,026 to 165,195 bytes,
  saving 1,531,831 bytes (27.70% of all 563 spool logs, 8.58% of the entire
  tool-results tree). Every inflated result matched its untouched source hash.
  In an isolated Eask source run, launch calls had a 0.36 ms median and
  0.83 ms maximum; a 5 ms editor timer's largest gap was 5.57 ms over the
  0.785-second worker sequence. These measure local retirement/compression,
  not remote transport or graphical rendering latency.

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
