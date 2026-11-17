# Project backlog

Canonical home for concise future-work requests and unresolved defects.
Detailed plans, investigations, and review reports belong in `work://shared/`.
Read this before planning work in any listed area.

Use the inbox for ideas that have not been investigated yet. Promote an
item to a detailed entry when its scope and current status are understood.
Remove items when they are implemented, obsolete, or no longer valuable.

## Inbox

- Nested notes (a note inside another note's code) are supported by the
  geometry, but not yet checked with the new label layout: stacked expanded
  labels, tint priority, the `M-m` chooser and entry hints when point is in
  both. Try it in practice and fix what looks wrong; NoteWrite can also
  create nested whole-line notes.
- Continue the work on the "always visible prompt" (the composer stays visible while the transcript scrolls). Two prototypes, from 2026-10-26, are in `.scratch/floating-composer/` (restored from session snapshots on 2026-11-17): a child frame (`mevedel-floating-composer.el`, checked by `check.el`, `check-typing.el` and `run-check.sh`) and a split window (`check-split.el`). Not the pinned prompt header shipped on 2026-10-31.
- Claude Code adapter exits silently (code 1) when mevedel sends the next
  prompt 0–330 ms after the previous `end_turn` (five times on 2026-11-07,
  mostly a queued message handed over by the PostToolBatch hook, while the
  CLI was winding down). `--trace-exit --trace-uncaught` print nothing, so
  likely an exit code set before a natural exit. mevedel now sets
  `CLAUDE_AGENT_LOGS` (`mevedel-claude-code-agent-log-directory`); read
  `~/.mevedel/claude-agent-logs/agent.log` after the next crash (not
  rotated; prune it occasionally). First experiment: delay a queued
  follow-up prompt ~1 s after `end_turn`. Offline review (2026-11-13) found
  no exit in `agent.log` (it logs none) and no telemetry for the moments, so
  the race is unproven. Telemetry now records `acp-prompt-send` (continuation,
  queued handover, ms since `end_turn`) and `acp-process-exit` (exit code or
  signal, ms since `end_turn` and last send); after the next crash read
  those before delaying anything. Node fatal reports were skipped: they embed
  the whole environment and a plain code-1 exit is not a fatal error.
- Typing lag (live probe 2026-11-07):
  the stale-table scan and the stream-update anchor capture were fixed in the
  2026-11-13 sweep (window change after an edit 2.75 → 0.012 ms; stream update
  at 1000 turns ≈21 → 0.69 ms). Not rechecked live yet: the pinned-prompt
  header runs (about 13%) and full rerenders (about 14%). Re-profile the live
  editor before further work.
- Verify forced session takeover on the real desktop and laptop over TRAMP
  using a disposable session, including old-holder cancellation and taking
  control back ([ADR 0100](adr/0100-portable-project-session-authority.md)).
- Replace the memory claim/fence protocol with one locked program per decision
  under the existing per-root memory lock directory, using the carrier's
  `begin-files`/`commit-files` transaction (a probe showed it undoes the first
  file when a later write fails; process death was not probed). Decision
  records become exclusively created slot files (`decisions/PROPOSAL.el`,
  `.reverse.el`, `.attempt-N.el`). Journal publication keeps its claim: it
  spans provider requests and needs takeover by a successor. The 2026-11-14
  design sketch estimates about 1,010 lines in 37 functions removed for
  150–250 new, and about 25 instead of 64 helper programs per following topic
  apply (no prototype). The user approved its four decisions (2026-11-14):
  replace ADR 0117's "matching files never count as applied" with a completion
  rule (a proposal whose files are each at the before-state or exact
  after-state is finished on accept and undone on reject); remove
  `recovery-required` and "U Roll back interrupted write"; a concurrent apply
  waits up to 20 s for the lock; no migration of decision history. Before it
  lands: a matched measurement against today's 64 / 88 / 57 programs (stop if
  the gain falls well short of half), and fault-injection tests for an index
  failure after the topic write, a kill between renames, the cross-host gap,
  reject during that gap, and cleanup during apply.
- Memory control-file programs: batching per call site saved 11 → 2
  programs (63.5 → 37.4 ms) but whole operations did not get faster; more
  needs a long-lived helper process instead of one program per step (ADR 0101
  decision history, 2026-11-14).
- Code-quality leftovers from the 2026-11-14 review pass 2: Codex
  `prompt_cache_key` is placed per request at three sites; collaboration
  connection binding uses an untyped positional list and JSON string surgery;
  optional and mixed-type values added by performance fixes; note creation is
  written twice and its owner API uses two prefixes.
- Unfocused views defer rendering, so selecting a view after a long run can
  trigger one large catch-up render. Consider rendering deferred views at a
  low rate (every few seconds or when idle), bounded so background views never
  cost visible CPU; measure catch-up time and background CPU before and after.
- Browser viewer: the remaining 13 `body:has(...)` rules cost about 0.7 ms per
  streamed update on long pages.

## Execution

### Live Emacs out of memory during long orchestration

On 2026-11-12 the kernel OOM killer twice killed the user's live Emacs hosting
a long implement-spec orchestration with about 40–60 retained sub-agent
conversations, at about 57.9 GB and 56.5 GB anonymous RSS. Before the second
kill, RSS stayed near 1.3 GB for about six hours, then reached about 57 GB
within roughly two hours. Both kills occurred during long `WaitAgent` calls
that ended with `Connection closed`; the test processes were not killed.
Investigate retained conversations/transcripts, the long-wait path
(`WaitAgent` and the MCP connection), and many concurrent full test suites
(the user's suspicion). Reproduce a similar orchestration load with
`memory-report` or periodic RSS sampling to locate the growth.
The 2026-11-13 sweep found no mechanism offline. The growth was private anon
memory (shmem 20 kB) at about 30 GB/h, GC time per collection stayed flat, and
no telemetry byte stream rose with it. Not ruled out: the never-trimmed acp.el
`acp-client-stderr(...)` buffer, GTK/Pango allocations, huge retained strings
and an upstream PGTK leak. It added `memory-sample` telemetry; after the next
occurrence, compare its `:rss-kb` growth with `:buffer-chars-total`,
`:largest-buffer-chars` and `:image-cache-bytes` first (RSS growing alone
points to C-side allocations or huge strings).
During that sweep's own orchestration, sampled RSS stayed near 930 MB.

### Cargo command duration and editor stalls

Reported command, run from the SNT-webapp root:

```bash
cd backend && cargo fmt && cargo clippy --locked --offline --all-targets -- -D warnings 2>&1 | grep -v 'hard linking' | grep -E -A10 '^(error|warning)' | head -30; echo "clippy ${PIPESTATUS[0]}"; cargo test --locked --offline 2>&1 | grep -E "FAILED|test result|panicked" | head -3
```

Open after investigation on 2026-11-04. The original unrestricted run took
294.1 seconds, but printed `clippy 0` within 0.6 seconds; the long phase was
the test pipeline. Its session recorded 99 delayed editor heartbeats, with a
5.635-second maximum and little editor CPU time. Slow synchronous session-control
file programs overlapped those stalls; an expensive progress redraw is not
established. A copied backend/cache replay reproduced a five-minute executable
build without running tests; the tests afterward took 0.36 seconds. A controlled
750-ms child delay blocked the synchronous control-file API's heartbeat for about
755 ms, while the existing async carrier kept gaps near 10 ms. Natural sampled
control calls during the rebuild stayed fast, so incident attribution remains
open. Timer-only lease renewal now uses a single-flight asynchronous heartbeat,
serialized with mutation/publication and protected against stale callbacks;
own-expiry reclaim remains asynchronous too. This removes that synchronous timer
wait, not the unexplained Cargo build duration or every source of editor lag.
The original filters hide build/lock messages and do not preserve the test
command's exit code. Use raw output for further reproductions.

### Journal worker cancellation through a launcher script

When `invocation-name` names a shell launcher rather than the Emacs binary,
journal workers cannot be cancelled. Fedora's `emacs-desktop` launcher leaves
a zombie `readlink` child in its process group, and the journal stopper
(`mevedel-journal-worker.el`, near the "not a group leader" check) refuses to
signal it. Reproduced 2026-11-17 by `mevedel-cockpit-surfaces-open-quit-windows/test@8`:
it fails through the launcher and passes when invocation points at the real
Emacs executable (`/proc/self/exe`). Decide whether workers should launch the
resolved Emacs executable or whether the stopper's group-leader check should
tolerate a launcher, and keep the stopper's protection against signalling
unrelated processes. Fresh Eval launches the same `invocation-name`; its
managed process-group stop was not checked against this launcher.

## Proactive sessions

Source: the 2026-11-16 review of Anthropic's "Building always-on agents"
(https://claude.com/resources/articles/building-always-on-agents). It breaks
always-on agents into five primitives: event inbox, timers, memory, permissions
and outbox. Mevedel already covers the inbox (the agent mailbox, Monitor and
Bash wake-ups, one turn per session), memory (curated memory, journal,
`/remember`) and permissions (automatic review, absence timeout, browser
answers, `mevedel-permission-notify-function`). The entries below fill the
gaps. They apply to local sessions as well as a remote host used through the
browser viewer.

Reference point (checked 2026-11-16,
https://code.claude.com/docs/en/scheduled-tasks): Claude Code's `CronCreate` /
`CronList` / `CronDelete` and `ScheduleWakeup` fire only between turns while
the session is idle. Tasks are stored per session and restored on resume.
Recurring tasks expire after seven days, one-shot tasks whose time passed are
dropped, missed recurring runs are not caught up, and background commands and
monitors are not restarted.

### Scheduled wake-ups

No tool lets the agent wake itself later. Add one-shot follow-ups ("check CI
in 20 minutes") and recurring schedules that deliver a timer record to the
owning session's mailbox, waking an idle root like a `MONITOR` record does.

- Persist timers with the session, not in a live Emacs timer only. Use wall
  time, so a timer due during a suspend fires once on wake (as the
  [absence timeout](permissions.md#absence-timeout) does), and fires once,
  not once per missed interval.
- A due timer waits for the current turn and coalesces with other mail.
  Bound the timer count per session, expire recurring timers, and list and
  cancel them from the background-work panel and a tool.
- Decide how timers interact with the paused-on-resume Goal rule
  ([goals](goals.md)) before implementation: a restored timer must not
  dispatch Goal work the rule withholds. Read that rule's history first.
- Keep the sleep inhibitor rule for waiting Monitors: a pending timer alone
  does not keep the machine awake.

### Wake sources across restarts

Monitors live only in the current Emacs runtime; resume and fork never restart
them ([Monitor](tools.md#monitor)). After a restart the agent is not told that
an event source it relied on is gone, so events are lost silently. At minimum,
tell the agent on the next turn which monitors were lost. Consider an opt-in
re-arm for monitors the agent marks as restartable, under the same restored-Goal
constraint as timers. A polling Monitor (for example `gh run watch`) then also
covers outside events such as CI without opening an inbound port.

### Notifications beyond permission prompts

`mevedel-permission-notify-function` covers prompts only. Unattended work that
ends (a timer run, a Goal turn, a Monitor-triggered turn) has no way to reach
a user who is not watching. Consider a general notification hook or model tool
with a quiet-by-default policy: notify only when a turn the user did not start
has something for them, never repeat the same notice, and leave the volume to
configuration. The relay never sees the room key, so notification text must
not carry conversation content in clear text; send "session X needs you" with
a link, or encrypt the payload for the device.

Not planned: inbound webhooks (a polling Monitor serves them) and the article's
deferred tool calls that end a run and resume when the user answers. A pending
card blocks only its own session, and browser answers plus the absence timeout
already cover being away.

## View

### Cursor jumps after switching back to a busy view

Background redraws no longer move point out of the composer through the native
partial-scroll reset that live recordings attributed on 2026-11-10 (ADR 0126).
One captured jump remains unexplained: after the user switched back to a busy
hidden view, the catch-up projection inserted about 1,900 characters above the
composer and point landed at the start of the bottom status row, with partial
vscroll zero before and after. One-second sampling does not identify the writer.
Once point sits in a status row, later redraws keep it on that row as a visited
row, so one misplacement drifts with status updates, including while hidden.
Capture the reveal-time projection with the bounded call recorder before
changing preservation policy.

The user also reports permission-prompt arrival moving the caret one line below
the prompt. Arrival-time RET targeting was removed when explicit `C-c C-j`
visiting was introduced; no surviving automatic arrival jump is confirmed.
Local real-permission fixtures preserve empty/multiline composer offsets after
graphical redisplay and event-loop return, but do not assert screen-row stability
or establish live equivalence. Capture the complete interaction materializer and
subsequent redisplay, distinguishing arrival from explicit visiting. Still open.

## Hook extensions

### Task transition events

Consider a task transition event when notifications or tracker synchronization
need to observe every completion. A hook matching `TaskWrite` misses automatic
completion by `mevedel-tool-task-finalize-owner` in `mevedel-tool-task.el`.
Observe actual state transitions across both paths rather than tool calls alone.
Defer until a concrete integration needs that coverage.

### Deferred extensions

- HTTP, prompt, MCP-tool, and agent handlers: revisit when a named workflow
  cannot be served adequately by command/Elisp handlers. Define permission,
  cancellation, and recursive-hook behavior before adding a handler type.
- Handler-level predicates: keep conditions inside handlers unless repeated
  workflows or measured launch cost justify declarative filtering. Prefer an
  event-specific matcher where sufficient, such as skill-name matching for
  `UserPromptExpansion`, rather than a general predicate language.
- Configuration, cwd/files, tool-batch, and shell-environment events: add
  individually when a caller needs a boundary existing events cannot express;
  define the event's scope and control effects before implementation.

## Flaky tests

- Resource-installation search can exceed its 20 s production deadline under
  heavy host load (2026-10-31). The search covers two fixture files (median
  0.16 s isolated, 0.63 s maximum contended), but tracing caught synchronized
  5-7 s event-loop stalls; the stall source is unidentified. The test watchdog
  now sits above the production deadline.
- `mevedel-agent-control-mail-batch/producer/test` stalled twice past 240 s in
  parallel affected-tier runs (2026-11-16), but passed alone, in an isolated
  24-test prefix replay on changed and pristine trees, and in 120 instrumented
  stress runs. Confirmed mechanism for the missing deadline: installed TRAMP
  2.8.2.31.1 discards an enclosing `with-timeout` timer during a direct TRAMP
  file operation (here `mevedel-execution-process--append-spool`), while
  `mevedel-transport-with-exclusive-connection` preserves it. The test wait
  helper now also checks elapsed time. Why the producer predicate stayed false
  is unexplained; production `with-timeout` uses around direct TRAMP
  operations may share the lost-deadline mechanism.
- `mevedel-execution-start-bash/remote/test@10` timed out once in a loaded
  affected-tier run (2026-11-16) with `group-id nil` while waiting for the
  fixture's zombie, before StopExecution. It passed in focused runs and in
  three repeated runs each on the changed and pristine trees (113/113).
- `mevedel-present-wayland/thread/test` skips on a suspected stock-pgtk
  deadlock (thread waits in `gtk_main_iteration`, main thread in
  `thread-join`). 2026-11-14 full run under load: 1 skip after the old 20 s
  fixture timeout (20.08 s); isolated runs: 0 skips in 7, 3.79 to 3.87 s each.
  The timeout is now 10 s. Root cause unconfirmed; the skip masks a real hang,
  so capture a backtrace of the hung Emacs if it recurs.
