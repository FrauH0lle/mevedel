# Project backlog

Canonical home for concise future-work requests and unresolved defects.
Detailed plans, investigations, and review reports belong in `work://shared/`.
Read this before planning work in any listed area.

Use the inbox for ideas that have not been investigated yet. Promote an
item to a detailed entry when its scope and current status are understood.
Remove items when they are implemented, obsolete, or no longer valuable.

## Inbox

- Agent harness defects from the 2026-11-14 sweep (handoff with symptoms and
  code locations: `work://shared/agent-harness-defects-2026-11-14.md`):
  retained agents cannot be messaged after their working directory is removed;
  a provider "Malformed JSON in response" ends a worker's turn and stops its
  executions; `InterruptAgent` leaves the agent's executions running; agent
  creation failed once with "Agent conversation could not be persisted".
- NoteWrite anchors only whole lines (`start_line`/`end_line`), while the user
  can mark a single word or expression. If AI notes on smaller spans turn out
  to be needed in practice, add optional columns or an in-line anchor text
  to NoteWrite. Whole lines were kept for now because they re-anchor more
  robustly after edits.
- Nested notes (a note inside another note's code) are supported by the
  geometry, but not yet checked with the new label layout: stacked expanded
  labels, tint priority, the `M-m` chooser and entry hints when point is in
  both. Try it in practice and fix what looks wrong; NoteWrite can also
  create nested whole-line notes.
- Continue the work on the "always visible prompt" (the composer stays visible while the transcript scrolls). Two prototypes, from 2026-10-26, are in `.scratch/floating-composer/`: a child frame (`mevedel-floating-composer.el`) and a split window (`check-split.el`). Not the pinned prompt header shipped on 2026-10-31.
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
- Prompt cache follow-ups (2026-11-07). Claude Code is verified live: with
  gptel-style reminder delivery (ADR 0123), each turn reads the whole
  conversation from the 1-hour cache, a plain Emacs restart keeps the full
  cache, and a restart after compaction keeps the system prompt and tools.
  Codex requests now send `prompt_cache_key` (branch
  `sweep/2026-11-13/codex-cache-key`), as the Codex CLI does
  (`codex-rs/core/src/client.rs` `session_derived_cache_key`): the root
  conversation sends its session id; each spawned sub-agent sends the
  session id plus its own agent id. Still open: verify live that Codex
  cache hits improve with it. gptel's Anthropic
  backend sends no cache markers
  (`gptel-cache` nil).
- Directives and references for the model (decided 2026-11-05; context in
  `work://shared/directive-reference-model-tools.md`). The workspace
  instruction store is done (ADR 0130; existing workspaces migrate with
  `scripts/migrate-sessions.el`). Next, as part of a small directive overhaul:
  read and write tools for the model (reading through a read-only resource
  family for Read/Grep/Glob, ADR 0104), and a workspace-level history so
  directives and references get undo/restore independent of message Rewind
  (which stays per session). Keep Buddy separate for now. The store's TRAMP
  round-trip cost is not measured on real SSH.
- Typing lag (live probe 2026-11-07, `work://shared/typing-lag-2026-11-07.md`):
  the stale-table scan and the stream-update anchor capture were fixed in the
  2026-11-13 sweep (window change after an edit 2.75 → 0.012 ms; stream update
  at 1000 turns ≈21 → 0.69 ms). Not rechecked live yet: the pinned-prompt
  header runs (about 13%) and full rerenders (about 14%). Re-profile the live
  editor before further work.
- Session lease follow-ups (all sessions use the portable lease since
  2026-11-14; PID lock deleted; forced takeover added, ADR 0100). Open:
  turn settlement still uses the synchronous writer (about 43-49 ms per turn on
  the lease; an async settlement through the publication queue looks
  feasible); the forced takeover has only TRAMP-mocked remote tests, so try it
  once for real between desktop and laptop; resume now parses every closed
  segment to repair stale running rows (cost on long histories not measured);
  the sync lease-update path returns a bare nil where the async path names the
  decline cause; a foreign `claiming` record that never finishes has no
  recovery test; an unmigrated, cleanly closed pre-lease session (no `.lock`,
  no `.lease`) counts as available, so age cleanup can delete it before
  `scripts/migrate-sessions.el` runs.
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
  streamed update on long pages; agent sheets don't pin a prompt yet.

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

### Harness-enforced Bash timeouts

Managed Bash commands run until they exit or are stopped. `145c66a` removed
`mevedel-bash-timeout` (120 s default) and the model-facing `timeout_seconds`,
so only a native `timeout` the model writes itself bounds a command. A
forgotten command keeps running and holds the
[system sleep inhibitor](sessions.md#system-sleep). When Bash completion wake-ups
and the [Monitor tool](tools.md#monitor) were added (2026-10), the user decided
against restoring a Bash deadline for now; only Monitor expires (`timeout_ms`).
Revisit if forgotten commands become a problem in practice. Reference point:
Claude Code caps background commands at 10 minutes by default and 2 hours at
most. Expiry would reuse the existing TERM/KILL process-group teardown and
report a timeout outcome.

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

### Intermittent session publication refusals

On 2026-11-06 an execution completion and two autosaves were refused with
`Portable session publication admission failed`. Later committed bytes include
the warning report and subsequent conversation; persistent data loss is not
established. A real isolated regression shows the old warning can also mean
an ancillary fixed-file write failed after successful lease reservation, with
rollback leaving healthy ownership. Diagnostics now distinguish that operation,
status and code from an unexplained phase refusal, without changing admission,
rollback or retry behavior. This does not identify the live incident's cause.
Since 2026-11-07 warm admission declines and reserve-phase lease-update
declines name their observed cause (recovery marker, newer generation, another
client's lease, ...).
Capture the actual rejected phase and target observation; do not infer another
owner or bypass authority from the generic warning. Shared causation with cursor
jumps remains unproved.

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

## Eval integration

### Sandbox Eval extensions

[ADR 0133](adr/0133-add-persistent-sandbox-eval.md) leaves two extensions
for when a concrete need appears: an opt-in init profile for the sandbox, and
sandbox Eval for remote execution targets (locally against TRAMP paths, or on
the remote host) if Elisp development over TRAMP needs it. Remote-host tests
need an Emacs-equipped target such as a container; current remote tests use
the `/mevedelmock:` method.

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

## Whiteboard

### Excalidraw parity left out of the element model

[ADR 0121](adr/0121-store-whiteboards-as-excalidraw-elements.md) stores and
draws Excalidraw elements but cherry-picks interaction. Not yet implemented:
segment dragging for elbow arrows, object and grid snapping, a frame tool and
moving children with their frame, sticky-note font auto-fit with its lifted
corner and date footer, the constant-width laser-pointer stroke, and embedding
the scene in PNG/SVG downloads. Add each when a board needs it; Excalidraw's
behavior is compiled in excali-mode's `docs/excalidraw-spec.md`
(https://github.com/yibie/excali-mode).

## Review

### Automatic turn advisor

Consider an opt-in second-model review of successful root turns, with bounded
feedback delivered to the next request. Reuse the Stop hook, reviewer runtime,
and reminder delivery. Keep findings distinct from Buddy's advisory notes, and
specify evidence scope, deduplication, cooldown, cancellation, and cost before
implementation. Pursue when recurring review failures justify the extra model
call; keep it advisory, without restarting completed turns. Implement this
bounded feature before considering generic prompt or agent hook handlers.
There is no automatic root-turn reviewer/instruction-injection path today.
The original request came from the 2026-08-20 watchdog/advisor discussion.

### Buddy comment trigger

Follow-up to `/buddy`: a comment containing `BUDDY!` in a buffer with
`mevedel-buddy-mode` runs guidance at once on its enclosing defun, using the
comment text as the question. No existing watcher fits as is. Buddy's change
recording sees the edit, but the trigger needs its own rules before it can use
the idle timer:

- Fire when the edit settles, not on the `!` keystroke, so the question after
  the marker is complete.
- Skip `mevedel-buddy-min-interval` and preempt an automatic review, as
  `mevedel-buddy-guide` does.
- Fire once per comment. A comment left in the buffer must not fire again on
  later edits elsewhere. Key it in memory by marker or text hash.
- Send the enclosing defun (`beginning-of-defun`), or the whole buffer when
  there is none.

Detection checks only the changed lines in `mevedel-buddy--after-change`; it
never polls or rescans buffers. Buddy does not remove the comment after the
guidance runs: the user deletes it.

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

## Browser session history actions

Owner links can create a Conversation Fork from a settled assistant response
(`fork` frame, docs/collaboration.md). Rewind, edit and Worktree Fork remain
Emacs-only. Each needs an owner-only typed frame that resolves the same stable
`forkPoint` the projection already publishes, and runs the existing Emacs
transaction. A browser frame runs under `inhibit-interaction`, so any native
minibuffer or overlay confirmation would refuse it. Each action therefore
needs a browser-side confirmation of its own.

- **Rewind:** `mevedel-session-rewind` confirms in Emacs. It names both sides
  of the boundary, capture gaps, the files to restore, external changes it
  would overwrite, and the live state it would clear. A browser rewind needs
  a host-built impact card (a `ui-request` like plan approval, bounded, with
  an "open in Emacs" fallback when the impact does not fit) and an owner
  confirmation. Every room in the session must then republish its records
  after the truncation. Redo follows the same shape.
- **Edit:** resubmitting an edited earlier prompt is Rewind before that
  prompt plus a new prompt, so it should follow Rewind and reuse its card.
- **Worktree Fork:** needs Git preflight, branch naming (the Worktree plan
  acceptance already validates a browser branch name) and the retained-artifact
  error path.
- **Fork with a first prompt:** the browser fork publishes an empty child and
  the owner writes in its room. Sending the prompt with the fork, as the `f`
  composer flow does, could reuse the `new-session` first-prompt path.
- **Non-owner request flow:** a full-control guest could ask, like
  `new-session`, with the owner or host approving.

## Unexplained failures and unverified leads

Recovered on 2026-10-28 from working notes that memory consolidation deleted
or proposed deleting. These are historical observations, unverified leads and
unfinished verification, not newly reproduced defects; no resolution or other
active record was found in the review. Verify current behavior before changing
code, and remove an item once explained, fixed or no longer worth pursuing.

### View and transcript

- Skill hint (2026-10-18): the hint refreshes only when the view's text
  changes, so a skill-list change waits for the next edit or render. Refreshing
  on skill changes would add per-command or push machinery; accept or close.
- A `.git/info` leak report in `acp-turn-start/tool-hooks` (2026-11-02 replay)
  did not reproduce in a worktree or a real-`.git` clone; suspect
  `mevedel-workspace-ensure-generated-state-ignored` resolving to the real
  repo root in a main-checkout run.
- `mevedel-preset--guard-preparation/test@6` failed once on 2026-11-06
  (should-error did not signal); 0/38 reproductions on 2026-11-07. The test
  now prints the FSM's rejection and error state on failure.
- Browser completion notices and image-button acceptance limits (2026-10-18):
  renderer/host protocol checks did not include browser visual acceptance of the
  final completion notices. Image-button graphical checks verified activation
  and normal-state decoration suppression, not hover-theme or higher-priority
  overlay styling. Locate subsequent evidence or exercise those specific states
  before extending the historical acceptance claims; preserve image activation,
  committed-artifact resolution and notice ownership/wording.
- Remaining layout checks (2026-10-17): minibuffer-active and graphical
  variable-height or wrapped tall drafts have no check; run them on the
  private-KWin runner, preserving draft position and interactive row identity.
- Task-panel mouse disclosure (2026-10-16 suggestion): panels currently toggle
  with TAB/RET while mouse clicks place point. Consider a clickable header only,
  preserving selection on task rows. This is an optional consistency improvement,
  not an implemented feature or a request for per-task detail inspectors.
- Pending-input local preview acceptance limits (2026-10-16): the implementation
  handoff reported rendered-consumer tests, but no separate read-only-authority,
  deferred-render, or live graphical interaction checks. This is historical
  verification scope, not a reproduced defect or proof current coverage is absent.
  Before claiming those scenarios verified, locate subsequent coverage or run a
  bounded check that preserves drafts, selection, viewport, delivery and queue
  management's explicit pause semantics.
- Automatic Read collapse (2026-10-14): an opened Read row later appeared
  collapsed in a live session; replays did not reproduce it. A bounded fold
  trace was prepared; the next step is a consented live reproduction. The prior
  live trial used an unattended scratch view, which deferred rendering: its
  sampler never opened a row while busy, so the final no-anomaly render did not
  test mid-turn row survival. Use an attended view, actually open the row during
  the active turn, and attribute trace events to the relevant buffer and time.
- Mailbox hierarchy (2026-10-14): a following Eval should be a sibling of the
  Finished card inside the same expanded activity. Later live inspection and
  incremental/canonical replays found it nested correctly, but did not reproduce
  the screenshot-time transition. The later collapsed-card display-boundary fix
  does not establish that transition's cause. Replay retained source/view-tail
  anchors and groupability across ToolCall metadata publication or targeted
  replacement; preserve incremental rendering rather than rerender every event.
  Under ADR 0119's rule, only a tool starts a run, so the Eval is a sibling of
  the Finished card only when a tool precedes the card in that run.

### Tests and transport

- End-to-end GPT-6 latency after the 2026-10-14 tool-call lag fixes remained
  unmeasured in the final checkpoint, with no follow-up measurement found;
  the component fixes were verified individually.
- Resource-installation search can exceed its 20 s production deadline under
  heavy host load (2026-10-31). The search covers two fixture files (median
  0.16 s isolated, 0.63 s maximum contended), but tracing caught synchronized
  5-7 s event-loop stalls; the stall source is unidentified. The test watchdog
  now sits above the production deadline.
- `mevedel-window-child-frame-return/test` failed once on parent-layout
  equality (109/110 on 2026-10-31; retry and base 110/110). Graphical,
  unreproduced; rerun on the private-KWin runner if it recurs.
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
