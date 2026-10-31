# Project backlog

Canonical home for concise future-work requests and unresolved defects.
Detailed plans, investigations, and review reports belong in `work://shared/`.
Read this before planning work in any listed area.

Use the inbox for ideas that have not been investigated yet. Promote an
item to a detailed entry when its scope and current status are understood.
Remove items when they are implemented, obsolete, or no longer valuable.

## Inbox

- Donwload button in browser/lobby/files beside the files
  - Also drag-n-drop of files for upload
- Make system prompt/role a bit more general so mevedel can also be used for writing for example
- Opening Artifact via lobby -> Artifacts did not work, required to first open it via a session which has the Artifact attached.
- This here about memory:
  ```
  **Yes—as sandbox protection scaffolding, not memory storage.**

  The sandbox creates these empty directories under writable roots:

  - `.mevedel/artifacts/.state/`
  - `.mevedel/leases/`

  It uses them as protected, read-only mount points so a sandboxed command cannot create or tamper with Mevedel’s internal state. The creation logic is in `mevedel-sandbox.el:618–630`, and those directories deliberately persist after cleanup (`mevedel-sandbox.el:732–741`).

  That explains the exact empty structure inside your memory directory. Removing it would not delete memories, but sandbox preparation may recreate it.

  My earlier suggestion that the memory directory had been treated as a workspace was incorrect: this scaffolding is created for **writable roots**, which can include the memory directory.
  ```
- The cursor has the tendency to jump to the "  [Manage]" row of "background work" on updates/redraws even though it was on the composer or elsewhere
- Why is the memory collection (cockpit -> Candidates or Memories) so slow? 5s or more? Especially for already included memories, what work is there to be done?
- auto update plugins? Or at least notifications?
- Maybe add edit/rewind/fork in browser?

## Memory and journal

### Asynchronous processing

Normal journal and memory housekeeping should be unobtrusive: no typing or
scrolling stalls, focus or composer changes, or routine success notifications.
Keep actionable failures visible.

Investigate and reduce main-thread stalls in memory and journal processing,
including authenticated recovery, proposal application, publication, and cleanup.
Reuse background workers where safe; preserve original-target authority, claim
lifetime, fresh checks after mutations, unsaved-buffer protection, active-turn
in-use note protection, cancellation, recovery, undo, and idempotence. Measure editor
responsiveness alongside CPU, memory, and backlog throughput on representative
workloads. Distinguish moving work off the main thread from reducing total cost.

### Shared-note lifecycle follow-up

ADR 0128 replaced completion-proof note review with 14-day expiry and
extraction-only review (merged 2026-10-30). A one-run real-provider replay
deleted 40 of 40 curated notes in 191 seconds, and an eight-note check
extracted 4 of 4 planted lessons without promoting routine notes. Remaining:
- Observe a real daily cycle at 50-100 arrivals per day, including live
  editor responsiveness.
- Leases do not observe shell access by ordinary path, accesses before an editor
  restart, or sessions on another client.

## Execution

### Harness-enforced Bash timeouts

Managed Bash commands run until they exit or are stopped. `145c66a` removed
`mevedel-bash-timeout` (120 s default) and the model-facing `timeout_seconds`,
so only a native `timeout` the model writes itself bounds a command. A
## Agents

- After compaction, a retained agent can resume its original assignment
  instead of its latest one (seen twice on 2026-10-29, once writing into a
  frozen run). Agent compaction should carry the parent's latest assignment
  message verbatim.
- Grandchild agents mail `/root` directly (about 15 times on 2026-10-29).
  Default a child's `SendMessage` target to its parent, or reject messages that
  skip a level unless explicitly flagged.
- After the MCP idle timeout (1,800 s) interrupts `WaitAgent`, later calls
  answer "Agent is already waiting" until the stuck wait clears.

## Checks

- `scripts/check-standards.py` should reject duplicate ERT test names across
  files; today only full-suite discovery catches them.
- It should also reject committed tests that reference untracked
  `test/fixtures/` files (`present-wayland-sync.c` broke clean checkouts until
  7eefdf5f; `test-mevedel-journal-cleanup-memory.el` was never committed until
  762589f6).
- Review tests for absolute deadlines under about one second that are not
  shown to measure the code under test; two such tests failed only under
  8-worker load on 2026-10-29.

forgotten command keeps running and holds the
[system sleep inhibitor](sessions.md#system-sleep). When Bash completion wake-ups
and the [Monitor tool](tools.md#monitor) were added (2026-10), the user decided
against restoring a Bash deadline for now; only Monitor expires (`timeout_ms`).
Revisit if forgotten commands become a problem in practice. Reference point:
Claude Code caps background commands at 10 minutes by default and 2 hours at
most. Expiry would reuse the existing TERM/KILL process-group teardown and
report a timeout outcome.

### Aggregate Monitor event pressure

Investigate whether many individually sub-threshold monitors can accumulate
excessive session-wide unread events or model wake requests. Current flood
limits are per monitor; serialized idle admission alone does not establish an
aggregate byte, rate, or request budget. Exercise concurrent producers and held
delivery before choosing any additional policy. This is an unverified design-risk
lead, not a reproduced defect or an approved requirement for new caps. Preserve
terminal evidence and prioritize reliable message delivery over suppressing wakes;
never silently drop events to satisfy a limit.

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

### Goal-continuation evidence coverage

Consider a producer-to-permission-review regression for Goal continuation:
exercise the generated turn and verify the fixed trigger and active Goal objective
reach review evidence without implying fresh human authorization. Existing
background-event coverage already checks native/ACP production, original/restored
transcripts and mailbox-payload exclusion; do not report that path as uncovered.
This is a historical test recommendation, not a demonstrated permission defect.

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

## Unexplained failures and unverified leads

Recovered on 2026-10-28 from working notes that memory consolidation deleted
or proposed deleting. These are historical observations, unverified leads and
unfinished verification, not newly reproduced defects; no resolution or other
active record was found in the review. Verify current behavior before changing
code, and remove an item once explained, fixed or no longer worth pursuing.

### View and transcript

- Status-zone audit follow-ups (2026-10-18): investigate whether copying panel
  text into the composer can retain zone ownership properties and allow a later
  reconcile after region recreation to delete draft text. This was an unverified
  hypothesis, not a reproduced defect; exercise the real copy/yank/recreation
  path before changing behavior. The same audit left stale-zone scan cost as a
  profiling lead: a synthetic transcript matched to an observed property density
  showed increasing scan cost with size, but no real-session optimization was
  established. Profile a representative long session before narrowing scans;
  preserve stale-text recovery across undo, reinsertion and lost local state.
- Historical status-audit verification (2026-10-18): five full-suite failures
  in side-conversation and Claude selected-context tests reproduced on both the
  patch and its base with the same shared dependencies after an earlier run
  passed. This rules out a patch-specific regression in that comparison, but
  does not identify the environment change. Locate later dependency/load-order
  resolution before treating that historical validation as clean.
- Composer jumpiness audit acceptance limits (2026-10-18): the directive-prompt
  fix did not cover text-scale changes without resize; truncation remained stale
  until a refit trigger. The skill-hint fix deferred skill-list changes without
  draft edits until the next edit. Locate later coverage or exercise these
  specific transitions before extending acceptance; these are historical limits,
  not newly reproduced defects. The same branch's full suite reported one
  unexpected `mevedel-present-wayland` failure; two passing isolated reruns under
  a GUI lock did not alone establish the claimed compositor-contention cause.
  Reproduce the original concurrent conditions or find later explanatory evidence
  before treating that failure as resolved.
- Ask overhaul follow-ups (2026-10-18): the final diagnosis left C-g cancelling
  the whole form as a raised, unchanged behavior question. Recheck current
  cancellation semantics and user intent before changing it. Its full-suite run
  also reported timing failures in journal-recall-expiry and ACP turn tool-hooks;
  three passing isolated reruns did not explain them. Find later resolution or
  reproduce the original ordered conditions before declaring those failures
  resolved. These are historical leads, not reproduced current failures.
- Browser completion notices and image-button acceptance limits (2026-10-18):
  renderer/host protocol checks did not include browser visual acceptance of the
  final completion notices. Image-button graphical checks verified activation
  and normal-state decoration suppression, not hover-theme or higher-priority
  overlay styling. Locate subsequent evidence or exercise those specific states
  before extending the historical acceptance claims; preserve image activation,
  committed-artifact resolution and notice ownership/wording.
- Composer/status layout acceptance limits (2026-10-17): the scoped cross-review
  resolved five findings using batch probes of production preservers and rendered
  content, but did not measure minibuffer-active or graphical variable-height
  cases. The rail microbenchmark did not establish graphical redisplay or
  long-session performance; integrated graphical acceptance remained root-owned.
  Before closing those checks, locate later evidence or run bounded multiwindow,
  wrapped/tall-draft and simultaneous status/approval scenarios, preserving draft
  position and interactive row identity. These are historical limits, not proof
  of a current defect or missing coverage.
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
- Transient chronology: future Read rows briefly shown inside an earlier
  activity group; 18 targeted checks stayed green.
- Mailbox hierarchy (2026-10-14): a following Eval should be a sibling of the
  Finished card inside the same expanded activity. Later live inspection and
  incremental/canonical replays found it nested correctly, but did not reproduce
  the screenshot-time transition. The later collapsed-card display-boundary fix
  does not establish that transition's cause. Replay retained source/view-tail
  anchors and groupability across ToolCall metadata publication or targeted
  replacement; preserve incremental rendering rather than rerender every event.
- `mevedel-view-interaction.el` initialization resets its overlay registries
  without deleting existing overlays; possible orphan overlays on
  reinitialization (setup calls it from `mevedel-view.el`).
- Identical ID-less pre-start running calls may merge until `started` creates
  a sibling (`mevedel-collaboration.el` matching); reachability and visible
  impact remain unverified.

### Shared editing

- A browser attached through a non-holder Emacs sees another Emacs's artifact
  edits only on its next read: automatic fanout enumerates holder-local rooms
  (`mevedel-collaboration-artifact.el`). Either document this limitation in
  `docs/shared-editing.md` or propagate updates.

### Tests and transport

- First-attempt curl exit 92 terminal error, observed before the fix for lost
  terminal callbacks after streamed headers; its cause remains unexplained.
- Eight-worker full-suite timeout of the Glob no-match case
  (`test/test-mevedel-tool-fs-search.el`); an exact prefix replay passed.
- Execution-scheduler reader exceeded its 0.3-second startup deadline in a
  full-suite run (`test/test-mevedel-execution-scheduler.el`).
- Coalesced-timer assertion found its next deadline no longer in the future
  (`test/test-mevedel-utilities.el`); an integral-second boundary crossing is a
  hypothesis.
- Sandboxed `git stash push -u` failed with "Device or resource busy" removing
  `.mevedel/memory/.mevedel/artifacts/.state`; protected sandbox mounts were
  suspected, not established as the cause.
- A public execution log (`execution-CWsvof.log`) named in an earlier summary
  could not later be found; its availability history remains unexplained.
- End-to-end GPT-6 latency after the 2026-10-14 tool-call lag fixes remained
  unmeasured in the final checkpoint, with no follow-up measurement found;
  the component fixes were verified individually.
