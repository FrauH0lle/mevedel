# Project backlog

Canonical home for concise future-work requests and unresolved defects.
Detailed plans, investigations, and review reports belong in `work://shared/`.
Read this before planning work in any listed area.

Use the inbox for ideas that have not been investigated yet. Promote an
item to a detailed entry when its scope and current status are understood.
Remove items when they are implemented, obsolete, or no longer valuable.

## Inbox

- Consider making mevedel's data buffers hidden
- Donwload button in browser/lobby/files beside the files
  - Also drag-n-drop of files for upload
- Make system prompt/role a bit more general so mevedel can also be used for writing for example
- C-c RET in composer does not work if cursor is IN text?
- Opening Artifact via lobby -> Artifacts did not work, required to first open it via a session which has the Artifact attached.
- Add timestamps with time+date
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
- The browser view hides thinking blocks it seems
- The cursor has the tendency to jump to the "  [Manage]" row of "background work" on updates/redraws even though it was on the composer or elsewhere
- We meed to add this as a development guideline: The user should not get surprised, e.g. cursor jumps, windows opening, etc.
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
note deferral, cancellation, recovery, undo, and idempotence. Measure editor
responsiveness alongside CPU, memory, and backlog throughput on representative
workloads. Distinguish moving work off the main thread from reducing total cost.

## Execution

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
