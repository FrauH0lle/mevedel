# Project backlog

Canonical home for concise future-work requests and unresolved defects.
Detailed plans, investigations, and review reports belong in `work://shared/`.
Read this before planning work in any listed area.

Use the inbox for ideas that have not been investigated yet. Promote an
item to a detailed entry when its scope and current status are understood.
Remove items when they are implemented, obsolete, or no longer valuable.

## Inbox

- Continue the work on the "always visible prompt" (the composer stays visible while the transcript scrolls). Two prototypes, from 2026-10-26, are in `.scratch/floating-composer/`: a child frame (`mevedel-floating-composer.el`) and a split window (`check-split.el`). Not the pinned prompt header shipped on 2026-10-31.

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

## View

### Cursor jumps to background-work Manage row

Still open: the user reconfirmed on 2026-11-04 that updates/redraws move point
from the composer or elsewhere to the background-work `[Manage]` row. The
October 31 candidate 4 fix addresses scrolling away and back, not this report.
An expanded isolated graphical replay preserved point across 216 updates with
changing panel sizes, multiple windows, and status/full/stream renders; its
negative control detected lost restoration. That does not reproduce the live
configuration or disprove the recurrence. Capture the actual command/update/
redisplay ordering in the affected view with bounded, text-free diagnostics
before adding another preservation layer. No redraw-only fix is verified.
The user later reported point beside `| [full-auto]` when returning to this
workspace by clicking the tab bar, not the conversation text. The recorder had
already timed out; whether point moved while hidden or on activation is not
established.
On 2026-11-05 the user also reported a jump without input while a request ran.
The screenshot shows point on the final `Worked for …` footer; a read-only
inspection found that view's stored point still before the composer. Tab
switching is therefore not required by the report. Include the entire terminal
callback and its post-render cleanup in the capture, with an empty composer as
well as a draft. A live capture on 2026-11-05 now brackets a completion-time
composer exit: the full terminal wrapper returned with point at input start,
but the next preserving-writer entry about 19 ms later already had buffer and
window point 57 characters before the unchanged input start. Selection, focus
and command serial stayed unchanged; later inspection found point on the
`Worked for …` footer. The moving caller remains unidentified. Investigate the
enclosing completion hooks and window/redisplay restoration outside that
terminal wrapper, rather than attributing the jump to its recorded body.
A later navigation-aware capture brackets another exit after a successful
status-zone shrink: the next observer sample about 859 ms later has buffer and
window point 15 characters before input, at status-start minus one. No command
is recorded; focus, selection and window start are unchanged, while pixel
vscroll falls from 3 to zero. The caret remained outside after capture. Trace
the status caller and subsequent redisplay, not only request completion. A
private graphical held-row release with 20-pixel partial scrolling retained
the caret with and without redundant start setters; it did not reproduce this
exit or establish a correction.
A later status/pre-redisplay capture ends at an upward-wheel command entry with
the caret still in the composer; its triggering snapshot is missing, so it
does not identify a background caller. The corrected all-view capture on
2026-11-06 retains a delayed exit after tab activation: command-post and two
window-change callbacks leave point at input, but the next pre-redisplay hook
entry about 19 ms later has buffer/window point 26 characters before unchanged
input. Window start advances and pixel vscroll falls from 20 to zero. Movement
already precedes that hook body; isolate tab/window restoration and redisplay
between these calls rather than attributing it to the dispatcher itself.
A private graphical replay through the real tab mouse-down handler preserves
the caret with zero/20-pixel scrolling, empty/multiline drafts, and deliberately
pending offscreen viewports. Its delayed-exit control detects a 26-character
injection. Repeating with the installed runtime and personal Evil, workspace
and view-local hooks also retains the caret; a matched clean-config control
does too. One configured case changes window start during settling, but these
caret checks do not establish visible viewport stability. A user-authorized
private rendered-history copy also passes both configured seven-case replays,
including the delayed-exit control; no natural caret exit occurs. The larger
frame is near-live-sized, not an exact geometry match. Current rather than
incident history, synthetic canonical DATA/status, omitted window overlays and
controlled mouse-handler delivery still prevent full live equivalence. This
bounded replay study establishes no cursor fix. Narrow the native restoration /
redisplay gap with live observations rather than adding blind preservation or
more fixture variants.
Subsequent external sampling preserves six composer exits; two measured against
the editable-input start lose 48 and 56 characters while partial vscroll resets
from 20 pixels to zero, without input-counter or endpoint window-size changes.
A paired native Emacs 31.1 experiment now demonstrates a viewport-first
mechanism: with a static header and a fully visible bottom cursor row, an
optional window start accepts the partial viewport, then redisplay clears
vscroll and relocates point to the preceding row before invoking pre-redisplay
again. The matched `set-window-vscroll` preservation control retains point and
viewport; the earlier header-free pair only moved the viewport. No text changes
occur in either native case. This reproduces a mechanism, not the live incident:
its actual glyph rows, chrome heights and pending native flags were not captured.
A bounded frozen-source comparison through the actual production viewport
setter now reproduces the same ordering under an artificial optional-start
stimulus: the baseline returns with point intact, then redisplay resets vscroll
and moves point 50 characters backward. Adding only the fourth preservation
argument retains point and the 3-pixel scroll. All eight diagnostic cases retain
valid setup and text/draft/mark/selection/chrome invariants. The enclosing clean
wrapper retains 3 pixels in both arms; the changed wrapper legitimately clamps
to 2 pixels in both arms, with zero strict bottom margin, so neither establishes
a wrapper-level benefit. An externally poisoned numeric no-op fails in both
arms without boundary setters; retain the redundant-setter guards. This verifies
the setter mechanism, not preserver-originated pending state or live causation.
The viewport setter now requests native preservation when numeric vscroll changes,
with a maintained native regression that fails after redisplay on the old call
and passes with only the fourth argument added. This corrects the demonstrated
setter mechanism; the archived live exits and any preserver-originated pending
state remain unattributed. Do not treat the live incident as closed.

### Pinned prompt on the remaining surfaces

The main view and agent transcript views pin the governing prompt on its own
header row below the status strip
([view contract](view.md#status-strip-and-cockpit-routing)). Two surfaces still
lose the prompt once it scrolls away:

- Directive frames set a window-level `header-line-format` to their condensed
  directive header (`mevedel-directive-frame--ensure`), which hides the
  buffer's prompt row. Decide whether the narrow frame should spend a second
  row on it, or show the prompt only while its header is off screen.
- The browser viewer (`relay/viewer/`) has no pinned prompt. A sticky element
  above the transcript, fed from the rendered prompt headers, is the analogue;
  check it on the narrow-phone viewer.

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

## Plugins

### Opt-in update notifications

Managed plugins are shallow GitHub clones (`mevedel-plugins-install`); they
change only through `/plugin update NAME` (`git pull --ff-only`) or the
cockpit's update action. Nothing tells the user an update exists. Automatic
updates stay out of scope: they change executable code, and hooks, without
consent.

Shallow clones never refresh their remote refs, so any check needs the
network. The current git seam (`mevedel-plugins--process-git`) is a
synchronous `process-file` that would block Emacs, and the plugin modules
have no async process, timer or place to surface results. Design:

- `mevedel-plugins-update-check-interval`: nil (default, no checks) or a
  number of days. No network call at load or startup: the first check runs
  on an idle timer after the registry is first used in a session, and the
  last check time persists in global plugin state, so restarts within the
  interval do not check again.
- The check runs `git ls-remote origin HEAD` asynchronously (`make-process`,
  one plugin at a time) for each managed root and compares the result with
  local `HEAD`. Results are cached per plugin.
- Surfacing follows NO SURPRISES in CODING_STANDARDS.md: one echo-area message
  per newly seen upstream commit ("updates available for N plugins; /plugin
  to review"), never repeated for the same commit; cockpit rows show
  "update available" from the cache. Offline or failed checks stay silent
  apart from a "check failed" row note.
- A cockpit "Check for updates" action runs the same async check on demand,
  regardless of the interval setting.
- Scope: `mevedel-plugin-lifecycle.el` (async check), `mevedel-plugin-registry.el`
  (setting, cached state), `mevedel-plugin-ui.el` (row note, action),
  docs/skills.md; tests through an async git-executor seam, including the
  "no network at load" guarantee.

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

- Stale-zone scan cost (2026-10-18): `mevedel-view-zone--delete-stale-text`
  scans the whole buffer on every changed reconcile, including each per-second
  rail update. A synthetic transcript showed cost growing with size; no real
  long session was profiled. Profile one before narrowing the scan, and keep
  stale-text recovery across undo, reinsertion and lost local state.
- Skill hint (2026-10-18): the hint refreshes only when the view's text
  changes, so a skill-list change waits for the next edit or render. Refreshing
  on skill changes would add per-command or push machinery; accept or close.
- Ask overhaul timing failures (2026-10-18): full-suite timing failures in
  `journal-recall-expiry` and
  `acp-turn-start/tool-hooks` did not reproduce under 8 contention copies on
  one CPU (2026-10-31); only the original ordered conditions remain to try.
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
