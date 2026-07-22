# System reminders

System reminders are model-visible, user-hidden guidance injected into
the prompt stream as `<system-reminder>` blocks. `mevedel-reminders.el`
owns the reminder struct, firing policy, session and agent scoped
reminder lists, and prompt-transform injection. The base system prompt
teaches the model that these blocks are system context, not user text or
tool output.

This page tracks implemented system reminders and candidate reminders
from the `SYSTEM-REMINDERS.md` research note that can be adapted to
mevedel. It deliberately excludes research-only items that do not map
to current mevedel concepts.

## Reminder flow

```mermaid
flowchart TD
    A[Session, agent, or runtime event] --> B[Evaluate reminder policy]
    B --> C{Should fire now?}
    C -- No --> D[Keep or throttle reminder]
    C -- Yes --> E[Render system-reminder block]
    E --> F[Inject through prompt transform or inline event path]
    F --> G[Update last-fired or consume FIFO event]
    G --> H[Model receives hidden guidance]
```

## Implemented

### Plan-file reference reminder

Accepted plans are recorded in session `plan-metadata` and persisted as
immutable `goals/<goal-id>/cycle-NNN-plan.md` artifacts under the session
directory. The mutable proposal remains at `current-plan.md`. The one-shot
`plan-reference` reminder surfaces bounded contents of the approved
plan on later turns when it may still be relevant. The Goal controller owns
planning and review prompts directly; their read-only restriction is enforced
by the permission system rather than by a reminder.

Standalone Plan Direct handoff does not use this reminder. Here and Worktree
targets receive the immutable artifact path and full accepted plan in the
canonical implementation prompt; Summary contexts additionally receive their
cached handoff before that prompt.

### Accepted-plan verification reminder

Accepting a plan marks verification as pending in `plan-metadata`.
The existing `verification-suggestion` reminder now mentions approved
plan execution verification while that flag is active, and spawning a
verifier clears the flag.

### Agent read-only role reminders

Verifier invocations get an every-turn `verifier-read-only` reminder
that reinforces verification-only behavior and the required verdict line.
Reviewer invocations get an every-turn `reviewer-read-only` reminder
that reinforces review-only behavior, no patching, and strict JSON review
output.

### Specialist navigation availability reminders

Workspace buffers are probed for live specialist navigation support.
One-shot reminders surface xref, Imenu, Treesitter, and Emacs Lisp
introspection when those capabilities are available and, when relevant,
include a `ToolSearch(..., load=true)` hint for deferred tools. These
reminders steer code-symbol work toward `XrefReferences`,
`XrefDefinitions`, `Imenu`, `Treesitter`, and loaded-state Emacs Lisp
introspection instead of broad text search or whole-file reads.

### Generic-tool specialist nudges

Successful `Grep` and `Read` results may receive a bounded appended
`<system-reminder>` when the call looks like code-symbol or structure
discovery and a specialist tool would be more precise. Nudges are
throttled per specialist family and suppress obvious good uses of the
generic tools, such as regex/literal Grep searches, exact Read ranges,
media/PDF reads, duplicate reads, and non-code files.

`mevedel-specialist-nudges.el` owns this post-tool prompting policy,
including eligibility, per-session or per-invocation throttling, deferred-tool
guidance, and exact reminder text. `mevedel-reminders.el` remains the owner of
workspace capability probing and the independent one-shot availability
reminders described above.

### PDF and large-attachment reference reminders

Large PDFs read without a `pages` selector receive an appended
`<system-reminder>` telling the model to prefer bounded
`Read(..., pages="START-END")` requests for relevant pages. Large PDFs
attached through `@file` get the same hidden guidance, and oversized
PDF `@file` attachments that cannot be attached include bounded-page
guidance in the rejection reminder.

### Runtime status and event reminders

- **Date-change:** `mevedel-reminders-make-date-change` compares the
  current date to the session's `last-observed-date` slot and updates
  the slot after firing.
- **Compaction availability:**
  `mevedel-reminders-make-compaction-available` fires once when
  automatic compaction is enabled and context usage crosses the
  configured reminder threshold.
- **Compact file-reference:** compaction queues reminders for file
  references whose contents were not retained; the `pending-events`
  reminder consumes the session FIFO on the next prompt.
- **Goal lifecycle event:** material transitions such as start, pause, resume,
  plan acceptance, review outcome, budget exhaustion, and compaction enqueue a
  one-shot event. These reminders only orient the next request; the Goal
  sidecar and its generated context fragment remain authoritative, and
  reminder delivery never changes controller state.
- **Token usage:** `mevedel-reminders-make-token-usage` reports high
  context pressure using the compaction token state, with sparse
  repeat firing.
- **Agent listing delta:**
  `mevedel-reminders-make-agent-listing-delta` compares the current
  visible named-role roster to the session's prior snapshot.
- **Skill listing delta:**
  `mevedel-reminders-make-skills-delta` compares the current active,
  enabled, model-invocable skill roster to the session's
  `skills-snapshot`. The first snapshot is silent; later changes list
  added skills with descriptions and removed skills by name.
- **Skill roster budget:**
  `mevedel-reminders-make-skills-roster-budget` fires when the
  prompt-context skill roster first becomes truncated or omitted, or when
  that budget status changes. The prompt section itself still carries the
  roster budget note; this reminder is only the event-shaped nudge.
- **Path-scoped skill activation:** tool activity that touches a matching
  path can activate dormant enabled path-scoped skills. When activated
  skills are model-invocable, a pending event names the triggering path
  and a capped list of newly active skills.
- **Hook outcome:** hooks record blocking outcomes through
  `mevedel-hooks-record-session-reminder`, consumed by `pending-events`;
  standalone `:system-message` remains a transient notification and hook-log
  entry. Additional hook context still uses `<hook-context>`.
Default session reminders are installed idempotently through
`mevedel-reminders-install-defaults`. Event-shaped reminders use the
session pending-reminder FIFO and the `pending-events` reminder unless
they are injected inline by the view layer.
