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

## Implemented

### Plan-mode workflow reminders

When permission mode is `plan`, `plan-mode` reminds the model to stay
read-only, gather only needed context, ask only undiscoverable
questions, and finish with exactly one `<proposed_plan>` block. The
first firing includes the preferred plan shape: title, Summary, Key
Changes, Regression Coverage, Validation, and Assumptions. Later
firings are sparse.

### Plan-mode reentry and exit reminders

Entering Plan mode installs `plan-mode-reentry` when a current plan
artifact already exists. Exiting Plan mode installs `plan-mode-exit`
to mark that implementation permissions have resumed.

### Plan-file reference reminder

Accepted plans are recorded in session `plan-metadata` and persisted
as `plans/current.md` under the session directory. The one-shot
`plan-reference` reminder surfaces bounded contents of the approved
plan on later turns when it may still be relevant.

### Accepted-plan verification reminder

Accepting a plan marks verification as pending in `plan-metadata`.
The existing `verification-suggestion` reminder now mentions approved
plan execution verification while that flag is active, and spawning a
verifier clears the flag.

## Implementable now

These have the required runtime concepts in the codebase today. They
may still need new session slots, reminder constructors, or tests, but
they are not blocked on another feature landing first.

### Date-change reminder

- **Purpose:** Keep the model's current-date context fresh during long
  sessions without asking the user to restate it.
- **Current state:** Environment context is included in the prompt, but
  there is no per-session date-change detector.
- **Implementation path:** Add a session slot for the last observed date
  string. Initialize it on session creation/resume. Add a reminder that
  fires when `(format-time-string "%F")` changes and updates the slot
  after emitting the reminder.
- **Likely files:** `mevedel-structs.el`, `mevedel-chat.el`,
  `mevedel-session-persistence.el`, `mevedel-reminders.el`,
  `test/test-mevedel-reminders.el`.

### Compaction availability reminder

- **Purpose:** Tell the model that automatic compaction is available, so
  it does not stop prematurely in long sessions.
- **Current state:** Auto-compaction exists and has token-baseline
  plumbing, but no model-visible reminder explains it.
- **Implementation path:** Add a one-shot or sparse reminder that fires
  when auto-compaction is enabled and the session has crossed a
  configured context-usage threshold. Reuse the existing compaction
  baseline/state where possible.
- **Likely files:** `mevedel-compact.el`, `mevedel-reminders.el`,
  `test/test-mevedel-reminders.el`, `test/test-mevedel-compact.el`.

### Compact file-reference reminder

- **Purpose:** After compaction, tell the model when previously read
  file contents were omitted and should be re-read if needed.
- **Current state:** Compaction preserves a tail and summary, and the
  session tracks touched files, but omitted file references are not
  queued as reminders.
- **Implementation path:** During compaction, collect file references
  whose contents cannot be retained. Store a bounded pending reminder
  payload on the session and consume it in `mevedel-reminders--transform`
  on the next turn.
- **Likely files:** `mevedel-compact.el`, `mevedel-reminders.el`,
  `mevedel-structs.el`, `test/test-mevedel-compact.el`.

### Token usage reminder

- **Purpose:** Make the model aware of context pressure near limits.
- **Current state:** Token information is visible in the UI and
  auto-compaction tracks API-reported baselines, but there is no
  model-visible token reminder.
- **Implementation path:** Add a sparse reminder that fires only near a
  high context-usage threshold. Use existing compaction/header token
  state rather than adding a second estimator.
- **Likely files:** `mevedel-compact.el`, `mevedel-chat.el`,
  `mevedel-reminders.el`, `test/test-mevedel-reminders.el`.

### Agent listing delta reminder

- **Purpose:** Notify the model when available agent types change during
  a session.
- **Current state:** Agent definitions and skill hot reload already
  exist; the session does not snapshot previously advertised agent
  names.
- **Implementation path:** Store a per-session snapshot of visible agent
  types. Compare it before reminder collection, then emit added/removed
  descriptions and update the snapshot.
- **Likely files:** `mevedel-agents.el`, `mevedel-skills.el`,
  `mevedel-reminders.el`, `test/test-mevedel-reminders.el`.

### Hook outcome reminders

- **Purpose:** Convert hook blocking/success/additional-context outcomes
  into consistent model-visible guidance where that is more useful than
  plain tool feedback.
- **Current state:** Hooks already support additional context through
  `<hook-context>` and can alter tool feedback.
- **Implementation path:** Normalize selected hook decisions into a
  pending reminder/context queue on the session. Keep existing
  `<hook-context>` for additional context; add explicit reminder text
  only for blocking or continuation cases where the model needs to adapt
  its next action.
- **Likely files:** `mevedel-hooks.el`, `mevedel-pipeline.el`,
  `mevedel-reminders.el`, `test/test-mevedel-hooks.el`,
  `test/test-mevedel-pipeline.el`.

### Queued user-message reminder

- **Purpose:** Tell the model that the user sent another message while a
  request was already active, and that it should account for that input
  while continuing.
- **Current state:** mevedel has queued user-message machinery, but the
  model-visible framing is not a dedicated reminder.
- **Implementation path:** When draining queued user messages, wrap the
  queued prompt in reminder text that says it arrived mid-turn and
  should be addressed without discarding current work.
- **Likely files:** `mevedel-view.el`, `mevedel-chat.el`,
  `mevedel-reminders.el`, `test/test-mevedel-view.el`.

### Background task status delta reminders

- **Purpose:** Provide richer model-visible updates when background
  agents stop, continue running, complete, or fail.
- **Current state:** `background-agents-pending` tells the parent which
  agents are still running, and agent result blocks report terminal
  outcomes.
- **Implementation path:** Extend agent status transitions to enqueue
  pending reminder events with status, agent id, description, optional
  delta summary, and transcript path. Consume those events through the
  reminder transform.
- **Likely files:** `mevedel-agent-exec.el`, `mevedel-tool-ui.el`,
  `mevedel-reminders.el`,
  `test/test-mevedel-agent-transcript-persistence.el`.

## Implementable later

These are reasonable reminder candidates, but they should wait until
another feature or integration exists. Implementing the reminder first
would create placeholder behavior or duplicate unrelated design work.

### PDF and large-attachment reference reminders

- **Blocked by:** A PDF/page-range read path or richer attachment
  handling.
- **Future path:** Once PDFs are readable through `Read` or `@file`,
  emit a reminder for large PDFs telling the model to request bounded
  page ranges.
- **Likely files:** `mevedel-tool-fs.el`, `mevedel-mentions.el`,
  `mevedel-reminders.el`.

### Opened-file and selected-lines reminders

- **Blocked by:** An explicit Emacs-side IDE state capture feature.
- **Future path:** Add commands or lightweight hooks that record opened
  files and active regions into session pending context. Consume that
  state as one-shot reminders. Prefer command-driven capture first to
  avoid noisy global advice.
- **Likely files:** a new IDE-state module, `mevedel-view.el`,
  `mevedel-reminders.el`.

### Output-style reminder

- **Blocked by:** A user-facing output-style feature.
- **Future path:** Add an output-style session slot and slash command.
  When the style is non-default, emit sparse reminders containing the
  style-specific constraints.
- **Likely files:** `mevedel-structs.el`, `mevedel-skills.el`,
  `mevedel-reminders.el`.

### USD budget reminder

- **Blocked by:** A configured budget source and reliable cost
  accounting.
- **Future path:** Once budget tracking exists, emit sparse reminders
  near spend thresholds. Do not infer spend from token estimates alone.
- **Likely files:** token/cost accounting module, `mevedel-reminders.el`.

### Reasoning-effort reminder

- **Blocked by:** Applying `effort` overrides to backend-specific gptel
  request parameters.
- **Future path:** After effort overrides are wired, emit a one-shot
  reminder when the user or skill changes effort for the current turn.
- **Likely files:** `mevedel-skills.el`, `mevedel-models.el`,
  `mevedel-reminders.el`.

### MCP instruction delta reminder

- **Blocked by:** A reliable source of MCP server instructions and
  server lifecycle deltas.
- **Future path:** Store per-session hashes of MCP server instructions.
  Emit added/removed instruction blocks when servers connect,
  disconnect, or update their guidance.
- **Likely files:** `mevedel-mentions.el`, MCP integration code,
  `mevedel-reminders.el`.
