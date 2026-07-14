# Project backlog

Canonical home for project notes, todos, feature ideas, fixes, and
explicitly deferred work. Read this before planning work in any listed
area.

Use the inbox for ideas that have not been investigated yet. Promote an
item to a detailed entry when its scope and current status are understood.
Remove items when they are implemented, obsolete, or no longer valuable.

## Inbox

- Add a memory-verification slash command or skill that consolidates project
  memories and checks whether they are still accurate; explore whether a
  weekly automated check is useful.
- Rename permission modes: `default` -> `ask`, `accept-edits` -> `edits`, and
  `trust-all` -> `auto`. Thus, UI and internal naming conventions are the same.
  - Or find better names, also fine.
- Pause the "working..." timer while user input is pending.
- Parse shell commands with tree-sitter when available.

## Entry format

Each entry records its source, owed change, reason for deferral, current
status, and blast radius. Keep entries terse and remove them when they
become implemented, obsolete, or unjustified.

## Composer and mentions

### Atomic mention bindings

- **Source:** `mevedel-view-composer.el`; `mevedel-view-history.el`;
  `mevedel-mentions.el`
- **What's owed:** First, let completed `$skill` mentions retain the exact
  resolved skill independently of their visible text and carry those bindings
  through dispatch, queueing, retries, and history. Follow with the same atomic
  binding contract for `@ref`, `@file`, and `@mcp` mentions once the skill path
  has established the shared representation.
- **Why deferred:** The binding must survive composer edits, queueing, input
  history, and mention expansion without making ordinary prompt strings
  harder to use.
- **Status check:** Claimed for the skill invocation and model/effort policy
  implementation. The completed design preserves exact source identity,
  reloads that source's latest contents, and invalidates the binding only when
  its token is edited. Composer, queue, and history still need the shared
  representation. The non-skill mention kinds remain the next slice.
- **Blast radius:** Duplicate names, renamed targets, and restored prompts can
  resolve differently from the target originally selected by completion.

## Sub-agents and coordination

### Bound concurrent sub-agent execution

- **Source:** `mevedel-agent-runtime.el`; `mevedel-agent-exec.el`;
  `mevedel-skills-invoke.el`
- **What's owed:** Apply one configurable session-wide concurrency limit to all
  sub-agent dispatch, including the Agent tool, fork skills, and first-class
  review/verification workers. Queue overflow in FIFO order, start queued work
  as slots open, and clear queued work when its owning request is cancelled.
  Cover nested agent dispatch with one shared depth policy and avoid
  parent-waits-for-child deadlocks under the concurrency cap; skill dispatch
  must not maintain a separate recursion limit.
- **Why deferred:** Dispatch currently starts agents immediately. A shared
  scheduler must preserve foreground/background waiting, cancellation, and
  transcript lifecycle semantics across every dispatch origin.
- **Status check:** Agent runtime tracks active background workers and FSMs but
  does not impose a shared concurrency limit or queue pending launches.
- **Blast radius:** Unbounded parallel dispatch can exhaust provider rate
  limits and local resources; separate per-feature limits would produce
  inconsistent ordering and resource accounting.

### Retry transient sub-agent request turns

- **Source:** `mevedel-agent-runtime.el`; `mevedel-agent-exec.el`;
  gptel request FSM handlers
- **What's owed:** Retry a bounded number of transient transport/provider
  failures, initially two retries, on the same sub-agent invocation. Retry only
  the failed HTTP turn while preserving its transcript, invocation identity,
  and completed tool results; never restart the whole agent automatically.
- **Why deferred:** Safe retry must reset partial response/FSM state without
  duplicating earlier tool calls or edits, and must distinguish retryable rate
  limits, timeouts, and selected 5xx responses from terminal failures.
- **Status check:** Sub-agent request errors currently settle as terminal
  outcomes; there is no general bounded transient retry policy.
- **Blast radius:** Missing retries make temporary provider failures abort
  otherwise useful work. Retrying at the wrong boundary can duplicate side
  effects or corrupt the child transcript.

### Git worktree isolation for parallel workers

- **Source:** `mevedel-worktree.el`; `mevedel-agent-runtime.el`
- **What's owed:** Assign isolated git worktrees to parallel workers that may
  edit overlapping files.
- **Why deferred:** The existing worktree UI manages user-created worktrees,
  but automatic worker allocation and cleanup need separate lifecycle rules.
- **Status check:** `mevedel-worktree.el` supports status, create, list, and
  deletion for sessions; agent dispatch does not allocate worker worktrees.
- **Blast radius:** Parallel workers must avoid or serialize conflicting file
  edits.

## Request lifecycle

### Prevent system sleep during active requests

- **Source:** `mevedel-structs.el` (`mevedel-request-begin`,
  `mevedel-request-push-canceller`); `mevedel-agent-runtime.el`
- **What's owed:** While a top-level or sub-agent request is active, hold an OS
  sleep inhibitor and release it on every completion, failure, abort, and stale
  request replacement path.  On Linux, start `systemd-inhibit --what=sleep` as
  an asynchronous child process and register its teardown as a request
  canceller.  Keep screen blanking and locking unaffected; add other platform
  mechanisms only when they are needed.
- **Why deferred:** Emacs 30 has no portable system-sleep inhibitor, and a
  leaked platform inhibitor could prevent intended suspend indefinitely.
- **Status check:** Request ownership and teardown are already centralized, so
  each request can own its inhibitor without a new session-level reference
  counter.  No inhibitor is currently acquired.
- **Blast radius:** Without this, automatic suspend interrupts long-running
  model, tool, and agent work.  Incorrect cleanup can drain a laptop battery or
  block explicit suspend after mevedel becomes idle.

## Compaction

### Size-reducing retry for oversized compaction requests

- **Source:** `mevedel-compact.el`; `docs/compaction.md`
- **What's owed:** When a compaction request exceeds the provider context,
  retry with a smaller source body and fail clearly if the reduced request
  still does not fit.
- **Why deferred:** Tool outputs are now capped before summarization, which
  handles the common oversized-input case; arbitrary large non-tool content
  still needs a bounded fallback policy.
- **Status check:** Compaction has transport retries and body/tail tool-output
  caps, but retries resend the same source body.
- **Blast radius:** A transcript dominated by very large user or assistant
  prose can still make compaction fail at the API boundary.

### Sub-agent auto-compaction

- **Source:** `mevedel-compact.el`; `docs/compaction.md`
- **What's owed:** Give sub-agent transcripts an independent compaction and
  rotation path.
- **Why deferred:** The compaction writer rotates the parent session's current
  segment; an agent transcript is persisted separately and is not a session
  segment.
- **Status check:** Continuation compaction is wired into agent FSMs, but the
  eligibility check rejects agent buffers because they are not the active
  parent segment.
- **Blast radius:** A sufficiently long sub-agent run can exhaust its context
  even when the parent session remains below its threshold.

## Tools

### Bedrock backend support for deferred tool loading

- **Source:** `mevedel-tools.el` (`mevedel-tools--handle-deferred-inject`)
- **What's owed:** Read and replace Bedrock tools under
  `(:toolConfig :tools)` when deferred tools are injected or expired.
- **Why deferred:** Bedrock uses a different payload nesting from the other
  supported gptel backends and has not been exercised here.
- **Status check:** The handler explicitly supports only the common `:tools`
  path.
- **Blast radius:** Bedrock sessions cannot use deferred-tool loading
  correctly.
